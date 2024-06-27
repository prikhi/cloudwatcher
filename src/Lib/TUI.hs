{-# LANGUAGE RecordWildCards #-}
module Lib.TUI
    ( AppEvent(..)
    , AppState(..)
    , app
    , initialState
    ) where

import           Brick                   hiding ( render )
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent.STM         ( TQueue
                                                , atomically
                                                , newTQueueIO
                                                , writeTQueue
                                                )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bitraversable             ( bisequence )
import           Data.List.NonEmpty             ( nonEmpty )
import           Data.Maybe                     ( fromMaybe )
import           Graphics.Vty.Attributes        ( cyan
                                                , defAttr
                                                , reverseVideo
                                                , white
                                                , withForeColor
                                                , withStyle
                                                )
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key(..)
                                                )
import           Amazonka.CloudWatchLogs
                                         hiding ( getLogEvents )
import           Amazonka.CloudWatchLogs.Lens

import           Lib
import           Lib.Events

import qualified Data.List                     as L
import qualified Data.Ord                      as O
import qualified Data.Text                     as T
import qualified Data.Vector                   as V


app :: App AppState AppEvent AppWidget
app = App
    { appDraw         = render
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent  = handleEvent
    , appStartEvent   = pure ()
    , appAttrMap      = const $ attrMap
                            defAttr
                            [ ( listSelectedFocusedAttr
                              , defAttr
                              `withForeColor` white
                              `withStyle`     reverseVideo
                              )
                            , (listSelectedAttr, defAttr `withForeColor` cyan)
                            ]
    }


-- STATE

data AppState = AppState
    { lgList      :: LogGroupList
    , lsList      :: Maybe LogStreamList
    , leList      :: Maybe LogEventList
    , ecList      :: Maybe LogEventList
    , focusedPane :: AppWidget
    , asyncQueue  :: TQueue WorkerEvent
    }

initialState :: IO AppState
initialState = do
    lgl <- logGroupList . L.sortOn (^. logGroup_logGroupName) <$> getLogGroups
    q   <- newTQueueIO
    return $ AppState { lgList      = lgl
                      , lsList      = Nothing
                      , leList      = Nothing
                      , ecList      = Nothing
                      , focusedPane = LogGroupWidget
                      , asyncQueue  = q
                      }

data AppWidget
    = LogGroupWidget
    | LogStreamWidget
    -- ^ Unused
    | LogEventWidget
    | EventContextWidget
    deriving (Eq, Ord, Show)


-- UPDATE

handleEvent
    :: BrickEvent AppWidget AppEvent
    -> EventM AppWidget AppState ()
handleEvent be = do
 AppState {..} <- get
 case be of
    VtyEvent (EvKey (KChar 'q') _) -> halt
    VtyEvent (EvKey KEnter      _) -> chooseItem
    VtyEvent (EvKey KEsc        _) -> goBack
    VtyEvent e                     -> case (focusedPane, lsList) of
        (LogGroupWidget, _) -> do
            (newLGList, _) <- nestEventM lgList $ handleListEventVi handleListEvent e
            modify $ \s -> s { lgList = newLGList }
        (LogStreamWidget, Just lsList_) -> do
            (newLSList, _) <- nestEventM lsList_ $ handleListEventVi handleListEvent e
            modify $ \s ->  s { lsList = Just newLSList }
        (LogStreamWidget, _) -> pure ()
        (LogEventWidget , _) -> case leList of
            Nothing      -> pure ()
            Just leList_ -> do
              (newLEList, _) <- nestEventM leList_ $ handleListEventVi handleListEvent e
              modify $ \s -> s { leList = Just newLEList }
        (EventContextWidget, _) -> case ecList of
            Nothing      -> pure ()
            Just ecList_ -> do
              (newECList, _) <- nestEventM ecList_ $ handleListEventVi handleListEvent e
              modify $ \s -> s { ecList = Just newECList }

    AppEvent e -> case e of
        SetLogStreams ls -> modify $ \s -> s { lsList = Just $ logStreamList ls }
        SetLogEvents  le -> modify $ \s -> s
            { leList = Just $ logEventList LogEventWidget $ L.sortOn
                           (O.Down . (^. filteredLogEvent_timestamp))
                           le
            }
        SetEventContext ec -> modify $ \s -> s
            { ecList = Just $ logEventList EventContextWidget $ L.sortOn
                           (O.Down . (^. filteredLogEvent_timestamp))
                           ec
            }
    _ -> pure ()
  where
    chooseItem :: EventM AppWidget AppState ()
    chooseItem = do
      AppState {..} <- get
      case focusedPane of
        LogGroupWidget -> case listSelectedElement lgList of
            Nothing -> pure ()
            Just (_, lg) -> do
              liftIO (atomically $ writeTQueue asyncQueue (GetLogEvents lg))
              modify $ \s -> s { focusedPane = LogEventWidget }
        LogEventWidget ->
            case
                    bisequence
                        ( leList >>= listSelectedElement
                        , listSelectedElement lgList
                        )
                of
                    Nothing -> pure ()
                    Just ((_, le), (_, lg)) -> do
                        liftIO $ atomically $ writeTQueue asyncQueue (GetEventContext lg le)
                        modify $ \s -> s { focusedPane = EventContextWidget }
        _ -> pure ()
    goBack :: EventM AppWidget AppState ()
    goBack = do
      AppState {..} <- get
      case focusedPane of
         LogGroupWidget  -> pure ()
         LogStreamWidget -> toLogGroupPane
         LogEventWidget  -> toLogGroupPane
         EventContextWidget -> modify $ \s -> s { ecList = Nothing, focusedPane = LogEventWidget }
    toLogGroupPane :: EventM AppWidget AppState ()
    toLogGroupPane = modify $ \s -> s { lsList      = Nothing
                                      , leList      = Nothing
                                      , focusedPane = LogGroupWidget
                                      }


-- RENDER

render :: AppState -> [Widget AppWidget]
render st = [body st]

body :: AppState -> Widget AppWidget
body st =
    vBox
        $ hBox
              ( hLimit
                    maxLogNameLength
                    ( label "Log Groups"
                    $ renderLogGroupList (focused == LogGroupWidget)
                    $ lgList st
                    )
              : padLeftRight 1 vBorder
              : mainPanes
              )
        : contextPane

  where
    focused :: AppWidget
    focused = focusedPane st
    maxLogNameLength :: Int
    maxLogNameLength =
        let nameLengths =
                fmap (T.length . fromMaybe "" . (^. logGroup_logGroupName))
                    <$> nonEmpty (V.toList $ listElements $ lgList st)
        in  maybe 20 maximum nameLengths
    mainPanes :: [Widget AppWidget]
    mainPanes = case (focused, lsList st, leList st) of
        (LogGroupWidget, _, _) ->
            [label "Events" $ center $ txt "Select a Log Group"]
        (LogStreamWidget, Nothing, _) ->
            [label "Streams" $ center $ txt "Loading Log Streams"]
        (LogStreamWidget, Just ls, _) ->
            [ label "Streams" $ renderLogStreamList True ls
            , vBorder
            , center $ txt "Select a Log Stream"
            ]
        (LogEventWidget, _, Just le) ->
            [label "Events" $ renderLogEventList True le]
        (LogEventWidget, _, Nothing) ->
            [label "Events" $ center $ txt "Loading Log Events"]
        (EventContextWidget, _, Just le) ->
            [label "Events" $ renderLogEventList False le]
        (EventContextWidget, _, _) -> []
    contextPane = case (focused, leList st, ecList st) of
        (LogEventWidget, Just _, Nothing) ->
            [vLimit 16 $ label "Context" $ center $ txt "Select an Event"]
        (EventContextWidget, Just _, Nothing) ->
            [vLimit 16 $ label "Context" $ center $ txt "Loading Event Context"]
        (_, Just _, Just ec) ->
            [label "Context" $ vLimit 15 $ renderLogEventList True ec]
        _ -> []
    label l w = vBox [hBorderWithLabel $ padLeftRight 1 $ txt l, w]

-- LOG GROUP

type LogGroupList = GenericList AppWidget V.Vector LogGroup

logGroupList :: [LogGroup] -> LogGroupList
logGroupList lgs = list LogGroupWidget (V.fromList lgs) 1

renderLogGroupList :: Bool -> LogGroupList -> Widget AppWidget
renderLogGroupList = renderList renderLogGroup
  where
    renderLogGroup :: Bool -> LogGroup -> Widget AppWidget
    renderLogGroup _ lg =
        txt . fromMaybe "<unnamed>" $ (lg ^. logGroup_logGroupName) <|> (lg ^. logGroup_arn)

-- LOG STREAM

type LogStreamList = GenericList AppWidget V.Vector LogStream

logStreamList :: [LogStream] -> LogStreamList
logStreamList lss = list LogStreamWidget (V.fromList lss) 1

renderLogStreamList :: Bool -> LogStreamList -> Widget AppWidget
renderLogStreamList = renderList renderLogStream
  where
    renderLogStream :: Bool -> LogStream -> Widget AppWidget
    renderLogStream _ ls =
        txt . fromMaybe "<unnamed>" $ (ls ^. logStream_logStreamName)

-- LOG EVENT

type LogEventList = GenericList AppWidget V.Vector FilteredLogEvent

logEventList :: AppWidget -> [FilteredLogEvent] -> LogEventList
logEventList w les = list w (V.fromList les) 1

renderLogEventList :: Bool -> LogEventList -> Widget AppWidget
renderLogEventList = renderList renderLogEvent
  where
    renderLogEvent :: Bool -> FilteredLogEvent -> Widget AppWidget
    renderLogEvent _ fle = txt . fromMaybe "<no-content>" $ fle ^. filteredLogEvent_message
