{-# LANGUAGE RecordWildCards #-}
module Main where

import           Brick                   hiding ( render )
import           Brick.BChan
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Control.Applicative
import           Control.Concurrent.Async       ( withAsync )
import           Control.Concurrent.STM         ( TQueue
                                                , writeTQueue
                                                , atomically
                                                , newTQueueIO
                                                , readTQueue
                                                )
import           Control.Lens
import           Control.Monad                  ( void
                                                , forever
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bitraversable             ( bisequence )
import           Data.List.NonEmpty             ( nonEmpty )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time
import           Graphics.Vty.Attributes        ( defAttr
                                                , withStyle
                                                , reverseVideo
                                                , withForeColor
                                                , cyan
                                                , white
                                                )
import           Graphics.Vty                   ( standardIOConfig
                                                , mkVty
                                                )
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key(..)
                                                )
import           Network.AWS.CloudWatchLogs
                                         hiding ( getLogEvents )

import           Lib

import qualified Data.List                     as L
import qualified Data.Ord                      as O
import qualified Data.Text                     as T
import qualified Data.Vector                   as V


main :: IO ()
main = do
    st         <- initialState
    brickQueue <- newBChan 100
    withAsync (startWorker brickQueue $ asyncQueue st) $ \_ -> do
        vty <- standardIOConfig >>= mkVty
        void $ customMain vty
                          (standardIOConfig >>= mkVty)
                          (Just brickQueue)
                          app
                          st
  where
    startWorker :: BChan AppEvent -> TQueue QueueEvent -> IO ()
    startWorker bQueue wQueue = forever $ do
        ev <- atomically $ readTQueue wQueue
        handleWorker bQueue ev

data AppEvent
    = SetLogStreams [LogStream]
    | SetLogEvents [FilteredLogEvent]
    | SetEventContext [FilteredLogEvent]

data QueueEvent
    = GetLogStreams LogGroup
    | GetLogEvents LogGroup
    | GetEventContext LogGroup FilteredLogEvent

data AppState =
    AppState
        { lgList :: LogGroupList
        , lsList :: Maybe LogStreamList
        , leList :: Maybe LogEventList
        , ecList :: Maybe LogEventList
        , focusedPane :: AppWidget
        , asyncQueue :: TQueue QueueEvent
        }

initialState :: IO AppState
initialState = do
    lgl <- logGroupList . L.sortOn (^. lgLogGroupName) <$> getLogGroups
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
    | LogEventWidget
    | EventContextWidget
    deriving (Eq, Ord, Show)

handleEvent
    :: AppState
    -> BrickEvent AppWidget AppEvent
    -> EventM AppWidget (Next AppState)
handleEvent s@AppState {..} = \case
    VtyEvent (EvKey (KChar 'q') _) -> halt s
    VtyEvent (EvKey KEnter      _) -> chooseItem
    VtyEvent (EvKey KEsc        _) -> goBack
    VtyEvent e                     -> case (focusedPane, lsList) of
        (LogGroupWidget, _) -> do
            newLGList <- handleListEventVi handleListEvent e lgList
            continue s { lgList = newLGList }
        (LogStreamWidget, Just lsList_) -> do
            newLSList <- handleListEventVi handleListEvent e lsList_
            continue s { lsList = Just newLSList }
        (LogStreamWidget, _) -> continue s
        (LogEventWidget , _) -> case leList of
            Nothing      -> continue s
            Just leList_ -> do
                newLEList <- handleListEventVi handleListEvent e leList_
                continue s { leList = Just newLEList }
        (EventContextWidget, _) -> case ecList of
            Nothing      -> continue s
            Just ecList_ -> do
                newECList <- handleListEventVi handleListEvent e ecList_
                continue s { ecList = Just newECList }

    AppEvent e -> case e of
        SetLogStreams ls -> continue s { lsList = Just $ logStreamList ls }
        SetLogEvents  le -> continue s
            { leList = Just $ logEventList LogEventWidget $ L.sortOn
                           (O.Down . (^. fleTimestamp))
                           le
            }
        SetEventContext ec -> continue s
            { ecList = Just $ logEventList EventContextWidget $ L.sortOn
                           (O.Down . (^. fleTimestamp))
                           ec
            }
    _ -> continue s
  where
    chooseItem :: EventM AppWidget (Next AppState)
    chooseItem = case focusedPane of
        LogGroupWidget -> case listSelectedElement lgList of
            Nothing -> continue s
            Just (_, lg) ->
                liftIO (atomically $ writeTQueue asyncQueue (GetLogEvents lg))
                    >> continue s { focusedPane = LogEventWidget }
        LogEventWidget ->
            case
                    bisequence
                        ( leList >>= listSelectedElement
                        , listSelectedElement lgList
                        )
                of
                    Nothing -> continue s
                    Just ((_, le), (_, lg)) ->
                        liftIO
                                (atomically $ writeTQueue
                                    asyncQueue
                                    (GetEventContext lg le)
                                )
                            >> continue s { focusedPane = EventContextWidget }
        _ -> continue s
    goBack :: EventM AppWidget (Next AppState)
    goBack = case focusedPane of
        LogGroupWidget  -> continue s
        LogStreamWidget -> toLogGroupPane
        LogEventWidget  -> toLogGroupPane
        EventContextWidget ->
            continue s { ecList = Nothing, focusedPane = LogEventWidget }
    toLogGroupPane :: EventM AppWidget (Next AppState)
    toLogGroupPane = continue s { lsList      = Nothing
                                , leList      = Nothing
                                , focusedPane = LogGroupWidget
                                }

handleWorker :: BChan AppEvent -> QueueEvent -> IO ()
handleWorker brickQueue = \case
    GetLogStreams lg -> case lg ^. lgLogGroupName of
        Nothing -> return ()
        Just lgName ->
            getLogStreams lgName (100 :: Int)
                >>= writeBChan brickQueue
                .   SetLogStreams
    GetLogEvents lg -> case lg ^. lgLogGroupName of
        Nothing     -> return ()
        Just lgName -> do
            end <- getCurrentTime
            let start = addUTCTime (fromInteger $ -15 * 60) end
            getLogEvents lgName 100 (start, end) "\" 500 - \""
                >>= writeBChan brickQueue
                .   SetLogEvents
    GetEventContext lg fle -> case lg ^. lgLogGroupName of
        Nothing -> return ()
        Just lgName ->
            getEventContext lgName fle 15
                >>= writeBChan brickQueue
                .   SetEventContext

app :: App AppState AppEvent AppWidget
app = App
    { appDraw         = render
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
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
                    fmap (T.length . fromMaybe "" . (^. lgLogGroupName))
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
        txt . fromMaybe "<unnamed>" $ (lg ^. lgLogGroupName) <|> (lg ^. lgArn)


-- LOG STREAM

type LogStreamList = GenericList AppWidget V.Vector LogStream

logStreamList :: [LogStream] -> LogStreamList
logStreamList lss = list LogStreamWidget (V.fromList lss) 1

renderLogStreamList :: Bool -> LogStreamList -> Widget AppWidget
renderLogStreamList = renderList renderLogStream
  where
    renderLogStream :: Bool -> LogStream -> Widget AppWidget
    renderLogStream _ ls =
        txt . fromMaybe "<unnamed>" $ (ls ^. lsLogStreamName)


-- LOG EVENT

type LogEventList = GenericList AppWidget V.Vector FilteredLogEvent

logEventList :: AppWidget -> [FilteredLogEvent] -> LogEventList
logEventList w les = list w (V.fromList les) 1

renderLogEventList :: Bool -> LogEventList -> Widget AppWidget
renderLogEventList = renderList renderLogEvent
  where
    renderLogEvent :: Bool -> FilteredLogEvent -> Widget AppWidget
    renderLogEvent _ fle = txt . fromMaybe "<no-content>" $ fle ^. fleMessage
