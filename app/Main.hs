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

data QueueEvent
    = GetLogStreams LogGroup
    | GetLogEvents LogGroup

data AppState =
    AppState
        { lgList :: LogGroupList
        , lsList :: Maybe LogStreamList
        , leList :: Maybe LogEventList
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
                      , focusedPane = LogGroupWidget
                      , asyncQueue  = q
                      }

data AppWidget
    = LogGroupWidget
    | LogStreamWidget
    | LogEventWidget
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
    AppEvent e -> case e of
        SetLogStreams ls -> continue s { lsList = Just $ logStreamList ls }
        SetLogEvents  le -> continue s
            { leList = Just $ logEventList $ L.sortOn
                           (O.Down . (^. fleTimestamp))
                           le
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
        _ -> continue s
    goBack :: EventM AppWidget (Next AppState)
    goBack = case focusedPane of
        LogGroupWidget  -> continue s
        LogStreamWidget -> toLogGroupPane
        LogEventWidget  -> toLogGroupPane
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
    hBox
        $  [ renderLogGroupList (focused == LogGroupWidget) $ lgList st
           , padLeftRight 1 vBorder
           ]
        <> streamPanes
  where
    focused :: AppWidget
    focused = focusedPane st
    streamPanes :: [Widget AppWidget]
    streamPanes = case (focused, lsList st, leList st) of
        (LogGroupWidget , _      , _) -> [center $ txt "Select a Log Group"]
        (LogStreamWidget, Nothing, _) -> [center $ txt "Loading Log Streams"]
        (LogStreamWidget, Just ls, _) ->
            [ renderLogStreamList True ls
            , vBorder
            , center $ txt "Select a Log Stream"
            ]
        (LogEventWidget, _, Just le) -> [renderLogEventList True le]
        (LogEventWidget, _, Nothing) -> [center $ txt "Loading Log Events"]



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

logEventList :: [FilteredLogEvent] -> LogEventList
logEventList les = list LogEventWidget (V.fromList les) 1

renderLogEventList :: Bool -> LogEventList -> Widget AppWidget
renderLogEventList = renderList renderLogEvent
  where
    renderLogEvent :: Bool -> FilteredLogEvent -> Widget AppWidget
    renderLogEvent _ fle = txt . fromMaybe "<no-content>" $ fle ^. fleMessage
