module Lib.Worker
    ( WorkerEvent(..)
    , handleWorkerEvent
    ) where

import           Brick.BChan                    ( BChan
                                                , writeBChan
                                                )
import           Control.Lens                   ( (^.) )
import           Data.Time                      ( addUTCTime
                                                , getCurrentTime
                                                )
import           Amazonka.CloudWatchLogs.Types

import           Lib
import           Lib.Events


handleWorkerEvent :: Integer -> BChan AppEvent -> WorkerEvent -> IO ()
handleWorkerEvent logLookbackMin brickQueue = \case
    GetLogStreams lg -> case lg ^. logGroup_logGroupName of
        Nothing -> return ()
        Just lgName ->
            getLogStreams lgName (100 :: Int)
                >>= writeBChan brickQueue
                .   SetLogStreams
    GetLogEvents lg -> case lg ^. logGroup_logGroupName of
        Nothing     -> return ()
        Just lgName -> do
            end <- getCurrentTime
            let start = addUTCTime
                    (fromInteger $ (-1) * logLookbackMin * 60)
                    end
            getLogEvents lgName 100 (start, end) "\" 500 - \""
                >>= writeBChan brickQueue
                .   SetLogEvents
    GetEventContext lg fle -> case lg ^. logGroup_logGroupName of
        Nothing -> return ()
        Just lgName ->
            getEventContext lgName fle 15
                >>= writeBChan brickQueue
                .   SetEventContext
