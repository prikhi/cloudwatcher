{-# LANGUAGE DeriveDataTypeable #-}
{- | CLI application harness.
-}
module Lib.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Brick                          ( customMain )
import           Brick.BChan                    ( BChan
                                                , newBChan
                                                )
import           Control.Concurrent.Async       ( withAsync )
import           Control.Concurrent.STM         ( TQueue
                                                , atomically
                                                , readTQueue
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Data.Version                   ( showVersion )
import           Graphics.Vty                   ( mkVty
                                                , standardIOConfig
                                                )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , args
                                                , cmdArgs
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )

import           Lib.TUI                        ( AppEvent
                                                , AppState(..)
                                                , app
                                                , initialState
                                                )
import           Lib.Worker                     ( WorkerEvent
                                                , handleWorkerEvent
                                                )
import           Paths_cloudwatcher             ( version )


-- | Start the Async Worker & Launch the TUI
run :: Args -> IO ()
run Args { lookback } = do
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
    -- Read & execute events sent to the Worker Queue
    startWorker
        :: BChan AppEvent
        -- ^ brick event queue to push to
        -> TQueue WorkerEvent
        -- ^ worker queue to pull from
        -> IO ()
    startWorker bQueue wQueue = forever $ do
        ev <- atomically $ readTQueue wQueue
        handleWorkerEvent lookback bQueue ev


-- ARGS

-- | CLI arguments supported by the executable.
data Args = Args
    { lookback :: Integer
    }
    deriving (Show, Read, Eq, Data, Typeable)

-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec

-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args { lookback = 15 &= args &= typ "LOOKBACK" }
        &= summary
               ("cloudwatcher v" <> showVersion version <> ", Pavan Rikhi 2021")
        &= program "cloudwatcher"
        &= helpArg [name "h"]
        &= help "Review Error Logs From AWS Cloudwatch"
