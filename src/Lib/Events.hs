module Lib.Events
    ( AppEvent(..)
    , WorkerEvent(..)
    ) where

import           Network.AWS.CloudWatchLogs     ( FilteredLogEvent
                                                , LogGroup
                                                , LogStream
                                                )

-- | Events for the worker to respond to.
data WorkerEvent
    = GetLogStreams LogGroup
    -- ^ Get all streams for the log group.
    | GetLogEvents LogGroup
    -- ^ Get the filtered & limited loglines for the log group.
    | GetEventContext LogGroup FilteredLogEvent
    -- ^ Get the surrounding context for the given log event.
    deriving (Show, Read)

-- | Events for the app to respond to.
data AppEvent
    = SetLogStreams [LogStream]
    -- ^ Set the (unused) list of LogStreams
    | SetLogEvents [FilteredLogEvent]
    -- ^ Set the list of search results
    | SetEventContext [FilteredLogEvent]
    -- ^ Set the log line's context
