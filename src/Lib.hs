{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( getLogGroups
    , getLogStreams
    , Lib.getLogEvents
    , getEventContext
    )
where

import           Control.Lens
import           Data.Dynamic
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Time.Clock.POSIX
import           Amazonka
import           Amazonka.CloudWatchLogs
import           Amazonka.CloudWatchLogs.DescribeLogGroups
import           Amazonka.CloudWatchLogs.Lens
import           Numeric.Natural


getLogGroups :: IO [LogGroup]
getLogGroups = collectPaginatedResponses $ PaginationConfig
    (\t -> newDescribeLogGroups & describeLogGroups_nextToken .~ t)
    (^. describeLogGroupsResponse_logGroups . _Just)
    (^. describeLogGroupsResponse_nextToken)
    100

getLogStreams :: Text -> Int -> IO [LogStream]
getLogStreams lgName limit = collectPaginatedResponses $ PaginationConfig
    (\t ->
        newDescribeLogStreams lgName
            & (describeLogStreams_nextToken .~ t)
            & (describeLogStreams_orderBy ?~ OrderBy_LastEventTime)
            & (describeLogStreams_descending ?~ True)
    )
    (^. describeLogStreamsResponse_logStreams . _Just)
    (^. describeLogStreamsResponse_nextToken)
    limit

getLogEvents
    :: Text -> Natural -> (UTCTime, UTCTime) -> Text -> IO [FilteredLogEvent]
getLogEvents lgName limit (start, end) filterText =
    collectPaginatedResponses $ PaginationConfig
        (\t ->
            newFilterLogEvents lgName
                & (filterLogEvents_nextToken .~ t)
                & (filterLogEvents_filterPattern ?~ filterText)
                & (filterLogEvents_limit ?~ limit)
                & (filterLogEvents_startTime ?~ toTimestamp start)
                & (filterLogEvents_endTime ?~ toTimestamp end)
        )
        (^. filterLogEventsResponse_events . _Just)
        (^. filterLogEventsResponse_nextToken)
        (fromIntegral limit)
  where
    toTimestamp :: UTCTime -> Natural
    toTimestamp = (1000 *) . floor . utcTimeToPOSIXSeconds

getEventContext :: Text -> FilteredLogEvent -> Int -> IO [FilteredLogEvent]
getEventContext groupName event prevLines = case event ^. filteredLogEvent_timestamp of
    Just end ->
        let start = end - (3 * 1000)
        in
            reverse
            .   take (prevLines + 1)
            .   reverse
            <$> collectPaginatedResponses
                    (PaginationConfig
                        (\t ->
                            newFilterLogEvents groupName
                                & (filterLogEvents_nextToken .~ t)
                                & (filterLogEvents_limit ?~ 500)
                                & (filterLogEvents_logStreamNames .~ fmap
                                      (:| [])
                                      (event ^. filteredLogEvent_logStreamName)
                                  )
                                & (filterLogEvents_startTime ?~ start)
                                & (filterLogEvents_endTime ?~ end + 1)
                        )
                        (^. filterLogEventsResponse_events . _Just)
                        (^. filterLogEventsResponse_nextToken)
                        500
                    )
    Nothing -> return []



data PaginationConfig a req resp =
    PaginationConfig
        { makeReq :: Maybe Text -> req
        , getVals :: resp -> [a]
        , getToken :: resp -> Maybe Text
        , lengthLimit :: Int
        }

-- collectPaginatedResponses :: AWSRequest req => PaginationConfig a req (Rs req) -> IO [a]
collectPaginatedResponses :: (AWSRequest req, Typeable req, Typeable (AWSResponse req)) => PaginationConfig a req (AWSResponse req) -> IO [a]
collectPaginatedResponses cfg = go (Right ([], Nothing))
  where
    go = \case
        Left  result            -> return result
        Right (acc, mPageToken) -> do
            let req = makeReq cfg mPageToken
            env <- newEnv discover
            resp <- runResourceT $ send env req
            let vals = acc <> getVals cfg resp
            if Prelude.length vals >= lengthLimit cfg
                then return vals
                else case getToken cfg resp of
                    Nothing   -> return vals
                    nextToken -> go $ Right (vals, nextToken)
