module Lib
    ( getLogGroups
    , getLogStreams
    , Lib.getLogEvents
    , getEventContext
    )
where

import           Control.Lens
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.AWS
import           Network.AWS.CloudWatchLogs
import           Numeric.Natural


callAWS :: AWS a -> IO a
callAWS req = do
    env <- newEnv Discover
    runResourceT $ runAWS env req

getLogGroups :: IO [LogGroup]
getLogGroups = collectPaginatedResponses $ PaginationConfig
    (\t -> describeLogGroups & dlgNextToken .~ t)
    (^. dlgrsLogGroups)
    (^. dlgrsNextToken)
    100

getLogStreams :: Text -> Int -> IO [LogStream]
getLogStreams lgName limit = collectPaginatedResponses $ PaginationConfig
    (\t ->
        describeLogStreams lgName
            & (dlssNextToken .~ t)
            & (dlssOrderBy ?~ LastEventTime)
            & (dlssDescending ?~ True)
    )
    (^. dlsrsLogStreams)
    (^. dlsrsNextToken)
    limit

getLogEvents
    :: Text -> Natural -> (UTCTime, UTCTime) -> Text -> IO [FilteredLogEvent]
getLogEvents lgName limit (start, end) filterText =
    collectPaginatedResponses $ PaginationConfig
        (\t ->
            filterLogEvents lgName
                & (fleNextToken .~ t)
                & (fleFilterPattern ?~ filterText)
                & (fleLimit ?~ limit)
                & (fleStartTime ?~ toTimestamp start)
                & (fleEndTime ?~ toTimestamp end)
        )
        (^. flersEvents)
        (^. flersNextToken)
        (fromIntegral limit)
  where
    toTimestamp :: UTCTime -> Natural
    toTimestamp = (1000 *) . floor . utcTimeToPOSIXSeconds

getEventContext :: Text -> FilteredLogEvent -> Int -> IO [FilteredLogEvent]
getEventContext groupName event prevLines = case event ^. fleTimestamp of
    Just end ->
        let start = end - (3 * 1000)
        in
            reverse
            .   take (prevLines + 1)
            .   reverse
            <$> collectPaginatedResponses
                    (PaginationConfig
                        (\t ->
                            filterLogEvents groupName
                                & (fleNextToken .~ t)
                                & (fleLimit ?~ 500)
                                & (fleLogStreamNames .~ fmap
                                      (:| [])
                                      (event ^. fleLogStreamName)
                                  )
                                & (fleStartTime ?~ start)
                                & (fleEndTime ?~ end + 1)
                        )
                        (^. flersEvents)
                        (^. flersNextToken)
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

collectPaginatedResponses
    :: AWSRequest req => PaginationConfig a req (Rs req) -> IO [a]
collectPaginatedResponses cfg = callAWS $ go (Right ([], Nothing))
  where
    go = \case
        Left  result            -> return result
        Right (acc, mPageToken) -> do
            let req = makeReq cfg mPageToken
            resp <- send req
            let vals = acc <> getVals cfg resp
            if length vals >= lengthLimit cfg
                then return vals
                else case getToken cfg resp of
                    Nothing   -> return vals
                    nextToken -> go $ Right (vals, nextToken)
