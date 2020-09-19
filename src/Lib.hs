module Lib
    ( getLogGroups
    )
where

import           Control.Lens
import           Data.Text                      ( Text )
import           Network.AWS
import           Network.AWS.CloudWatchLogs


callAWS :: AWS a -> IO a
callAWS req = do
    env <- newEnv Discover
    runResourceT $ runAWS env req

getLogGroups :: IO [LogGroup]
getLogGroups = callAWS $ getPaginated (Right ([], Nothing))
  where
    getPaginated :: Either [LogGroup] ([LogGroup], Maybe Text) -> AWS [LogGroup]
    getPaginated = \case
        Left  result            -> return result
        Right (acc, mPageToken) -> do
            let req = case mPageToken of
                    Nothing -> describeLogGroups
                    Just pageToken ->
                        describeLogGroups & dlgNextToken ?~ pageToken

            resp <- send req
            let gs = acc <> resp ^. dlgrsLogGroups
            case resp ^. dlgrsNextToken of
                Nothing   -> return gs
                nextToken -> getPaginated $ Right (gs, nextToken)
