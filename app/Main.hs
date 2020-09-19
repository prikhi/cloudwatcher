{-# LANGUAGE RecordWildCards #-}
module Main where

import           Brick                   hiding ( render )
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Control.Applicative
import           Control.Lens
import           Control.Monad                  ( void )
import           Data.Maybe                     ( fromMaybe )
import           Graphics.Vty.Attributes        ( defAttr
                                                , withStyle
                                                , reverseVideo
                                                )
import           Graphics.Vty.Input.Events      ( Event(..)
                                                , Key(..)
                                                )
import           Network.AWS.CloudWatchLogs

import           Lib

import qualified Data.List                     as L
import qualified Data.Vector                   as V


main :: IO ()
main = do
    st <-
        AppState . logGroupList . L.sortOn (^. lgLogGroupName) <$> getLogGroups
    void $ defaultMain app st

data AppState =
    AppState
        { lgList :: GenericList AppWidget V.Vector LogGroup
        }

initialState :: LogGroupList -> AppState
initialState = AppState

data AppWidget
    = LogGroupWidget
    deriving (Eq, Ord, Show)

data AppEvent

handleEvent
    :: AppState
    -> BrickEvent AppWidget AppEvent
    -> EventM AppWidget (Next AppState)
handleEvent s@AppState {..} = \case
    VtyEvent (EvKey (KChar 'q') _) -> halt s
    VtyEvent e                     -> do
        newLGList <- handleListEventVi handleListEvent e lgList
        continue s { lgList = newLGList }
    _ -> continue s

app :: App AppState AppEvent AppWidget
app = App
    { appDraw         = render
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
    , appAttrMap      = const $ attrMap
                            defAttr
                            [ ( listAttr <> listSelectedAttr
                              , defAttr `withStyle` reverseVideo
                              )
                            ]
    }

render :: AppState -> [Widget AppWidget]
render st = [body st]

body :: AppState -> Widget AppWidget
body st = hBox [renderLogGroupList $ lgList st, vBorder, str "El Oh Elm"]



type LogGroupList = GenericList AppWidget V.Vector LogGroup

logGroupList :: [LogGroup] -> LogGroupList
logGroupList lgs = list LogGroupWidget (V.fromList lgs) 1

renderLogGroupList :: LogGroupList -> Widget AppWidget
renderLogGroupList = renderList renderLogGroup True
  where
    renderLogGroup :: Bool -> LogGroup -> Widget AppWidget
    renderLogGroup isSelected lg =
        (if isSelected then withDefAttr (listAttr <> listSelectedAttr) else id)
            .   txt
            .   fromMaybe "<unnamed>"
            $   (lg ^. lgLogGroupName)
            <|> (lg ^. lgArn)
