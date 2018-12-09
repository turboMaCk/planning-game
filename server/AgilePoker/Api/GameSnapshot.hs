{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Api.GameSnapshot where

import AgilePoker.Data.Game
import Data.Maybe (mapMaybe)
import AgilePoker.Data.Player
import Data.Aeson.Types (ToJSON(..), Value(..), (.=), object)
import qualified Data.Map as Map
import qualified Data.Text as T


data GameSnapshot
  = RunningGameSnapshot { snapshotName :: T.Text
                        , snapshotVotes :: [ T.Text ]
                        , snapshotPoints :: Int
                        }
  | FinishedGameSnapshot { totalPoints :: Int
                         , userVotes :: [ ( T.Text, Vote ) ]
                         }


instance ToJSON GameSnapshot where
  toJSON snapshot =
    object
      [ "name"   .= snapshotName snapshot
      , "votes"  .= snapshotVotes snapshot
      , "points" .= snapshotPoints snapshot
      , "status" .= case snapshot of
                      RunningGameSnapshot _ _ _ ->
                        String "running"

                      FinishedGameSnapshot _ _ ->
                        String "finished"
      ]


snapshot :: Players -> Games -> GameSnapshot
snapshot players games@(RunningGames _ (RunningGame name votes)) =
  RunningGameSnapshot { snapshotName   = name
                      , snapshotVotes  = votes'
                      , snapshotPoints = sumGamePoints games
                      }
  where
    votes' :: [ T.Text ]
    votes' = mapMaybe (\(id, _) -> playerName <$> Map.lookup id players) $ Map.toList votes
snapshot players games@(FinishedGames finised) =
  FinishedGameSnapshot { totalPoints = sumGamePoints games
                       , userVotes   = gamesVotes games
                       }
