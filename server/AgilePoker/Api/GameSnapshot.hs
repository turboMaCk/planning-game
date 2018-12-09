{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Api.GameSnapshot where

import AgilePoker.Data.Game
import AgilePoker.Data.Player
import Data.Aeson.Types (ToJSON(..), Value(..), (.=), object)
import qualified Data.Map as Map
import qualified Data.Text as T


data GameSnapshot
  = RunningGameSnapshot  { snapshotName         :: T.Text
                         , snapshotVotes        :: [ T.Text ]
                         , totalPoints          :: Int
                         }
  | LockedGameSnapshot   { snapshotName         :: T.Text
                         , userVotes            :: [ ( T.Text, Vote ) ]
                         , totalPoints          :: Int
                         }
  | FinishedGameSnapshot { totalPoints          :: Int
                         , snapshotGamesVotes   :: [ ( T.Text, Vote ) ]
                         }


instance ToJSON GameSnapshot where
  toJSON (RunningGameSnapshot name votes points) =
    object
      [ "name"        .= name
      , "maskedVotes" .= votes
      , "points"      .= points
      , "status"      .= String "Running"
      ]
  toJSON (LockedGameSnapshot name votes points) =
    object
      [ "name"      .= name
      , "userVotes" .= votes
      , "points"    .= points
      , "status"    .= String "Locked"
      ]
  toJSON (FinishedGameSnapshot points votes) =
    object
      [ "roundVotes" .= votes
      , "points"     .= points
      , "status"     .= String "Finished"
      ]


snapshot :: Players -> Games -> GameSnapshot
snapshot players games@(FinishedGames finised) =
  FinishedGameSnapshot { totalPoints          = sumGamePoints games
                       , snapshotGamesVotes   = gamesVotes games
                       }
snapshot players games@(RunningGames _ (RunningGame name votes isLocked)) =
  if isLocked then
    LockedGameSnapshot  { snapshotName   = name
                        , userVotes      = playersVotes players games
                        , totalPoints    = sumGamePoints games
                        }
  else
    RunningGameSnapshot { snapshotName   = name
                        , snapshotVotes  = fst <$> playersVotes players games
                        , totalPoints    = sumGamePoints games
                        }
