{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.GameSnapshot
  ( snapshot
  ) where

import           Data.Aeson.Types        (ToJSON (..), Value (..), object, (.=))
import qualified Data.Map                as Map
import qualified Data.Text               as T

import           AgilePoker.Data.Game
import           AgilePoker.Data.Id      (Id)
import           AgilePoker.Data.Player
import           AgilePoker.Data.Session (SessionId)


data GameSnapshot
  = RunningGameSnapshot  { snapshotName  :: T.Text
                         , snapshotVotes :: [ T.Text ]
                         , totalPoints   :: Int
                         }
  | LockedGameSnapshot   { snapshotName :: T.Text
                         , userVotes    :: [ ( T.Text, Vote ) ]
                         , totalPoints  :: Int
                         }
  | FinishedGameSnapshot { totalPoints       :: Int
                         , snapshotGameVotes :: [ ( T.Text, Vote ) ]
                         , playerGameVotes   :: [ ( T.Text, [ ( T.Text, Vote ) ] ) ]
                         }


-- @TODO: possible to generalize
pointsPair :: [ ( T.Text, Vote ) ] -> [ Value ]
pointsPair = fmap toVal
  where
    toVal ( name, points ) =
      object
        [ "name" .= name
        , "value" .= points
        ]


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
      , "userVotes" .= pointsPair votes
      , "points"    .= points
      , "status"    .= String "Locked"
      ]
  toJSON (FinishedGameSnapshot points votes playerVotes) =
    object
      [ "roundVotes" .= pointsPair votes
      , "points"     .= points
      , "status"     .= String "Finished"
      ]


snapshot :: ( Id SessionId, Player ) -> Players -> Games -> GameSnapshot
snapshot _ players games@(FinishedGames finised) =
  FinishedGameSnapshot { totalPoints       = sumGamePoints games
                       , snapshotGameVotes = gamesVotes games
                       , playerGameVotes   = []
                       }
snapshot banker players games@(RunningGames _ (RunningGame name votes isLocked)) =
  if isLocked then
    LockedGameSnapshot  { snapshotName = name
                        , userVotes    = playersVotes banker players games
                        , totalPoints  = sumGamePoints games
                        }
  else
    RunningGameSnapshot { snapshotName  = name
                        , snapshotVotes = fst <$> playersVotes banker players games
                        , totalPoints   = sumGamePoints games
                        }
