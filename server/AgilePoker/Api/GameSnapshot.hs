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

{--
This can't be just and ToJSON instance of `Game` type
because correct resolution of snapshot requires
Players so sessionIds hold internally can be replaced
woth Player name's for the public API so private information
doesn't leak outside.
--}
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
      [ "name"        .= name
      , "playerVotes" .= pointsPair votes
      , "points"      .= points
      , "status"      .= String "Locked"
      ]
  toJSON (FinishedGameSnapshot points votes playerVotes) =
    object
      [ "roundVotes"  .= pointsPair votes
      , "playerVotes" .=
        fmap
          (\(task, xs) ->
             object ["name" .= task, "playerVotes" .= pointsPair xs])
          playerVotes
      , "points" .= points
      , "status" .= String "Finished"
      ]


-- @TODO: might need banker as well
snapshot :: ( Id SessionId, Player ) -> Players -> Games -> GameSnapshot
snapshot banker players games@(FinishedGames _) =
  FinishedGameSnapshot { totalPoints       = sumGamePoints games
                       , snapshotGameVotes = gamesVotes games
                       , playerGameVotes   = gamesPlayerVotes banker players games
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
