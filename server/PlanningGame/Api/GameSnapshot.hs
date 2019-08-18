{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.GameSnapshot
  ( snapshot
  ) where

import           Data.Aeson.Types          (ToJSON (..), Value (..), object,
                                            (.=))
import           Data.Text                 (Text)

import           PlanningGame.Data.Game    (Games (..), RunningGame (..),
                                            Vote (..))
import           PlanningGame.Data.Id      (Id)
import           PlanningGame.Data.Player  (Player, Players)
import           PlanningGame.Data.Session (SessionId)

import qualified PlanningGame.Data.Game    as Game

{--
This can't be just and ToJSON instance of `Game` type
because correct resolution of snapshot requires
Players so sessionIds hold internally can be replaced
woth Player name's for the public API so private information
doesn't leak outside.
--}
data GameSnapshot
  = RunningGameSnapshot
    { snapshotName  :: Text
    , snapshotVotes :: [ Int ]
    , totalPoints   :: Int
    }
  | LockedGameSnapshot
    { snapshotName :: Text
    , playerVotes  :: [ ( Int, Text, Vote ) ]
    , totalPoints  :: Int
    }
  | FinishedGameSnapshot
    { totalPoints       :: Int
    , snapshotGameVotes :: [ ( Text, Vote ) ]
    , playerGameVotes   :: [ ( Text, [ ( Int, Text, Vote ) ] ) ]
    }


-- @TODO: possible to generalize
taskPointsPair :: [ ( Text, Vote ) ] -> [ Value ]
taskPointsPair = fmap toVal
  where
    toVal ( name, points ) =
      object
        [ "name"  .= name
        , "value" .= points
        ]

userPointsPair :: [ ( Int, Text, Vote ) ] -> [ Value ]
userPointsPair = fmap toVal
  where
    toVal ( id', name, points ) =
      object
        [ "id"    .= id'
        , "name"  .= name
        , "value" .= points
        ]


instance ToJSON GameSnapshot where
  toJSON (RunningGameSnapshot name votes points) =
    object
      [ "name"         .= name
      , "maskedVotes"  .= votes
      , "points"       .= points
      , "status"       .= String "Running"
      ]
  toJSON (LockedGameSnapshot name votes points) =
    object
      [ "name"         .= name
      , "playerVotes"  .= userPointsPair votes
      , "points"       .= points
      , "status"       .= String "Locked"
      ]
  toJSON (FinishedGameSnapshot points votes pVotes) =
    object
      [ "roundVotes"  .= taskPointsPair votes
      , "playerVotes" .=
        fmap
          (\(task, xs) ->
             object [ "name" .= task
                    , "playerVotes" .= userPointsPair xs
                    ])
          pVotes
      , "points" .= points
      , "status" .= String "Finished"
      ]


-- @TODO: might need banker as well
snapshot :: ( Id SessionId, Player ) -> Players -> Games -> GameSnapshot
snapshot banker players games@(FinishedGames _) =
  FinishedGameSnapshot
    { totalPoints       = Game.sumPoints games
    , snapshotGameVotes = Game.allVotes games
    , playerGameVotes   = Game.playerVotes banker players games
    }
snapshot banker players games@(RunningGames _ (RunningGame name _ isLocked)) =
  if isLocked then
    LockedGameSnapshot
        { snapshotName = name
        , playerVotes  = Game.currentPlayerVotes banker players games
        , totalPoints  = Game.sumPoints games
        }
  else
    RunningGameSnapshot
        { snapshotName  = name
        , snapshotVotes = (\(id', _, _) -> id') <$> Game.currentPlayerVotes banker players games
        , totalPoints   = Game.sumPoints games
        }
