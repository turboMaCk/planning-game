{-# LANGUAGE OverloadedStrings   #-}

module AgilePoker.Data.Game
  (Vote(..), RunningGame, FinishedGame, Games
  , startGame, addVote, nextRound, completeGame
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import AgilePoker.Data.Session
import AgilePoker.Data.Id


data Vote
  = OnePoint
  | TwoPoints
  | ThreePoints
  | FivePoints
  | EightPoints
  | ThreeteenPoints
  | TwentyPoints
  | FortyPoints
  | HunderedPoints
  | InfinityPoints
  | UnknownPoints


instance Show Vote where
  show OnePoint        = "1"
  show TwoPoints       = "2"
  show ThreePoints     = "3"
  show FivePoints      = "5"
  show EightPoints     = "8"
  show ThreeteenPoints = "13"
  show TwentyPoints    = "20"
  show FortyPoints     = "40"
  show HunderedPoints  = "100"
  show InfinityPoints  = "Infinity"
  show UnknownPoints   = "?"


type RunningGame =
  ( T.Text, Map.Map (Id SessionId) Vote )


data FinishedGame =
  FinishedGame { gameVotes :: Map.Map (Id SessionId) Vote
               , winningVote :: Vote
               , gameName :: T.Text
               }


-- Isomorphic to ( [ PatGame ], Maybe RunningGame ) but more explicit
-- Past games are stored in opossite order
data Games
  = RunningGames  [ FinishedGame ] RunningGame
  | FinishedGames [ FinishedGame ]


data GameError
  = GameFinished


newGame :: T.Text -> RunningGame
newGame name = ( name, Map.empty )


finishGame :: Vote -> RunningGame -> FinishedGame
finishGame vote ( name, votes ) =
  FinishedGame { gameName = name
               , gameVotes = votes
               , winningVote = vote
               }


startGame :: T.Text -> Games
startGame name =
  RunningGames [] $ newGame name


addVote :: Id SessionId -> Vote -> Games -> Either GameError Games
addVote _ _ (FinishedGames _)                        = Left GameFinished
addVote sId vote (RunningGames past ( name, votes )) =
  Right $ RunningGames past ( name, updatedVotes )
  where
    updatedVotes :: Map.Map (Id SessionId) Vote
    updatedVotes =
      Map.insert sId vote votes


nextRound :: Vote -> T.Text -> Games -> Either GameError Games
nextRound _ _ (FinishedGames _)                    = Left GameFinished
nextRound vote newName (RunningGames past current) =
  Right $ RunningGames (finishGame vote current : past) $ newGame newName


completeGame :: Vote -> Games -> Either GameError Games
completeGame _ (FinishedGames _)              = Left GameFinished
completeGame vote (RunningGames past current) =
  Right $ FinishedGames (finishGame vote current : past)
