{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}

module AgilePoker.Data.Game
  (Vote(..), GameError(..), RunningGame(..), FinishedGame, Games(..)
  , startGame, addVote, nextRound, completeGame, sumGamePoints
  , gamesVotes, playersVotes, isFinished, finishCurrentGame, allVoted
  ) where

-- import Data.List (notNull)
import Data.Maybe (mapMaybe)
import Control.Monad (mzero)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson.Types as AT
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import AgilePoker.Data.Session
import AgilePoker.Data.Player
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


instance ToJSON Vote where
  -- @TODO: some perf penalty for translating [ Char ] to Text
  toJSON = toJSON . T.pack . show


voteToInt :: Vote -> Int
voteToInt OnePoint        = 1
voteToInt TwoPoints       = 2
voteToInt ThreePoints     = 3
voteToInt FivePoints      = 5
voteToInt EightPoints     = 8
voteToInt ThreeteenPoints = 13
voteToInt TwentyPoints    = 20
voteToInt FortyPoints     = 40
voteToInt HunderedPoints  = 100
voteToInt InfinityPoints  = 0
voteToInt UnknownPoints   = 0


instance FromJSON Vote where
  parseJSON (AT.String str) =
    case str of
      "1"        -> pure OnePoint
      "2"        -> pure TwoPoints
      "3"        -> pure ThreePoints
      "5"        -> pure FivePoints
      "8"        -> pure EightPoints
      "13"       -> pure ThreePoints
      "20"       -> pure TwentyPoints
      "40"       -> pure FortyPoints
      "100"      -> pure HunderedPoints
      "Infinity" -> pure InfinityPoints
      "?"        -> pure UnknownPoints
      _          -> mzero
  parseJSON _     = mzero


-- @TODO: use hash
data RunningGame =
  RunningGame T.Text (Map.Map (Id SessionId) Vote) Bool


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
  | VotingEndedErr


newGame :: T.Text -> RunningGame
newGame name = RunningGame name Map.empty False


finishGame :: Vote -> RunningGame -> FinishedGame
finishGame vote (RunningGame name votes _) =
  FinishedGame { gameName = name
               , gameVotes = votes
               , winningVote = vote
               }


startGame :: T.Text -> Games
startGame name =
  RunningGames [] $ newGame name


addVote :: Id SessionId -> Vote -> Games -> Either GameError Games
addVote _ _ (FinishedGames _)                        = Left GameFinished
addVote sId vote (RunningGames past (RunningGame name votes isLocked)) =
  if isLocked then
    Left VotingEndedErr
  else
    Right $ RunningGames past $ RunningGame name updatedVotes False
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


-- @TODO: fails silently on finished games
finishCurrentGame :: Games -> Games
finishCurrentGame (FinishedGames past) = FinishedGames past
finishCurrentGame (RunningGames past (RunningGame name votes _)) =
  RunningGames past $ RunningGame name votes True


isFinished :: Games -> Bool
isFinished (FinishedGames _) = True
isFinished (RunningGames _ (RunningGame _ _ bool)) = bool


finishedGames :: Games -> [ FinishedGame ]
finishedGames (RunningGames finished _) = finished
finishedGames (FinishedGames finished) = finished


sumGamePoints :: Games -> Int
sumGamePoints games =
  sum $ (voteToInt . winningVote) <$> finishedGames games


gamesVotes :: Games -> [ ( T.Text, Vote ) ]
gamesVotes games =
  reverse $ (\g -> ( gameName g, winningVote g )) <$> finishedGames games


playersVotes :: ( Id SessionId, Player ) -> Players -> Games -> [ ( T.Text, Vote ) ]
playersVotes _ _ (FinishedGames _) = []
playersVotes ( bankerId, banker ) players (RunningGames _ (RunningGame _ votes _)) =
  mapMaybe (\(id, vote) -> (\p -> ( playerName p, vote )) <$> Map.lookup id allPlayers)
  $ Map.toList votes

  where
    allPlayers = Map.insert bankerId banker players


allVoted :: Players -> Games -> Bool
allVoted _ (FinishedGames _ ) = False
allVoted players (RunningGames _ (RunningGame _ votes _ )) =
  not $ Map.null $ Map.filterWithKey (\sId _ -> Map.member sId votes) players
