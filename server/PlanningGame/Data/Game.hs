{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Game
  ( Vote(..)
  , GameError(..)
  , RunningGame(..)
  , FinishedGame
  , Games(..)
  , gamesPlayerVotes
  , startGame
  , addVote
  , nextRound
  , autoNextName
  , completeGame
  , sumGamePoints
  , gamesVotes
  , playersVotes
  , isFinished
  , finishCurrentGame
  , allVoted
  ) where

import           Control.Monad           (mzero)
import           Data.Aeson.Types        (FromJSON (..), ToJSON (..),
                                          Value (..), (.=))
import           Data.Map.Strict         (Map)
import           Data.Maybe              (mapMaybe)
import           Data.Text               (Text)

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import qualified Data.Set as Set

import           PlanningGame.Data.Id
import           PlanningGame.Data.Player
import           PlanningGame.Data.Session


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


instance ToJSON Vote where
  -- @TODO: some perf penalty for translating [ Char ] to Text
  toJSON = toJSON . Text.pack . show


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


instance FromJSON Vote where
  parseJSON (String str) =
    case str of
      "1"        -> pure OnePoint
      "2"        -> pure TwoPoints
      "3"        -> pure ThreePoints
      "5"        -> pure FivePoints
      "8"        -> pure EightPoints
      "13"       -> pure ThreeteenPoints
      "20"       -> pure TwentyPoints
      "40"       -> pure FortyPoints
      "100"      -> pure HunderedPoints
      "Infinity" -> pure InfinityPoints
      _          -> mzero
  parseJSON _     = mzero


-- @TODO: named members
data RunningGame =
  RunningGame Text (Map (Id SessionId) Vote) Bool


data FinishedGame =
  FinishedGame
    { gameVotes   :: Map (Id SessionId) Vote
    , winningVote :: Vote
    , gameName    :: Text
    }


-- Isomorphic to ( [ PatGame ], Maybe RunningGame ) but more explicit
-- Past games are stored in opossite order
data Games
  = RunningGames  [ FinishedGame ] RunningGame
  | FinishedGames [ FinishedGame ]


data GameError
  = GameFinished
  | VotingEndedErr
  deriving (Eq)


instance Show GameError where
  show GameFinished   = "GameFinished"
  show VotingEndedErr = "VotingEnded"


getName :: Text -> Games -> Text
getName name games
  | Text.null name =
      checkName $ "Task-" <> (Text.pack $ show (Set.size gameNames + 1))
  | otherwise = checkName name

  where
    gameNames =
      case games of
        RunningGames finished (RunningGame name _ _) ->
          Set.fromList $ name : (gameName <$> finished)

        FinishedGames finished ->
          Set.fromList $ gameName <$> finished

    checkName txt =
      if Set.member txt gameNames then
        checkName $ txt <> ".1"
      else
        txt


autoNextName :: Games -> Text
autoNextName =
  getName ""


newGame :: Text -> RunningGame
newGame name =
  RunningGame name Map.empty False


finishGame :: Vote -> RunningGame -> FinishedGame
finishGame vote (RunningGame name votes _) =
  FinishedGame
    { gameName = name
    , gameVotes = votes
    , winningVote = vote
    }


startGame :: Text -> Games
startGame name =
  RunningGames [] $ newGame (getName name $ FinishedGames [])


addVote :: Id SessionId -> Vote -> Games -> Either GameError Games
addVote _ _ (FinishedGames _)                                          = Left GameFinished
addVote sId vote (RunningGames past (RunningGame name votes isLocked)) =
  if isLocked then
    Left VotingEndedErr
  else
    Right $ RunningGames past $ RunningGame name updatedVotes False

  where
    updatedVotes :: Map (Id SessionId) Vote
    updatedVotes =
      Map.insert sId vote votes


nextRound :: Vote -> Text -> Games -> Either GameError Games
nextRound _ _ (FinishedGames _)                          = Left GameFinished
nextRound vote newName games@(RunningGames past current) =
  Right $ RunningGames (finishGame vote current : past)
    $ newGame (getName newName games)


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
isFinished (FinishedGames _)                       = True
isFinished (RunningGames _ (RunningGame _ _ bool)) = bool


finishedGames :: Games -> [ FinishedGame ]
finishedGames (RunningGames finished _) = finished
finishedGames (FinishedGames finished)  = finished


sumGamePoints :: Games -> Int
sumGamePoints games =
  sum $ (voteToInt . winningVote) <$> finishedGames games


gamesVotes :: Games -> [ ( Text, Vote ) ]
gamesVotes games =
  reverse $ (\g -> ( gameName g, winningVote g )) <$> finishedGames games


gamesPlayerVotes :: ( Id SessionId, Player ) -> Players -> Games -> [ ( Text, [ ( Text, Vote ) ] ) ]
gamesPlayerVotes ( bankerId, bankerName ) players' games =
  reverse $ (\g -> ( gameName g, playerVotes g )) <$> finishedGames games

  where
    playerVotes :: FinishedGame -> [ ( Text, Vote ) ]
    playerVotes game =
      -- @TODO: Better errr handling
      fmap (\(sId, vote) -> (maybe "" playerName $ Map.lookup sId players, vote)) $ Map.toList $ gameVotes game

    players =
      Map.insert bankerId bankerName players'


playersVotes :: ( Id SessionId, Player ) -> Players -> Games -> [ ( Text, Vote ) ]
playersVotes _ _ (FinishedGames _) = []
playersVotes ( bankerId, banker ) players (RunningGames _ (RunningGame _ votes _)) =
  mapMaybe (\(id, vote) -> (\p -> ( playerName p, vote )) <$> Map.lookup id allPlayers)
    $ Map.toList votes

  where
    allPlayers = Map.insert bankerId banker players


allVoted :: Players -> Games -> Bool
allVoted _ (FinishedGames _) = False
allVoted players (RunningGames _ (RunningGame _ votes _ )) =
  Map.null $ Map.filterWithKey (\sId _ -> not $ Map.member sId votes) players
