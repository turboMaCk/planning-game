{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Game
  ( Vote(..)
  , GameError(..)
  , RunningGame(..)
  , FinishedGame
  , Games(..)
  , playerVotes
  , start
  , addVote
  , nextRound
  , autoNextName
  , complete
  , sumPoints
  , allVotes
  , currentPlayerVotes
  , isFinished
  , finishCurrent
  , allVoted
  , restartCurrent
  , removePlayerVotes
  , renameCurrent
  ) where

import           Control.Monad                   (mzero)
import           Data.Aeson.Types                (FromJSON (..), ToJSON (..),
                                                  Value (..))
import           Data.Map.Strict                 (Map)
import           Data.Maybe                      (mapMaybe)
import           Data.Text                       (Text)

import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text

import           PlanningGame.Api.Error          (Error (..), ErrorType (..))
import           PlanningGame.Data.Player        (Players, PlayerStatus(Active))
import           PlanningGame.Data.Session       (Session)

import qualified PlanningGame.Data.AutoIncrement as Inc
import qualified PlanningGame.Data.Player        as Player


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
  RunningGame Text (Map Session Vote) Bool


data FinishedGame =
  FinishedGame
    { votes       :: Map Session Vote
    , winningVote :: Vote
    , name        :: Text
    }


-- Isomorphic to ( [ PatGame ], Maybe RunningGame ) but more explicit.
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


instance Error GameError where
  toType GameFinished   = Forbidden
  toType VotingEndedErr = Forbidden

  toReadable GameFinished   = "Game is already finished."
  toReadable VotingEndedErr = "Votting is already closed."


getName :: Text -> Games -> Text
getName name' games
  | Text.null name' =
      checkName $ "Task-" <> Text.pack (show $ Set.size gameNames + 1)
  | otherwise = checkName name'

  where
    gameNames =
      case games of
        RunningGames finished (RunningGame n _ _) ->
          Set.fromList $ n : (name <$> finished)

        FinishedGames finished ->
          Set.fromList $ name <$> finished

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


finish :: Vote -> RunningGame -> FinishedGame
finish vote (RunningGame n v _) =
  FinishedGame
    { name = n
    , votes = v
    , winningVote = vote
    }


start :: Text -> Games
start name =
  RunningGames [] $ newGame (getName name $ FinishedGames [])


addVote :: Session -> Vote -> Games -> Either GameError Games
addVote _ _ (FinishedGames _)                                          = Left GameFinished
addVote sId vote (RunningGames past (RunningGame name votes isLocked)) =
  if isLocked then
    Left VotingEndedErr
  else
    Right $ RunningGames past $ RunningGame name updatedVotes False

  where
    updatedVotes :: Map Session Vote
    updatedVotes =
      Map.insert sId vote votes


nextRound :: Vote -> Text -> Games -> Either GameError Games
nextRound _ _ (FinishedGames _)                          = Left GameFinished
nextRound vote newName games@(RunningGames past current) =
  Right $ RunningGames (finish vote current : past)
    $ newGame (getName newName games)


complete :: Vote -> Games -> Either GameError Games
complete _ (FinishedGames _)              = Left GameFinished
complete vote (RunningGames past current) =
  Right $ FinishedGames (finish vote current : past)


-- @TODO: fails silently on finished games
finishCurrent :: Games -> Games
finishCurrent (FinishedGames past) = FinishedGames past
finishCurrent (RunningGames past (RunningGame name votes _)) =
  RunningGames past $ RunningGame name votes True


-- @TODO: fails silently on finished games
restartCurrent :: Games -> Games
restartCurrent (FinishedGames past) = FinishedGames past
restartCurrent (RunningGames past (RunningGame name _ _)) =
  RunningGames past $ RunningGame name Map.empty False


isFinished :: Games -> Bool
isFinished (FinishedGames _)                       = True
isFinished (RunningGames _ (RunningGame _ _ bool)) = bool


finishedGames :: Games -> [ FinishedGame ]
finishedGames (RunningGames finished _) = finished
finishedGames (FinishedGames finished)  = finished


sumPoints :: Games -> Int
sumPoints games =
  sum $ voteToInt . winningVote <$> finishedGames games


allVotes :: Games -> [ ( Text, Vote ) ]
allVotes games =
  reverse $ (\FinishedGame { name, winningVote } -> ( name, winningVote ))
  <$> finishedGames games


playerVotes :: Players -> Games -> [ ( Text, [ ( Int, Vote ) ] ) ]
playerVotes players games =
  reverse $ (\game@FinishedGame { name } -> ( name, playerVotes' game ))
  <$> finishedGames games

  where
    playerVotes' :: FinishedGame -> [ ( Int, Vote ) ]
    playerVotes' game =
      fmap (\(sId, vote) ->
              let player = Player.lookup sId players
              in
              -- @TODO: Better errr handling
              ( maybe 0 Inc.unwrapId player
              , vote
              )
           ) $ Map.toList $ votes game

-- @TODO: refactor `playerVotes` vs `playersVotes`
currentPlayerVotes :: Players -> Games -> [ ( Int, Vote ) ]
currentPlayerVotes _ (FinishedGames _) = []
currentPlayerVotes players (RunningGames _ (RunningGame _ votes _)) =
  mapMaybe (\(id', vote) -> (\p -> ( Inc.unwrapId p, vote )) <$> Player.lookup id' players)
    $ Map.toList votes


allVoted :: Players -> Games -> Bool
allVoted _ (FinishedGames _) = False
allVoted players (RunningGames _ (RunningGame _ votes _ )) =
  all (`Map.member` votes) sessionIds

  where
    sessionIds =
      fst <$> Prelude.filter (\(_, p) -> Player.status p == Active) (Player.toList players)


removePlayerVotes :: Session -> Games -> Games
removePlayerVotes sesId games' =
  case games' of
    FinishedGames games ->
      FinishedGames $ finished games

    RunningGames finished' (RunningGame text votes locked) ->
      RunningGames (finished finished') $ RunningGame text (filterVotes votes) locked

  where
    filterVotes =
      Map.filterWithKey (\k _ -> sesId /= k)

    finished games =
      map (\g -> g { votes = filterVotes $ votes g }) games

renameCurrent :: Text -> Games -> Games
renameCurrent _ g@(FinishedGames _) = g
renameCurrent newName' original@(RunningGames finished' (RunningGame _ votes locked)) =
  if Text.null newName then
    original
  else
    RunningGames finished' $ RunningGame newName votes locked

  where
    newName = Text.strip newName'
