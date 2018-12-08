{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}

module AgilePoker.Data.Game
  (Vote(..), RunningGame, FinishedGame, Games
  , startGame, addVote, nextRound, completeGame
  ) where

import Control.Monad (mzero)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson.Types as AT
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


instance ToJSON Vote where
  -- @TODO: some perf penalty for translating [ Char ] to Text
  toJSON = toJSON . T.pack . show


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


data RunningGame =
  RunningGame T.Text (Map.Map (Id SessionId) Vote)


instance ToJSON RunningGame where
  toJSON (RunningGame name votes) =
    -- @TODO: encode votes?
    AT.object
        [ "name" .= name
        ]


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
newGame name = RunningGame name Map.empty


finishGame :: Vote -> RunningGame -> FinishedGame
finishGame vote (RunningGame name votes ) =
  FinishedGame { gameName = name
               , gameVotes = votes
               , winningVote = vote
               }


startGame :: T.Text -> Games
startGame name =
  RunningGames [] $ newGame name


addVote :: Id SessionId -> Vote -> Games -> Either GameError Games
addVote _ _ (FinishedGames _)                        = Left GameFinished
addVote sId vote (RunningGames past (RunningGame name votes )) =
  Right $ RunningGames past $ RunningGame name updatedVotes
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
