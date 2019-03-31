{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Table.Type
  ( TableId
  , Table(..)
  , Tables
  , TableError(..)
  , emptyTables
  ) where

import           Control.Concurrent          (MVar)
import           Data.Aeson.Types            (ToJSON (..), Value (..), object,
                                              (.=))
import           Data.Map                    (Map)

import qualified Data.Map.Strict             as Map

import           PlanningGame.Api.Error        (Error (..), ErrorType (..))
import           PlanningGame.Api.GameSnapshot (snapshot)
import           PlanningGame.Data.Game        (GameError (..), Games)
import           PlanningGame.Data.Id          (Id)
import           PlanningGame.Data.Player      (Player, PlayerError (..), Players)
import           PlanningGame.Data.Session     (SessionId)
import           Data.Time.Clock               (UTCTime)


data TableId


data Table = Table
  { tableId        :: Id TableId
  , tableBanker    :: ( Id SessionId, Player )
  , tablePlayers   :: Players
  , tableGame      :: Maybe Games
  , tableCreatedAt :: UTCTime
  }


instance ToJSON Table where
  toJSON table =
    object
        [ "id"      .= tableId table
        , "banker"  .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        , "game"    .=
          case tableGame table of
            Just game ->
              toJSON $ snapshot (tableBanker table) (tablePlayers table) game

            Nothing ->
              Null
        ]


type Tables =
  Map (Id TableId) (MVar Table)


data TableError
  = TableNotFound
  | PlayerNotFound
  | PlayerError PlayerError
  | GameError GameError
  deriving (Eq)


instance Show TableError where
  show TableNotFound   = "TableNotFound"
  show PlayerNotFound  = "PlayerNotFound"
  show (PlayerError e) = "PlayerError:" <> show e
  show (GameError e)   = "GameError:" <> show e


instance Error TableError where
  toType TableNotFound   = NotFound
  toType PlayerNotFound  = Forbidden
  toType (GameError e)   = toType e
  toType (PlayerError e) = toType e

  toReadable TableNotFound   = "Table doesn't exist."
  toReadable PlayerNotFound  = "You're not a player on this table."
  toReadable (GameError e)   = toReadable e
  toReadable (PlayerError e) = toReadable e


emptyTables :: Tables
emptyTables =
  Map.empty
