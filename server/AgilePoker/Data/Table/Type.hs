{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Table.Type
  ( TableId
  , Table(..)
  , Tables
  , TableError(..)
  , emptyTables
  ) where

import           Control.Concurrent          (MVar)
import           Data.Aeson.Types            (ToJSON (..), Value (..), object,
                                              (.=))
import           Data.ByteString             (ByteString)
import           Data.Map                    (Map)

import qualified Data.Map.Strict             as Map

import           AgilePoker.Api.GameSnapshot (snapshot)
import           AgilePoker.Data.Game        (GameError (..), Games)
import           AgilePoker.Data.Id          (Id)
import           AgilePoker.Data.Player      (Player, PlayerError (..), Players)
import           AgilePoker.Data.Session     (SessionId)


data TableId


data Table = Table
  { tableId      :: Id TableId
  , tableBanker  :: ( Id SessionId, Player )
  , tablePlayers :: Players
  , tableGame    :: Maybe Games
  }


instance ToJSON Table where
  toJSON table =
    object
        [ "id" .= tableId table
        , "banker" .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        , "game" .= case tableGame table of
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


emptyTables :: Tables
emptyTables =
  Map.empty
