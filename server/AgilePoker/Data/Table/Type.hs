{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Data.Table.Type
  (TableId, Table(..), Tables, TableError(..)
  , emptyTables
  ) where

import Data.Aeson.Types (ToJSON(..), (.=), object)
import Data.ByteString (ByteString)
import Control.Concurrent (MVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

import AgilePoker.Data.Id (Id)
import AgilePoker.Data.Player (Player, Players)
import AgilePoker.Data.Session (SessionId)


data TableId


-- @TODO: incomplete (missing games)
data Table = Table
  { tableId :: Id TableId
  , tableBanker :: ( Id SessionId, Player )
  , tablePlayers :: Players
  }


instance ToJSON Table where
  toJSON table =
    object
        [ "id" .= tableId table
        , "banker" .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        ]


type Tables =
  Map.Map (Id TableId) (MVar Table)


data TableError
  = TableNotFound
  | NameTaken
  | PlayerNotFound


emptyTables :: Tables
emptyTables = Map.empty
