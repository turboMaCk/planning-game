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

import AgilePoker.Data.Player (Player, Players)
import AgilePoker.Data.Session (SessionId)


-- @TODO: Just alias for now
type TableId = ByteString


-- @TODO: incomplete (missing games)
data Table = Table
  { tableId :: TableId
  , tableBanker :: ( SessionId, Player )
  , tablePlayers :: Players
  }


instance ToJSON Table where
  toJSON table =
    object
        [ "id" .= TE.decodeUtf8 (tableId table)
        , "banker" .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        ]


type Tables =
  Map.Map ByteString (MVar Table)


data TableError
  = TableNotFound
  | NameTaken
  | PlayerNotFound


emptyTables :: Tables
emptyTables = Map.empty
