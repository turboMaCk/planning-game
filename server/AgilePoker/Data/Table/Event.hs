{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Table.Event
  ( Event(..)
  , encodeEvent
  ) where

import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            (ToJSON (..), (.=))
import qualified Data.Aeson.Types            as AT
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.IntMap                 as IntMap
import qualified Data.Text                   as T

import           AgilePoker.Api.GameSnapshot (snapshot)
import           AgilePoker.Data.Game        (Games)
import           AgilePoker.Data.Id          (Id)
import           AgilePoker.Data.Player      (Player, Players)
import           AgilePoker.Data.Session     (SessionId)

import           AgilePoker.Data.Table.Type  (Table)


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table
    | GameStarted ( Id SessionId, Player ) Players Games
    | VoteAccepted Player
    | VotingEnded ( Id SessionId, Player ) Players Games
    | GameEnded ( Id SessionId, Player ) Players Games


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    AT.object
        [ "event"  .= T.pack "UserJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    AT.object
        [ "event"  .= T.pack "UserStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    AT.object
        [ "event" .= T.pack "SyncTableState"
        , "table" .= table
        ]
  toJSON (GameStarted dealer players games) =
    AT.object
        [ "event" .= T.pack "GameStarted"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (VoteAccepted player) =
    AT.object
        [ "event"  .= T.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded dealer players games) =
    AT.object
        [ "event" .= T.pack "VotingEnded"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (GameEnded dealer players games) =
    AT.object
        [ "event" .= T.pack "GameEnded"
        , "game"  .= snapshot dealer players games
        ]


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
