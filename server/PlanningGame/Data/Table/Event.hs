{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Table.Event
  ( Event(..)
  , encodeEvent
  ) where

import           Data.Aeson.Types            (ToJSON (..), (.=), object)
import           Data.ByteString             (ByteString)

import qualified Data.ByteString.Lazy        as LazyByteString
import qualified Data.Aeson                  as Aeson
import qualified Data.Text                   as Text

import           PlanningGame.Api.GameSnapshot (snapshot)
import           PlanningGame.Data.Game        (Games)
import           PlanningGame.Data.Id          (Id)
import           PlanningGame.Data.Player      (Player, Players)
import           PlanningGame.Data.Session     (SessionId)

import           PlanningGame.Data.Table.Type  (Table)


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
    object
        [ "event"  .= Text.pack "PlayerJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    object
        [ "event"  .= Text.pack "PlayerStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    object
        [ "event" .= Text.pack "SyncTableState"
        , "table" .= table
        ]
  toJSON (GameStarted dealer players games) =
    object
        [ "event" .= Text.pack "GameStarted"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (VoteAccepted player) =
    object
        [ "event"  .= Text.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded dealer players games) =
    object
        [ "event" .= Text.pack "VotingEnded"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (GameEnded dealer players games) =
    object
        [ "event" .= Text.pack "GameEnded"
        , "game"  .= snapshot dealer players games
        ]


encodeEvent :: Event -> ByteString
encodeEvent =
  LazyByteString.toStrict . Aeson.encode
