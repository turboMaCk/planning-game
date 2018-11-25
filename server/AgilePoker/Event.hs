{-# LANGUAGE OverloadedStrings #-}
module AgilePoker.Event
  ( Event
  , encodeEvent
  , userJoined
  , userStatusUpdate
  ) where

import Data.ByteString (ByteString)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT

import AgilePoker.Session


data Event
    = UserJoined Session
    | UserStatusUpdate Session


instance ToJSON Event where
  toJSON (UserJoined session) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "user" .= toJSON session
        ]
  toJSON (UserStatusUpdate session) =
    AT.object
        [ "event" .= T.pack "UserStatusUpdate"
        , "user" .= toJSON session
        ]


userJoined :: Session -> Event
userJoined = UserJoined


userStatusUpdate :: Session -> Event
userStatusUpdate = UserStatusUpdate


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
