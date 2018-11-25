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
  toJSON (UserJoined (Session { sessionName=name, sessionConnections=conns })) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "name" .= name
        , "connected" .= not (IntMap.null conns)
        ]
  toJSON (UserStatusUpdate (Session { sessionName=name, sessionConnections=conns })) =
    AT.object
        [ "event" .= T.pack "UserStatusUpdate"
        , "name" .= name
        , "connected" .= not (IntMap.null conns)
        ]


userJoined :: Session -> Event
userJoined = UserJoined


userStatusUpdate :: Session -> Event
userStatusUpdate = UserStatusUpdate


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
