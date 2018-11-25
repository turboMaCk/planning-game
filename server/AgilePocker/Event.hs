{-# LANGUAGE OverloadedStrings #-}
module AgilePocker.Event
  ( Event
  , encodeEvent
  , userJoined
  , userLeft
  ) where

import Data.Maybe (isJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT

import AgilePocker.Session

data Event
    = UserJoined Session
    | UserLeft T.Text


instance ToJSON Event where
  toJSON (UserJoined (Session { sessionName=name, sessionConnection=conn })) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "name" .= name
        , "connected" .= isJust conn
        ]
  toJSON (UserLeft name) =
    AT.object
        [ "event" .= T.pack "UserLeft"
        , "name" .= name
        ]


userJoined :: Session -> Event
userJoined = UserJoined


userLeft :: Session -> Event
userLeft Session { sessionName=name } =
  UserLeft name


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
