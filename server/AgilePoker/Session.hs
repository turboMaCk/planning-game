{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Session
  ( Session(..), SessionId, Sessions
  , emptySessions, addSession, getSession, removeSession
  , assignConnection, disconnectSession
  ) where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import qualified Data.Aeson.Types as AT


import AgilePoker.Id


data Session = Session
  { sessionId :: ByteString
  , sessionName :: T.Text
  , sessionConnections :: IntMap WS.Connection
  }


instance ToJSON Session where
  toJSON (Session { sessionName=name, sessionConnections=conns, sessionId=id }) =
    AT.object
        [ "id" .= TE.decodeUtf8 id
        , "name" .= name
        , "connected" .= not (IntMap.null conns)
        ]


-- @TODO: Just alias for now
type SessionId = ByteString


type Sessions =
  Map ByteString Session


addSession :: T.Text -> Sessions -> IO ( Sessions, ( SessionId, Session ) )
addSession name sessions = do
    newId <- generateId sessions
    let newSession = (Session newId name IntMap.empty)
    pure $ ( Map.insert newId newSession sessions
            , ( newId, newSession )
            )


assignConnection :: SessionId -> WS.Connection -> Sessions -> ( Sessions, Maybe ( Int, Session ) )
assignConnection id' conn sessions =
  case Map.lookup id' sessions of
    Just (session@(Session { sessionConnections=conns })) ->
      let index = IntMap.size conns
          updatedSession = (session { sessionConnections = IntMap.insert index conn conns })
      in
      ( Map.insert id' updatedSession sessions
      , Just ( index, updatedSession )
      )
    Nothing ->
      ( sessions, Nothing )


getSession :: ByteString -> Sessions -> Maybe Session
getSession = Map.lookup


emptySessions :: Sessions
emptySessions = Map.empty


removeSession :: SessionId -> Sessions -> Sessions
removeSession = Map.delete


disconnectSession :: ( SessionId, Int ) -> Sessions -> Sessions
disconnectSession ( sessionId, index ) = Map.alter (fmap update) sessionId
  where
    update :: Session -> Session
    update session@(Session { sessionConnections=conns }) =
      session { sessionConnections = IntMap.delete index conns }
