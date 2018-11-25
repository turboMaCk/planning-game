{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Session
  ( Session(..), SessionId, Sessions
  , emptySessions, addSession, getSession, removeSession
  , assignConnection, disconnectSession
  ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (liftM)
import System.Random (randomRs, newStdGen)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Network.WebSockets as WS


data Session = Session
  { sessionId :: ByteString
  , sessionName :: T.Text
  , sessionConnections :: IntMap WS.Connection
  }


-- @TODO: Just alias for now
type SessionId = ByteString


type Sessions =
  Map ByteString Session


-- @TODO: Add name uniqueness?
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


-- @TODO: Those are rather abstract and should in different module


randString :: IO String
randString =
    liftM (take 32 . randomRs ('a','z')) newStdGen


generateId :: Map.Map ByteString a -> IO ByteString
generateId m = do
  newId <- encodeUtf8 . T.pack <$> randString
  if Map.member newId m then
    generateId m
  else
    pure newId
