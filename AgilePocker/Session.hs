{-# LANGUAGE OverloadedStrings #-}

module AgilePocker.Session
  ( Session(..), SessionId, Sessions
  , emptySessions, addSession, getSession, removeSession
  , assignConnection
  ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import Control.Monad (liftM)
import System.Random (randomRs, newStdGen)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS


data Session = Session
  { sessionId :: ByteString
  , sessionName :: T.Text
  , sessionConnection :: Maybe WS.Connection
  }


-- @TODO: Just alias for now


type SessionId = ByteString


type Sessions =
  Map.Map ByteString Session


addSession :: T.Text -> Sessions -> IO ( Sessions, SessionId )
addSession name sessions = do
  newId <- generateId sessions
  pure $ ( Map.insert newId (Session newId name Nothing) sessions
         , newId
         )


-- @TODO: silent remove failing
assignConnection :: SessionId -> WS.Connection -> Sessions -> Sessions
assignConnection id' conn sessions = Map.alter (fmap update) id' sessions
  where
    update session =
      session { sessionConnection = Just conn }


getSession :: ByteString -> Sessions -> Maybe Session
getSession = Map.lookup


emptySessions :: Sessions
emptySessions = Map.empty


removeSession :: SessionId -> Sessions -> Sessions
removeSession = Map.delete


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
