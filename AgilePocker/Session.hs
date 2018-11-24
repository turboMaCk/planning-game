{-# LANGUAGE OverloadedStrings #-}

module AgilePocker.Session
  ( Session(..), SessionId, Sessions
  , emptySessions, addSession, getSession, removeSession
  ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import Control.Monad (liftM)
import System.Random (randomRs, newStdGen)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS


data Session = Session
  { sessionName :: T.Text
  , sessionConnection :: WS.Connection
  }


-- @TODO: Just alias for now


type SessionId = ByteString


type Sessions =
  Map.Map ByteString Session


addSession :: T.Text -> WS.Connection -> Sessions -> IO ( Sessions, SessionId )
addSession name conn sessions = do
  newId <- generateId sessions
  let newSession = Session name conn
  pure $ ( Map.insert newId newSession sessions
         , newId
         )


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
