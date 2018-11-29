{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module AgilePoker.Server.Authorization
  ( SessionAuth
  , authHeaderHandler, authCookieHandler
  ) where

import Servant (Handler, AuthProtect, errBody, throwError, err401, err403)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy as LB
import Network.Wai (Request, requestHeaders)
import Web.Cookie (parseCookies)
import qualified Control.Concurrent as Concurrent

import AgilePoker.Session
import AgilePoker.Api.Errors


type SessionAuth =
  AuthHandler Request Session


data AuthorizationError
  = SessionNotFound
  | SessionIdMissing


instance Error AuthorizationError where
  toType SessionNotFound  = Forbidden
  toType SessionIdMissing = Unauthorized

  toReadable SessionNotFound  = "Session Expired."
  toReadable SessionIdMissing = "Session required."


lookupSession :: MVar Sessions -> SessionId -> Handler Session
lookupSession state' sId = do
  state <- liftIO $ Concurrent.readMVar state'

  case getSession sId state of
    Just session ->
      pure session

    Nothing ->
      respondError SessionNotFound


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e =
  maybe (Left e) Right


handler :: (Request -> Maybe SessionId) -> MVar Sessions -> Request -> Handler Session
handler getSession state req = do
    either respondError (lookupSession state) $
        maybeToEither SessionIdMissing $ getSession req


-- Header Authorization


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionId itself
parseSessionId :: ByteString -> Maybe SessionId
parseSessionId headerVal =
  stripPrefix "Bearer " headerVal


authHeaderHandler :: MVar Sessions -> AuthHandler Request Session
authHeaderHandler = mkAuthHandler . handler get
  where
    get :: Request -> Maybe SessionId
    get req =
        parseSessionId
            =<< lookup "Authorization" (requestHeaders req)


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "header") = Session


-- Cookie Authorization


authCookieHandler :: MVar Sessions -> AuthHandler Request Session
authCookieHandler = mkAuthHandler . handler get
  where
    get :: Request -> Maybe SessionId
    get req =
        lookup "authorization" . parseCookies
            =<< lookup "cookie" (requestHeaders req)


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie") = Session
