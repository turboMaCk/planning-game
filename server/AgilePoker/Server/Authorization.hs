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
import Network.Wai (Request, requestHeaders)
import Web.Cookie (parseCookies)
import qualified Control.Concurrent as Concurrent

import AgilePoker.Session


type SessionAuth =
  AuthHandler Request Session


lookupSession :: MVar Sessions -> SessionId -> Handler Session
lookupSession state' sessionId = do
  state <- liftIO $ Concurrent.readMVar state'
  case getSession sessionId state of
    Just session ->
      pure session

    Nothing ->
      throwError $ err403 { errBody = "Invalid SessionId" }


-- Header Authorization


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionId itself
parseSessionId :: ByteString -> Maybe SessionId
parseSessionId headerVal =
  stripPrefix "Bearer " headerVal


authHeaderHandler :: MVar Sessions -> AuthHandler Request Session
authHeaderHandler state = mkAuthHandler handler
  where
    maybeToEither e =
      maybe (Left e) Right

    throw401 msg =
        throwError $ err401 { errBody = msg }

    mSessionId :: Request -> Maybe SessionId
    mSessionId req =
        parseSessionId
            =<< lookup "AUTHORIZATION" (requestHeaders req)

    handler :: Request -> Handler Session
    handler req = do
      either throw401 (lookupSession state) $
        maybeToEither "Missing SessionId" $ mSessionId req

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "header") = Session


-- Cookie Authorization


authCookieHandler :: MVar Sessions -> AuthHandler Request Session
authCookieHandler state = mkAuthHandler handler
  where
  maybeToEither e =
    maybe (Left e) Right

  throw401 msg =
    throwError $ err401 { errBody = msg }

  mSessionId :: Request -> Maybe SessionId
  mSessionId req =
    lookup "authorization" . parseCookies
        =<< lookup "cookie" (requestHeaders req)

  handler :: Request -> Handler Session
  handler req = either throw401 (lookupSession state) $ do
    maybeToEither "Missing SessionId" $ mSessionId req


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie") = Session
