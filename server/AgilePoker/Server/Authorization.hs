{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module AgilePoker.Server.Authorization
  ( HeaderAuth(..), CookieAuth(..), SessionHeaderAuth, SessionCookieAuth
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


data AuthorizationError
  = SessionNotFound
  | SessionIdMissing


instance Error AuthorizationError where
  toType SessionNotFound  = Forbidden
  toType SessionIdMissing = Unauthorized

  toReadable SessionNotFound  = "Session expired."
  toReadable SessionIdMissing = "Session required."


lookupSession :: MVar Sessions -> SessionId -> Handler Session
lookupSession state' sId = do
  state <- liftIO $ Concurrent.readMVar state'

  case getSession sId state of
    Just session -> pure session
    Nothing      -> respondError SessionNotFound


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e =
  maybe (Left e) Right


handler :: (Request -> Maybe SessionId) -> (Session -> a) -> MVar Sessions -> Request -> Handler a
handler getSessionId construct state req =
    either respondError (\id' -> construct <$> lookupSession state id') $
        maybeToEither SessionIdMissing $ getSessionId req


-- Header Authorization


newtype HeaderAuth a =
  HeaderAuth { unHeaderAuth :: a }


type SessionHeaderAuth =
  AuthHandler Request (HeaderAuth Session)


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionId itself
parseAuthorizationHeader :: ByteString -> Maybe SessionId
parseAuthorizationHeader headerVal =
  stripPrefix "Bearer " headerVal


authHeaderHandler :: MVar Sessions -> AuthHandler Request (HeaderAuth Session)
authHeaderHandler = mkAuthHandler . handler get HeaderAuth
  where
    get :: Request -> Maybe SessionId
    get req =
        parseAuthorizationHeader
            =<< lookup "Authorization" (requestHeaders req)


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "header") = (HeaderAuth Session)


-- Cookie Authorization


newtype CookieAuth a =
  CookieAuth { unCookieAuth :: a }


type SessionCookieAuth =
  AuthHandler Request (CookieAuth Session)


authCookieHandler :: MVar Sessions -> AuthHandler Request (CookieAuth Session)
authCookieHandler = mkAuthHandler . handler get CookieAuth
  where
    get :: Request -> Maybe SessionId
    get req =
        lookup "sessionId" . parseCookies
            =<< lookup "Cookie" (requestHeaders req)


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie") = (CookieAuth Session)
