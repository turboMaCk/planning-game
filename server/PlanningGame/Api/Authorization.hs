{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module PlanningGame.Api.Authorization
  ( HeaderAuth(..)
  , CookieAuth(..)
  , SessionHeaderAuth
  , SessionCookieAuth
  , authHeaderHandler
  , authCookieHandler
  ) where

import           Control.Concurrent                (MVar)
import           Control.Monad.IO.Class            (liftIO)
import           Data.ByteString                   (ByteString)
import           Network.Wai                       (Request)
import           Servant                           (AuthProtect, Handler)
import           Servant.Server.Experimental.Auth  (AuthHandler, AuthServerData,
                                                    mkAuthHandler)


import qualified Network.Wai as Wai
import qualified Control.Concurrent                as Concurrent
import qualified Data.ByteString                   as ByteString
import qualified Web.Cookie                        as Cookie

import           PlanningGame.Api.Error              (Error (..), ErrorType(..))
import           PlanningGame.Data.Id                (Id (..))
import           PlanningGame.Data.Session           (Session, SessionId,
                                                    Sessions, getSession)

import qualified PlanningGame.Api.Error as Error

data AuthorizationError
  = SessionNotFound
  | SessionIdMissing
  deriving (Show, Eq)


instance Error AuthorizationError where
  toType SessionNotFound  = Forbidden
  toType SessionIdMissing = Unauthorized

  toReadable SessionNotFound  = "Session expired."
  toReadable SessionIdMissing = "Session required."


lookupSession :: MVar Sessions -> Id SessionId -> Handler Session
lookupSession state' sId = do
  state <- liftIO $ Concurrent.readMVar state'

  case getSession sId state of
    Just session -> pure session
    Nothing      -> Error.respond SessionNotFound


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e =
  maybe (Left e) Right


handler :: (Request -> Maybe (Id SessionId)) -> (Session -> a) -> MVar Sessions -> Request -> Handler a
handler getSessionId construct state req =
    either Error.respond (\id' -> construct <$> lookupSession state id') $
        maybeToEither SessionIdMissing $ getSessionId req


-- Header Authorization


newtype HeaderAuth a =
  HeaderAuth { unHeaderAuth :: a }


type SessionHeaderAuth =
  AuthHandler Request (HeaderAuth Session)


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionId itself
parseAuthorizationHeader :: ByteString -> Maybe (Id SessionId)
parseAuthorizationHeader headerVal =
  Id <$> ByteString.stripPrefix "Bearer " headerVal


authHeaderHandler :: MVar Sessions -> AuthHandler Request (HeaderAuth Session)
authHeaderHandler = mkAuthHandler . handler get HeaderAuth
  where
    get :: Request -> Maybe (Id SessionId)
    get req =
        parseAuthorizationHeader
            =<< lookup "Authorization" (Wai.requestHeaders req)


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
    get :: Request -> Maybe (Id SessionId)
    get req = Id <$>
        (lookup "sessionId" . Cookie.parseCookies
            =<< lookup "Cookie" (Wai.requestHeaders req))


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie") = (CookieAuth Session)
