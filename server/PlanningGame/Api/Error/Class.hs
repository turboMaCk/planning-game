{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.Error.Class
  ( Error(..)
  , ErrorType(..)
  , respondError
  ) where

import           Control.Monad.Error.Class (MonadError)
import           Data.Aeson.Types          (ToJSON (..), (.=))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Servant                   (ServantErr, err401, err403, err404,
                                            err409, err422, errBody, errHeaders,
                                            throwError)

import qualified Data.Aeson                as Aeson
import qualified Data.Text                 as Text


data ErrorType
    = NotFound
    | Unauthorized
    | Forbidden
    | Conflict
    | Unprocessable
    deriving (Generic)


instance ToJSON ErrorType


class Error a where
  toType     :: a -> ErrorType
  toReadable :: a -> Text


{-- Just to trick the compiler
--}
newtype WrapError a =
  WrapError a


instance (Show a, Error a) => ToJSON (WrapError a) where
  toJSON (WrapError err) =
    Aeson.object
      [ "status"  .= toType err
      , "error"   .= toJSON (Text.pack $ show err)
      , "message" .= toReadable err
      ]


respondError :: ( Show a, Error a) => MonadError ServantErr m => a -> m b
respondError res =
  throwError $ err
    { errBody = Aeson.encode $ toJSON (WrapError res)
    , errHeaders = [ ( "Content-Type", "application/json;charset=utf-8" ) ]
    }
  where
    err =
        case toType res of
            NotFound      -> err404
            Unauthorized  -> err401
            Forbidden     -> err403
            Conflict      -> err409
            Unprocessable -> err422
