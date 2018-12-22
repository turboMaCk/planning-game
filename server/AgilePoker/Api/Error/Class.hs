{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.Error.Class
  ( Error(..)
  , ErrorType(..)
  , respondError
  ) where

import           Control.Monad.Error.Class (MonadError)
import           Data.Aeson.Types          (ToJSON (..), (.=))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Servant                   (ServantErr, err401, err403, err404,
                                            err409, errBody, errHeaders,
                                            throwError)

import qualified Data.Aeson                as Aeson


data ErrorType
    = NotFound
    | Unauthorized
    | Forbidden
    | Conflict
    deriving (Generic)


instance ToJSON ErrorType


class Error a where
  toType     :: a -> ErrorType
  toReadable :: a -> Text


{-- Just to trick the compiler
--}
newtype Wrap a = Wrap { unWrap :: a }


instance Error a => ToJSON (Wrap a) where
  toJSON (Wrap err) =
    Aeson.object
      [ "error"   .= toType err
      , "message" .= toReadable err
      ]


respondError :: Error a => MonadError ServantErr m => a -> m b
respondError res =
  throwError $ err
    { errBody = Aeson.encode $ toJSON (Wrap res)
    , errHeaders = [ ( "Content-Type", "application/json;charset=utf-8" ) ]
    }
  where
    err =
        case toType res of
            NotFound     -> err404
            Unauthorized -> err401
            Forbidden    -> err403
            Conflict     -> err409
