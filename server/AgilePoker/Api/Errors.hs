{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}

module AgilePoker.Api.Errors where

import Servant (ServantErr, throwError, errBody, errHeaders, err401, err403, err404, err409)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson.Types (ToJSON(..), (.=))
import Data.Aeson (encode)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Aeson.Types as AT


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


-- Just to trick the compiler
newtype Wrap a = Wrap { unWrap :: a }


instance Error a => ToJSON (Wrap a) where
  toJSON (Wrap err) =
    AT.object
      [ "error"   .= toType err
      , "message" .= toReadable err
      ]


respondError :: Error a => MonadError ServantErr m => a -> m b
respondError res =
  throwError $ err { errBody = encode $ toJSON (Wrap res)
                   , errHeaders = [ ("Content-Type", "application/json;charset=utf-8") ]
                   }
  where
    err =
        case toType res of
            NotFound     -> err404
            Unauthorized -> err401
            Forbidden    -> err403
            Conflict     -> err409
