{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AgilePoker.Data.Id
  ( Id(..)
  , generateId
  ) where

import           Control.Monad      (liftM)
import           Data.Aeson.Types   (ToJSON (..))
import           Data.ByteString    (ByteString)
import qualified Data.Map           as Map
import           Data.Map.Strict    (Map)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TextEncoding
import           Servant            (FromHttpApiData (..))
import           System.Random      (newStdGen, randomRs)


{-- This is implementation of Id as a so called phantom type
--}
newtype Id a =
  Id { unId :: ByteString }


instance Show (Id a) where
  show = show . unId


instance Eq (Id a) where
  (==) (Id a) (Id b) = a == b


instance Ord (Id a) where
  compare (Id a) (Id b) = compare a b


instance FromHttpApiData (Id a) where
  parseUrlPiece bs = Right . Id $ TextEncoding.encodeUtf8 bs


instance ToJSON (Id a) where
  toJSON = toJSON . TextEncoding.decodeUtf8 . unId


randString :: IO String
randString =
    liftM (take 32 . randomRs ('a','z')) newStdGen


class Lookupable a id where
  member :: Id id -> a -> Bool


instance Lookupable (Set (Id a)) a where
  member = Set.member


instance Lookupable (Map (Id id) a) id where
  member = Map.member


generateId :: Lookupable a id => a -> IO (Id id)
generateId m = do
  newId <- Id . encodeUtf8 . Text.pack <$> randString
  if member newId m then
    generateId m
  else
    pure newId
