{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PlanningGame.Data.Id
  ( Id(..)
  , Incremental
  , generateId
  , empty
  , insert
  , lookup
  , delete
  , assocs
  , filter
  , null
  , alter
  ) where

import           Prelude            hiding (lookup, filter, null)
import           Control.Monad      (liftM)
import           Data.Aeson.Types   (ToJSON (..))
import           Data.ByteString    (ByteString)
import           Data.Bifunctor     (second)
import           Data.Map.Strict    (Map)
import           Data.Set           (Set)
import           Data.Text.Encoding (encodeUtf8)
import           Servant            (FromHttpApiData (..))
import           System.Random      (newStdGen, randomRs)

import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as TextEncoding


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


-- Incremental Ids


newtype IncId a =
  IncId { unIncId :: Int }
  deriving (Eq, Ord)


instance Show (IncId a) where
  show = show . unIncId


data WithId i a =
  WithId (IncId i) a


unwrapValue :: WithId i a -> a
unwrapValue (WithId _ a) = a


data Incremental i k v =
  Incremental Int (Map k (WithId i v))


instance Foldable (Incremental i k) where
  foldr f acc (Incremental _ m) = foldr (\a -> f $ unwrapValue a) acc m
  foldl f acc (Incremental _ m) = foldl (\acc -> f acc . unwrapValue) acc m


empty :: Incremental i k v
empty =
  Incremental 0 Map.empty


insert :: Ord k => k -> v -> Incremental i k v -> Incremental i k v
insert k v (Incremental i map) =
  Incremental (i + 1) $ Map.insert k (WithId (IncId i) v) map


lookup :: Ord k => k -> Incremental i k v -> Maybe v
lookup k (Incremental _ map) =
  unwrapValue <$> Map.lookup k map


delete :: Ord k => k -> Incremental i k v -> Incremental i k v
delete k (Incremental i map) =
  Incremental i $ Map.delete k map


assocs :: Incremental i k v -> [ ( k, v ) ]
assocs (Incremental _ map) =
  second unwrapValue <$> Map.assocs map


filter :: (v -> Bool) -> Incremental i k v -> Incremental i k v
filter predicate (Incremental i map) =
  Incremental i $ Map.filter (predicate . unwrapValue) map


null :: Incremental i k v -> Bool
null (Incremental _ map) = Map.null map


alter :: Ord k => (Maybe v -> Maybe v) -> k -> Incremental i k v -> Incremental i k v
alter f k (Incremental i map) =
  Incremental (i + 1) $ Map.alter g k map

  where
    g Nothing               = WithId (IncId i) <$> f Nothing
    g (Just (WithId id' v)) = WithId id' <$> f (Just v)
