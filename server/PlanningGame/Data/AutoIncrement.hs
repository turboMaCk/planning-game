module PlanningGame.Data.AutoIncrement
  ( Incremental
  , WithId(..)
  , unwrapValue
  , empty
  , insert
  , lookup
  , delete
  , assocs
  , filter
  , null
  , alter
  , mock
  ) where

import           Data.Map.Strict (Map)
import           Prelude         hiding (filter, lookup, map, null)
import           Data.Aeson.Types                (ToJSON (..))

import qualified Data.Map        as Map


newtype IncId a =
  IncId { unIncId :: Int }
  deriving (Eq, Ord)


instance Show (IncId a) where
  show = show . unIncId


data WithId i a =
  WithId (IncId i) a


instance ToJSON (IncId a) where
  toJSON (IncId i) = toJSON i


unwrapValue :: WithId i a -> a
unwrapValue (WithId _ a) = a


data Incremental i k v =
  Incremental Int (Map k (WithId i v))


instance Foldable (Incremental i k) where
  foldr f acc (Incremental _ m) = foldr (\a -> f $ unwrapValue a) acc m
  foldl f acc (Incremental _ m) = foldl (\acc' -> f acc' . unwrapValue) acc m


empty :: Incremental i k v
empty =
  Incremental 0 Map.empty


insert :: Ord k => k -> v -> Incremental i k v -> ( Incremental i k v, WithId i v )
insert k v (Incremental i map) =
  (Incremental (i + 1) $ Map.insert k withId map, withId)
  where
    withId = WithId (IncId i) v


lookup :: Ord k => k -> Incremental i k v -> Maybe (WithId i v)
lookup k (Incremental _ map) =
  Map.lookup k map


delete :: Ord k => k -> Incremental i k v -> Incremental i k v
delete k (Incremental i map) =
  Incremental i $ Map.delete k map


assocs :: Incremental i k v -> [ (k, WithId i v )]
assocs (Incremental _ map) =
    Map.assocs map


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


-- @TODO: used as a temporary hack
mock :: Int -> v -> WithId i v
mock i v = WithId (IncId i) v
