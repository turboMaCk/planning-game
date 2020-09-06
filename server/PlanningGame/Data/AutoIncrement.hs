module PlanningGame.Data.AutoIncrement
  ( Incremental
  , WithId(..)
  , unwrapValue
  , getById
  , unwrapId
  , empty
  , insert
  , lookup
  , delete
  , assocs
  , filter
  , null
  , alter
  , update
  ) where

import           Data.Aeson.Types (ToJSON (..))
import           Data.List        (find)
import           Data.Map.Strict  (Map)
import           Prelude          hiding (filter, lookup, map, null)

import qualified Data.Map         as Map


newtype IncId a =
  IncId { unIncId :: Int }
  deriving (Eq, Ord)


instance Show (IncId a) where
  show = show . unIncId


data WithId a =
  WithId (IncId a) a


instance Functor WithId where
  fmap f (WithId (IncId i) a) = WithId (IncId i) (f a)


instance ToJSON (IncId a) where
  toJSON (IncId i) = toJSON i


unwrapValue :: WithId a -> a
unwrapValue (WithId _ a) =
  a


unwrapId :: WithId a -> Int
unwrapId (WithId id' _) =
  unIncId id'


data Incremental k v =
  Incremental Int (Map k (WithId v))


instance Foldable (Incremental k) where
  foldr f acc (Incremental _ m) = foldr (f . unwrapValue) acc m
  foldl f acc (Incremental _ m) = foldl (\acc' -> f acc' . unwrapValue) acc m


empty :: Incremental k v
empty =
  Incremental 1 Map.empty


insert :: Ord k => k -> v -> Incremental k v -> ( Incremental k v, WithId v )
insert k v (Incremental i map) =
  case Map.lookup k map of
    Just (WithId id' _) ->
        ( Incremental i $ Map.insert k (WithId id' v) map
        , WithId id' v
        )

    Nothing ->
        ( Incremental (i + 1) $ Map.insert k (WithId (IncId i) v) map
        , WithId (IncId i) v
        )


lookup :: Ord k => k -> Incremental k v -> Maybe (WithId v)
lookup k (Incremental _ map) =
  Map.lookup k map


delete :: Ord k => k -> Incremental k v -> Incremental k v
delete k (Incremental i map) =
  Incremental i $ Map.delete k map


assocs :: Incremental k v -> [ ( k, WithId v )]
assocs (Incremental _ map) =
  Map.assocs map


getById :: Int -> Incremental k v -> Maybe ( k, WithId v )
getById id' =
  find f . assocs

  where
    f ( _, WithId i _ )  = unIncId i == id'


filter :: (v -> Bool) -> Incremental k v -> Incremental k v
filter predicate (Incremental i map) =
  Incremental i $ Map.filter (predicate . unwrapValue) map


null :: Incremental k v -> Bool
null (Incremental _ map) = Map.null map


alter :: Ord k => (Maybe v -> Maybe v) -> k -> Incremental k v -> Incremental k v
alter f k (Incremental i map) =
  Incremental (i + 1) $ Map.alter g k map

  where
    g Nothing               = WithId (IncId i) <$> f Nothing
    g (Just (WithId id' v)) = WithId id' <$> f (Just v)


update :: Ord k => (v -> Maybe v) -> k -> Incremental k v -> Incremental k v
update f k (Incremental i map) =
  Incremental i $ Map.update g k map

  where
    g (WithId id' val) =
      WithId id' <$> f val
