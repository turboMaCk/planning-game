{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Player
  ( Player(..)
  , PlayerId
  , Players
  , PlayerError(..)
  , create
  , getName
  , collection
  , add
  , addConnection
  , disconnect
  , empty
  , allConnections
  , addConnectionTo
  , removeConnectionFrom
  , hasConnection
  , numberOfConnections
  , kick
  , getByName
  , lookup
  , insert
  , toList
  , anyOnline
  ) where

import           Data.Aeson.Types                (ToJSON (..), (.=))
import           Data.Bifunctor                  (second)
import           Data.IntMap.Strict              (IntMap)
import           Data.Text                       (Text)
import           Network.WebSockets              (Connection)
import           Prelude                         hiding (lookup)

import qualified Data.Aeson.Types                as Aeson
import qualified Data.IntMap                     as IntMap
import qualified Data.Maybe                      as Maybe
import qualified Data.Text                       as Text
import qualified Network.WebSockets              as WS

import           PlanningGame.Api.Error          (Error (..), ErrorType (..))
import           PlanningGame.Data.AutoIncrement (Incremental, WithId (..))
import           PlanningGame.Data.Id            (Id)
import           PlanningGame.Data.Session       (Session, SessionId)

import qualified PlanningGame.Data.AutoIncrement as Inc


data PlayerId


data Player = Player
  { name              :: Text
  , playerConnections :: IntMap Connection
  }


instance Show Player where
   show Player { name } = "Name: " <> show name


instance ToJSON (WithId PlayerId Player) where
  toJSON (WithId id' player@(Player { name })) =
    Aeson.object
        [ "id" .= toJSON id'
        , "name" .= name
        , "connected" .= hasConnection player
        ]


type Players =
  Incremental PlayerId (Id SessionId) Player
  -- Map (Id SessionId) Player


data PlayerError
  = NameTaken
  | NameEmpty
  deriving (Show, Eq)


instance Error PlayerError where
  toType NameTaken = Conflict
  toType NameEmpty = Unprocessable

  toReadable NameTaken = "Player with this name already exists."
  toReadable NameEmpty = "Name can't be empty."


create :: Text -> Player
create n =
  Player n IntMap.empty


getName :: WithId PlayerId Player -> Text
getName = name . Inc.unwrapValue


nameTaken :: Text -> Players -> Bool
nameTaken name' players =
  not $ Inc.null $
    Inc.filter ((==) name' . name) players


add :: Session -> Text -> Players -> Either PlayerError ( Players, WithId PlayerId Player )
add id' name players =
  if Text.null name then
    Left NameEmpty

  else if nameTaken name players then
    Left NameTaken

  else
    Right $ Inc.insert id' (create name) players


addConnectionTo :: Connection -> Player -> (Player, Int)
addConnectionTo conn player@Player { playerConnections } =
  (updatedPlayer, index)

  where
    index =
      IntMap.size playerConnections

    updatedPlayer =
      player { playerConnections = IntMap.insert index conn playerConnections }


hasConnection :: Player -> Bool
hasConnection Player { playerConnections } =
  not $ IntMap.null playerConnections


numberOfConnections :: Player -> Int
numberOfConnections Player { playerConnections } =
  IntMap.size playerConnections


addConnection :: Id SessionId -> Connection -> Players -> ( Players, Maybe ( WithId PlayerId Player, Int ) )
addConnection id' conn players =
  case Inc.lookup id' players of
    Just player ->
      let
        ( updatedPlayer, index ) =
          addConnectionTo conn $ Inc.unwrapValue player

        (updatedPlayers, playerWithId) = Inc.insert id' updatedPlayer players
      in
      ( updatedPlayers
      , Just ( playerWithId, index )
      )

    Nothing ->
      ( players, Nothing )


removeConnectionFrom :: Int -> Player -> Player
removeConnectionFrom index player@(Player { playerConnections }) =
      player { playerConnections = IntMap.delete index playerConnections }


disconnect :: Id SessionId -> Int -> Players -> Players
disconnect sessionId index =
  Inc.alter (fmap $ removeConnectionFrom index) sessionId


empty :: Players
empty = Inc.empty


allConnections :: Player -> [ WS.Connection ]
allConnections Player { playerConnections } =
  snd <$> IntMap.toList playerConnections


kick :: Id SessionId -> Players -> Players
kick =
  Inc.delete


getByName :: Text -> Players -> Maybe ( Id SessionId, WithId PlayerId Player )
getByName name' =
  Maybe.listToMaybe . Maybe.mapMaybe filterId . Inc.assocs

  where
    filterId ( id', player ) =
      if getName player == name' then
        Just ( id', player )

      else
        Nothing


lookup :: Id SessionId -> Players -> Maybe (WithId PlayerId Player)
lookup = Inc.lookup


-- TODO: review
insert :: Id SessionId -> Player -> Players -> Players
insert i p = fst . Inc.insert i p


toList :: Players -> [ (Id SessionId, Player) ]
toList ps = second Inc.unwrapValue <$> Inc.assocs ps


collection :: Players -> [ WithId PlayerId Player ]
collection players = snd <$> Inc.assocs players


anyOnline :: Players -> Bool
anyOnline =
    not . Inc.null . Inc.filter hasConnection
