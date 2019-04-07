{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Player
  ( Player(..)
  , Players
  , PlayerError(..)
  , create
  , add
  , addConnection
  , disconnect
  , get
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

import           Prelude                   hiding (lookup)
import           Data.Aeson.Types          (ToJSON (..), object, (.=))
import           Data.IntMap.Strict        (IntMap)
import           Data.Text                 (Text)
import           Network.WebSockets        (Connection)

import qualified Data.IntMap               as IntMap
import qualified Data.Text                 as Text
import qualified Network.WebSockets        as WS
import qualified Data.Maybe                as Maybe

import           PlanningGame.Api.Error    (Error (..), ErrorType (..))
import           PlanningGame.Data.Id      (Id, Incremental)
import           PlanningGame.Data.Session (Session, SessionId)


import qualified PlanningGame.Data.Id      as Id


data PlayerId


data Player = Player
  { name              :: Text
  , playerConnections :: IntMap Connection
  }


instance Show Player where
   show Player { name } = "Name: " <> show name


instance ToJSON Player where
  toJSON player@(Player { name }) =
    object
        [ "name" .= name
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


nameTaken :: Text -> Players -> Bool
nameTaken name' players =
  not $ Id.null $
    Id.filter ((==) name' . name) players


add :: Session -> Text -> Players -> Either PlayerError ( Players, Player )
add id' name players =
  if Text.null name then
    Left NameEmpty
  else if nameTaken name players then
    Left NameTaken
  else
    let
      newPlayer =
        create name
    in
    Right ( Id.insert id' newPlayer players, newPlayer )


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


addConnection :: Id SessionId -> Connection -> Players -> ( Players, Maybe ( Player, Int ) )
addConnection id' conn players =
  case Id.lookup id' players of
    Just player ->
      let
        ( updatedPlayer, index ) =
          addConnectionTo conn player
      in
      ( Id.insert id' updatedPlayer players
      , Just ( updatedPlayer, index )
      )

    Nothing ->
      ( players, Nothing )


removeConnectionFrom :: Int -> Player -> Player
removeConnectionFrom index player@(Player { playerConnections }) =
      player { playerConnections = IntMap.delete index playerConnections }


disconnect :: Id SessionId -> Int -> Players -> Players
disconnect sessionId index =
  Id.alter (fmap $ removeConnectionFrom index) sessionId


get :: Id SessionId -> Players -> Maybe Player
get = Id.lookup


empty :: Players
empty = Id.empty


allConnections :: Player -> [ WS.Connection ]
allConnections Player { playerConnections } =
  snd <$> IntMap.toList playerConnections


kick :: Id SessionId -> Players -> Players
kick =
  Id.delete


getByName :: Text -> Players -> Maybe ( Id SessionId, Player )
getByName name' =
  Maybe.listToMaybe . Maybe.mapMaybe filterId . Id.assocs

  where
    filterId ( id', player ) =
      if name player == name' then
        Just ( id', player )

      else
        Nothing

lookup :: Id SessionId -> Players -> Maybe Player
lookup = Id.lookup


insert :: Id SessionId -> Player -> Players -> Players
insert = Id.insert


toList :: Players -> [ (Id SessionId, Player) ]
toList = Id.assocs


anyOnline :: Players -> Bool
anyOnline =
    not . Id.null . Id.filter hasConnection
