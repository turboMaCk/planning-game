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
  , getSessionId
  ) where

import           Data.Aeson.Types          (ToJSON (..), object, (.=))
import           Data.IntMap.Strict        (IntMap)
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import           Network.WebSockets        (Connection)

import qualified Data.IntMap               as IntMap
import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import qualified Network.WebSockets        as WS
import qualified Data.Maybe                as Maybe

import           PlanningGame.Api.Error    (Error (..), ErrorType (..))
import           PlanningGame.Data.Id      (Id)
import           PlanningGame.Data.Session (Session, SessionId)


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
  Map (Id SessionId) Player


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
nameTaken name' sessions =
  not $ Map.null $
    Map.filter ((==) name' . name) sessions


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
    Right ( Map.insert id' newPlayer players, newPlayer )


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
  case Map.lookup id' players of
    Just player ->
      let
        ( updatedPlayer, index ) =
          addConnectionTo conn player
      in
      ( Map.insert id' updatedPlayer players
      , Just ( updatedPlayer, index )
      )

    Nothing ->
      ( players, Nothing )


removeConnectionFrom :: Int -> Player -> Player
removeConnectionFrom index player@(Player { playerConnections }) =
      player { playerConnections = IntMap.delete index playerConnections }


disconnect :: Id SessionId -> Int -> Players -> Players
disconnect sessionId index =
  Map.alter (fmap $ removeConnectionFrom index) sessionId


get :: Id SessionId -> Players -> Maybe Player
get = Map.lookup


empty :: Players
empty = Map.empty


allConnections :: Player -> [ WS.Connection ]
allConnections Player { playerConnections } =
  snd <$> IntMap.toList playerConnections


-- @TODO: this might need to close the connection
kick :: Text -> Players -> Players
kick name' =
  Map.filter ((/=) name' . name)


getSessionId :: Text -> Players -> Maybe (Id SessionId)
getSessionId name' =
  headMay . Maybe.mapMaybe filterId . Map.assocs

  where
    headMay [] = Nothing
    headMay x  = Just $ head x

    filterId (id', rec) =
      if name rec == name' then
        Just id'

      else
        Nothing
