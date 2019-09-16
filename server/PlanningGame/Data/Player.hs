{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Player
  ( Player(..)
  , PlayerStatus(..)
  , PlayerId
  , Players
  , PlayerError(..)
  , create
  , getName
  , collection
  , changeName
  , add
  , addConnection
  , disconnect
  , empty
  , allConnections
  , removeConnectionFrom
  , hasConnection
  , numberOfConnections
  , kick
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
import qualified Data.Text                       as Text
import qualified Network.WebSockets              as WS

import           PlanningGame.Api.Error          (Error (..), ErrorType (..))
import           PlanningGame.Data.AutoIncrement (Incremental, WithId (..))
import           PlanningGame.Data.Id            (Id)
import           PlanningGame.Data.Session       (Session, SessionId)

import qualified PlanningGame.Data.AutoIncrement as Inc


data PlayerId


data PlayerStatus
  = Active
  | Idle
  deriving (Eq)


data Player = Player
  { name              :: Text
  -- @TODO: autoincrement can be maybe utilized here as well
  , playerConnections :: IntMap Connection
  , status            :: PlayerStatus
  }


instance Show Player where
   show Player { name } = "Name: " <> show name


instance ToJSON (WithId PlayerId Player) where
  toJSON (WithId id' player@(Player { name, status })) =
    Aeson.object
        [ "id"        .= toJSON id'
        , "name"      .= name
        , "connected" .= hasConnection player
        , "isActive"  .= toJSON (status == Active)
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


create :: Text -> Bool -> Player
create n isActive =
  Player n IntMap.empty (if isActive then Active else Idle)


getName :: WithId PlayerId Player -> Text
getName = name . Inc.unwrapValue


nameTaken :: Text -> Players -> Bool
nameTaken name' players =
  not $ Inc.null $
    Inc.filter ((==) name' . name) players


add :: Session -> Text -> Bool -> Players -> Either PlayerError ( Players, WithId PlayerId Player )
add sesId' name' isActive players
  | Text.null name =
    Left NameEmpty

  | nameTaken name players =
    Left NameTaken

  | otherwise =
    Right $ Inc.insert sesId' (create name isActive) players

  where
    name = Text.strip name'


--- @TODO: using Maybe because PlayerNotfound is already part of TableError
--- Probably need to to rething error handling types because of this case
changeName :: Session -> Text -> Players -> Either PlayerError (Maybe ( Players, WithId PlayerId Player ))
changeName session name' players
  | Text.null name =
    Left NameEmpty

  | nameTaken name players =
    Left NameTaken

  | otherwise =
    case lookup session players of
      Just player ->
        let
          newPlayer = (\p -> p { name = name }) $ Inc.unwrapValue player
        in
        Right $ Just $ Inc.insert session newPlayer players

      Nothing ->
        -- Doesn't feel right lol
        Right Nothing

  where
    name = Text.strip name'


addConnectionTo :: Connection -> Player -> ( Player, Int )
addConnectionTo conn player@Player { playerConnections } =
  ( updatedPlayer, index )

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


addConnection :: Session -> Connection -> Players -> ( Players, Maybe ( WithId PlayerId Player, Int ) )
addConnection id' conn players =
  case Inc.lookup id' players of
    Just player ->
      let
        ( updatedPlayer, index ) =
            addConnectionTo conn $ Inc.unwrapValue player

        ( updatedPlayers, playerWithId ) =
            Inc.insert id' updatedPlayer players
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
empty =
  Inc.empty


allConnections :: Player -> [ WS.Connection ]
allConnections Player { playerConnections } =
  snd <$> IntMap.toList playerConnections


kick :: Id SessionId -> Players -> Players
kick =
  Inc.delete


lookup :: Id SessionId -> Players -> Maybe (WithId PlayerId Player)
lookup =
  Inc.lookup


-- TODO: review
insert :: Id SessionId -> Player -> Players -> Players
insert i p =
  fst . Inc.insert i p


toList :: Players -> [ (Id SessionId, Player) ]
toList ps =
  second Inc.unwrapValue <$> Inc.assocs ps


collection :: Players -> [ WithId PlayerId Player ]
collection players =
  snd <$> Inc.assocs players


anyOnline :: Players -> Bool
anyOnline =
  not . Inc.null . Inc.filter hasConnection
