{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlanningGame.Data.Table
  ( Table(..)
  , Tables
  , TableId
  , TableError(..)
  , empty
  , create
  , getPlayer
  , isActive
  , isDealer
  , lookup
  , assignConnection
  , allConnections
  , sessionByPlayerId
  , getDealer
  , updatePlayer
  ) where

import           Control.Concurrent              (MVar)
import           Data.Aeson.Types                (ToJSON (..), Value (..), (.=))
import           Data.Map                        (Map)
import           Data.Text                       (Text)
import           Data.Time.Clock                 (UTCTime)
import           Network.WebSockets              (Connection)
import           Prelude                         hiding (lookup)

import qualified Control.Concurrent              as Concurrent
import qualified Data.Aeson                      as Aeson
import qualified Data.Map.Strict                 as Map
import qualified Data.Time.Clock                 as Clock

import           PlanningGame.Api.Error          (Error (..), ErrorType (..))
import           PlanningGame.Api.GameSnapshot   (snapshot)

import           PlanningGame.Data.AutoIncrement (WithId)
import           PlanningGame.Data.Game          (GameError, Games)
import           PlanningGame.Data.Id            (Id (..), generateId)
import           PlanningGame.Data.Player        (Player (..), PlayerError (..),
                                                  PlayerStatus, Players)
import           PlanningGame.Data.Session       (Session, SessionId)

import qualified PlanningGame.Data.AutoIncrement as Inc
import qualified PlanningGame.Data.Player        as Player


-- Types


data TableId


data Table = Table
  { tableId   :: Id TableId
  , dealer    :: Id SessionId
  , players   :: Players
  , game      :: Maybe Games
  , createdAt :: UTCTime
  }


instance ToJSON Table where
  toJSON table =
    Aeson.object
        [ "id"      .= tableId table
        , "dealer"  .= getDealer table
        , "players" .= Player.collection (players table)
        , "game"    .=
          case game table of
            Just game ->
              toJSON $ snapshot (players table) game

            Nothing ->
              Null
        ]


type Tables =
  Map (Id TableId) (MVar Table)


data TableError
  = TableNotFound
  | PlayerNotFound
  | PlayerError PlayerError
  | GameError GameError
  deriving (Eq)


instance Show TableError where
  show TableNotFound   = "TableNotFound"
  show PlayerNotFound  = "PlayerNotFound"
  show (PlayerError e) = "PlayerError:" <> show e
  show (GameError e)   = "GameError:" <> show e


instance Error TableError where
  toType TableNotFound   = NotFound
  toType PlayerNotFound  = Forbidden
  toType (GameError e)   = toType e
  toType (PlayerError e) = toType e

  toReadable TableNotFound   = "Table doesn't exist."
  toReadable PlayerNotFound  = "You're not a player on this table."
  toReadable (GameError e)   = toReadable e
  toReadable (PlayerError e) = toReadable e


empty :: Tables
empty =
  Map.empty


-- Basic Operations


create :: Session -> Text -> PlayerStatus -> Tables -> IO ( Tables, Either TableError Table )
create id' name' status' tables = do
  tId <- generateId tables
  now <- Clock.getCurrentTime

  case Player.add id' name' status' Player.empty of
    Right (players, _) -> do
      let newTable = Table tId id' players Nothing now

      mvarTable <- Concurrent.newMVar newTable

      pure
          ( Map.insert tId mvarTable tables
          , Right newTable
          )
    Left err ->
      pure ( tables, Left $ PlayerError err )


isActive :: Table -> Bool
isActive Table { players } =
  Player.anyOnline players


getPlayer :: Session -> Id TableId -> Tables -> IO (Either TableError (WithId Player))
getPlayer session tableId tables =
  maybe (pure $ Left TableNotFound) getPlayer' $
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError (WithId Player))
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      pure $ maybe (Left PlayerNotFound) Right $
        Player.lookup session $ players table


allConnections :: Table -> [ Connection ]
allConnections Table { players } =
   concat $ foldr (\p acc -> Player.allConnections p : acc) [] players


assignConnection :: Session -> Connection -> Table -> ( Table, Maybe ( WithId Player, Int ) )
assignConnection session conn table@Table { players } =
  let
    ( updatedPlayers, mPair ) =
      Player.addConnection session conn players
  in
    case mPair of
      Nothing   -> ( table, Nothing )
      Just pair -> ( table { players = updatedPlayers }
                   , Just pair
                   )

isDealer :: Session -> Table -> Bool
isDealer session Table { dealer } =
  session == dealer


lookup :: Id TableId -> Tables -> Maybe (MVar Table)
lookup =
  Map.lookup


sessionByPlayerId :: Int -> Table -> Maybe (Id SessionId)
sessionByPlayerId id' table =
  fst <$> Inc.getById id' (players table)


getDealer :: Table -> WithId Player
getDealer Table { dealer, players } =
  case Inc.lookup dealer players of
    Just p  -> p
    -- Should not ever happen
    -- @TODO: avoidable bottom
    Nothing -> undefined


updatePlayer :: (Player -> Maybe Player) -> Session -> Table -> Table
updatePlayer f session table@(Table { players }) =
  table { players = Inc.update f session players }
