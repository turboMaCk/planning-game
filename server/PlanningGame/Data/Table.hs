{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlanningGame.Data.Table
  ( Table(..)
  , Tables
  , TableId
  , Event
  , TableError(..)
  , emptyTables
  , createTable
  , joinTable
  , getTablePlayer
  , tableStreamHandler
  , tableActive
  ) where

import           Control.Concurrent          (MVar)
import           Control.Exception           (finally)
import           Control.Monad               (forM_, forever, mzero)
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Text                   (Text)
import           Network.WebSockets          (Connection)
import           Data.Aeson.Types            (ToJSON (..), Value (..), object,
                                              (.=))
import           Data.Map                    (Map)
import           Data.Time.Clock             (UTCTime)
import           Data.ByteString             (ByteString)

import qualified Control.Concurrent          as Concurrent
import qualified Data.ByteString.Lazy        as LazyByteString
import qualified Data.Aeson                  as Aeson
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified Data.Time.Clock             as Clock
import qualified Network.WebSockets          as WS

import           PlanningGame.Api.GameSnapshot (snapshot)
import           PlanningGame.Api.Error        (Error (..), ErrorType (..))

import           PlanningGame.Data.Game
import           PlanningGame.Data.Id          (Id, generateId)
import           PlanningGame.Data.Player
import           PlanningGame.Data.Session

import           PlanningGame.Data.Table.Msg


-- Types


data TableId


data Table = Table
  { tableId        :: Id TableId
  , tableBanker    :: ( Id SessionId, Player )
  , tablePlayers   :: Players
  , tableGame      :: Maybe Games
  , tableCreatedAt :: UTCTime
  }


instance ToJSON Table where
  toJSON table =
    object
        [ "id"      .= tableId table
        , "banker"  .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        , "game"    .=
          case tableGame table of
            Just game ->
              toJSON $ snapshot (tableBanker table) (tablePlayers table) game

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


emptyTables :: Tables
emptyTables =
  Map.empty


-- Event


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table
    | GameStarted ( Id SessionId, Player ) Players Games
    | VoteAccepted Player
    | VotingEnded ( Id SessionId, Player ) Players Games
    | GameEnded ( Id SessionId, Player ) Players Games


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    object
        [ "event"  .= Text.pack "PlayerJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    object
        [ "event"  .= Text.pack "PlayerStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    object
        [ "event"        .= Text.pack "SyncTableState"
        , "table"        .= table
        , "nextGameName" .= maybe "Task-1" autoNextName (tableGame table)
        ]
  toJSON (GameStarted dealer players games) =
    object
        [ "event" .= Text.pack "GameStarted"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (VoteAccepted player) =
    object
        [ "event"  .= Text.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded dealer players games) =
    object
        [ "event"        .= Text.pack "VotingEnded"
        , "game"         .= snapshot dealer players games
        , "nextGameName" .= autoNextName games
        ]
  toJSON (GameEnded dealer players games) =
    object
        [ "event" .= Text.pack "GameEnded"
        , "game"  .= snapshot dealer players games
        ]


encodeEvent :: Event -> ByteString
encodeEvent =
  LazyByteString.toStrict . Aeson.encode


-- Basic Operations


createTable :: Session -> Text -> Tables -> IO ( Tables, Either TableError Table )
createTable id' name' tables =
  let
    name =
      Text.strip name'
  in
  if Text.null name then
    pure ( tables,  Left $ PlayerError NameEmpty )

  else do
    tId <- generateId tables
    now <- Clock.getCurrentTime

    let banker = createPlayer name
    let newTable = Table tId ( id', banker ) emptyPlayers Nothing now

    mvarTable <- Concurrent.newMVar newTable

    pure
        ( Map.insert tId mvarTable tables
        , Right newTable
        )


tableActive :: Table -> Bool
tableActive Table { tableBanker, tablePlayers } =
  bankerOnline || anyPlayerOnline

  where
    bankerOnline =
      hasConnection $ snd tableBanker

    anyPlayerOnline =
      not $ Map.null $ Map.filter hasConnection tablePlayers


-- @TODO: Add check if session is not already present
joinTable :: Session -> Id TableId -> Text -> Tables -> IO ( Either TableError Table )
joinTable session tableId name' tables =
  let
    name =
      Text.strip name'
  in
  case Map.lookup tableId tables of
    Just mvar -> do
      table <- Concurrent.readMVar mvar

      if playerName (snd $ tableBanker table) == name then
        pure $ Left $ PlayerError NameTaken

      else
        let
            ePlayers = addPlayer session name (tablePlayers table)
        in
        case ePlayers of
          Right ( newPlayers, newPlayer ) ->
            Concurrent.modifyMVar mvar $ \t -> do
                let updatedTable = t { tablePlayers = newPlayers }

                -- Broadcast to connections
                broadcast t $ PlayerJoined newPlayer

                pure ( updatedTable, Right updatedTable )

          Left err ->
             pure $ Left $ PlayerError err

    Nothing ->
      pure $ Left TableNotFound


getTablePlayer :: Session -> Id TableId -> Tables -> IO (Either TableError Player)
getTablePlayer session tableId tables =
  fromMaybe (pure $ Left TableNotFound) $ getPlayer' <$>
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError Player)
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      if fst (tableBanker table) == session then
          pure $ Right $ snd (tableBanker table)

      else
          pure $ maybe (Left PlayerNotFound) Right $
            Map.lookup session $ tablePlayers table


allConnections :: Table -> [ Connection ]
allConnections Table { tableBanker, tablePlayers } =
  concat $ (allPlayerConnections $ snd tableBanker)
         : (foldr (\p acc -> allPlayerConnections p : acc) [] tablePlayers)


assignConnection :: Session -> Connection -> Table -> ( Table, Maybe ( Player, Int ) )
assignConnection session conn table@Table { tableBanker, tablePlayers } =
    if fst tableBanker == session then
        let ( updatedBanker, connId ) = addConnectionToPlayer conn $ snd tableBanker
        in
        ( table { tableBanker = ( session, updatedBanker ) }
        , Just ( updatedBanker, connId )
        )

    else
        let ( updatedPlayers, mPair ) = addPlayerConnection session conn tablePlayers
        in
        case mPair of
            Nothing   -> ( table, Nothing )
            Just pair -> ( table { tablePlayers = updatedPlayers }
                           , Just pair
                           )


allTablePlayers :: Table -> Players
allTablePlayers table =
  Map.insert bankerId banker $ tablePlayers table

  where
    (bankerId, banker) = tableBanker table


-- WS Handling


handleStreamMsg :: Session -> MVar Table -> Connection -> IO ()
handleStreamMsg session state conn = forever $ do
  bs :: LazyByteString.ByteString <- WS.receiveData conn
  let decoded :: Maybe Msg = Aeson.decode bs

  case decoded of
    -- @TODO: handle unrecosinable msg
    Nothing  -> pure ()
    Just msg ->
      Concurrent.modifyMVar_ state $ handleMsg conn session msg


disconnect :: MVar Table -> Id SessionId -> Int -> IO ()
disconnect state sessionId connId =
  Concurrent.modifyMVar_ state $ \table@Table { tableBanker=banker } ->
    if fst banker == sessionId then do
      let updatedTable = table { tableBanker = ( fst banker , removeConnectionFromPlayer connId $ snd banker ) }
      let player = snd $ tableBanker updatedTable

      if hasConnection player then
        pure ()

      else
        broadcast updatedTable $ PlayerStatusUpdate player

      pure updatedTable

    else do
      let updatedTable = table { tablePlayers = disconnectPlayer sessionId connId (tablePlayers table) }
      let mPlayer = Map.lookup sessionId $ tablePlayers updatedTable

      maybe mzero (broadcast updatedTable . PlayerStatusUpdate) mPlayer
      pure updatedTable


tableStreamHandler :: MVar Tables -> Session -> Id TableId -> Connection -> IO ()
tableStreamHandler state session id' conn = do
  tables <- Concurrent.readMVar state
  let mTable = Map.lookup id' tables

  case mTable of
    Just tableState -> do

        -- 1. Assign connection to player
        mConnId <- Concurrent.modifyMVar tableState $ pure . assignConnection session conn

        -- 2. Sync sate to new player
        table <- Concurrent.readMVar tableState
        WS.sendTextData conn $ encodeEvent $ SyncTableState table

        -- 3. Start player handler
        case mConnId of
            Just ( player, connId ) ->

                -- 3.1 Remove connection on disconnection
                flip finally (disconnect tableState session connId) $ do

                    -- 3.2 Ping Thread
                    WS.forkPingThread conn 30

                    -- 3.3 Broadcast join event
                    if playerNumberOfConnections player == 1 then do
                        table' <- Concurrent.readMVar tableState
                        broadcast table' $ PlayerStatusUpdate player

                    else
                        mzero

                    -- 3.4 Delegate to Msg handler
                    handleStreamMsg session tableState conn

            Nothing ->
                -- @TODO: player doesn't exist (session is not a member)
                mzero

    Nothing -> do
        -- @TODO: handle table doesn't exist
        putStrLn "Table not found"
        mzero


broadcast :: Table -> Event -> IO ()
broadcast table event = do
  forM_ (allConnections table) $ flip WS.sendTextData $ encodeEvent event


isBanker :: Session -> Table -> Bool
isBanker session Table { tableBanker=pair } =
  session == fst pair


-- MSG and Event handling


-- @TODO: refactor
handleMsg :: Connection -> Session -> Msg -> Table -> IO Table
handleMsg _ session (NewGame name) table
  | isBanker session table
  , isNothing (tableGame table) = do
      let game = startGame name
      let players = tablePlayers table
      broadcast table $ GameStarted (tableBanker table) players game
      pure $ table { tableGame = Just game }

  | not $ isBanker session table = do
      -- @TODO: Handle forbidden action
      pure $ table

  | otherwise =
      -- @TODO: Handle already started
      pure $ table

handleMsg _ session FinishRound table
  | isBanker session table =
      case tableGame table of
        Just games -> do
          let newGames = finishCurrentGame games
          broadcast table $ VotingEnded (tableBanker table) (tablePlayers table) newGames
          pure $ table { tableGame = Just newGames }

        Nothing ->
          -- @TODO: handled non started game
          pure table

  | otherwise =
    -- @TODO: handle forbidden
    pure table

handleMsg _ session (NextRound vote name) table
  | isBanker session table =
      case tableGame table of
        Just games -> do
          case nextRound vote name games of
            Left _ ->
              -- @TODO: missing err handling
              pure table
            Right newGames -> do
              broadcast table $ GameStarted (tableBanker table) (tablePlayers table) newGames
              pure $ table { tableGame = Just newGames }
        Nothing ->
          -- @TODO: handled non started game
          pure table

  | otherwise =
    -- @TODO: handle forbidden
    pure table

handleMsg _ session (Vote vote) table =
  case tableGame table of
    Just game ->
      case addVote session vote game of
        Right newGames -> do
          maybe (pure ()) (broadcast table . VoteAccepted) $ getPlayer session $ allTablePlayers table

          -- Auto end game when all voted
          if allVoted (allTablePlayers table) newGames then do
            let finishedNewGames = finishCurrentGame newGames

            broadcast table $ VotingEnded (tableBanker table) (tablePlayers table) finishedNewGames
            pure $ table { tableGame = Just finishedNewGames }

          else
            pure $ table { tableGame = Just newGames }

        -- @TODO: can't vote error
        Left _ ->
          pure table

    Nothing ->
      -- @TODO: hanlde not started
      pure table

handleMsg _ session (FinishGame vote) table
  | isBanker session table =
    case tableGame table of
      Just games -> do
        case completeGame vote games of
          Right newGames -> do
            broadcast table $ GameEnded (tableBanker table) (tablePlayers table) newGames
            pure $ table { tableGame = Just newGames }

          Left _ ->
            -- @TODO: handle already canceled
            pure table

      Nothing ->
        -- @TODO: handle game wasn't started
        pure table

  | otherwise =
      -- @TODO: handle forbidden
      pure table

handleMsg _ session RestartRound table
  | isBanker session table =
    case tableGame table of
      Just g -> do
        let game = restartCurrentGame g
        let players = tablePlayers table
        broadcast table $ GameStarted (tableBanker table) players game
        pure $ table { tableGame = Just game }

      Nothing ->
        -- @TODO: handle err
        pure table

  | otherwise =
      -- @TODO: handle forbidden
      pure table
