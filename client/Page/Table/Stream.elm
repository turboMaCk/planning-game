port module Page.Table.Stream exposing
    ( Event(..)
    , Msg(..)
    , StreamError(..)
    , connect
    , disconnect
    , observe
    , sendMsg
    )

import Data exposing (Game, Player, Table, Vote)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode exposing (Value)


port observe_ : (String -> msg) -> Sub msg


port streamError : (String -> msg) -> Sub msg


port connect : String -> Cmd msg


port disconnect : () -> Cmd msg


port emit : String -> Cmd msg


type StreamError
    = CantConnect
    | Disconnected
    | DecodingError Decode.Error


streamErrorDecoder : String -> Maybe StreamError
streamErrorDecoder str =
    let
        result ( val, c ) acc =
            if val == str then
                Just c

            else
                acc
    in
    [ ( "can_not_connect", CantConnect )
    , ( "connection_closed", Disconnected )
    ]
        |> List.foldl result Nothing


type Event
    = PlayerJoin Player
    | PlayerStatusUpdate Player
    | SyncTableState ( Table, Game ) String
    | GameStarted Game
    | VoteAccepted Player
    | VotingEnded Game String
    | GameEnded Game
    | PlayerKicked Player


eventField : String -> (a -> Event) -> Decoder a -> Decoder Event
eventField value toEvent decoder =
    let
        checkValue str =
            if str == value then
                Decode.map toEvent decoder

            else
                Decode.fail <|
                    "event doesn't match, expected: "
                        ++ value
                        ++ " actual: "
                        ++ str
    in
    Decode.field "event" Decode.string
        |> Decode.andThen checkValue


eventDecoder : Decoder Event
eventDecoder =
    let
        nextGameName =
            Decode.field "nextGameName" Decode.string
    in
    Decode.oneOf
        [ eventField "PlayerJoined" PlayerJoin <| Decode.field "player" Data.playerDecoder
        , eventField "PlayerStatusUpdate" PlayerStatusUpdate <| Decode.field "player" Data.playerDecoder
        , Decode.succeed SyncTableState
            |> Decode.andMap (Decode.field "table" Data.tableWithGameDecoder)
            |> Decode.andMap nextGameName
            |> eventField "SyncTableState" identity
        , eventField "GameStarted" GameStarted <| Decode.field "game" Data.gameDecoder
        , eventField "VoteAccepted" VoteAccepted <| Decode.field "player" Data.playerDecoder
        , Decode.succeed VotingEnded
            |> Decode.andMap (Decode.field "game" Data.gameDecoder)
            |> Decode.andMap nextGameName
            |> eventField "VotingEnded" identity
        , eventField "GameEnded" GameEnded <| Decode.field "game" Data.gameDecoder
        , eventField "PlayerKicked" PlayerKicked <| Decode.field "player" Data.playerDecoder
        ]


observe : (Result StreamError Event -> msg) -> Sub msg
observe msg =
    Sub.batch
        [ observe_ <|
            msg
                << Result.mapError DecodingError
                << Decode.decodeString eventDecoder
        , streamError <|
            msg
                << Err
                << Maybe.withDefault Disconnected
                << streamErrorDecoder
        ]



-- Msg


type Msg
    = NewGame String
    | Vote Vote
    | FinishRound
    | NextGame String Vote
    | Finish Vote
    | Restart
    | KickPlayer Player


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        NewGame name ->
            Encode.object
                [ ( "msg", Encode.string "NewGame" )
                , ( "name", Encode.string name )
                ]

        Vote vote ->
            Encode.object
                [ ( "msg", Encode.string "Vote" )
                , ( "vote", Data.encodeVote vote )
                ]

        FinishRound ->
            Encode.object
                [ ( "msg", Encode.string "FinishRound" ) ]

        NextGame name vote ->
            Encode.object
                [ ( "msg", Encode.string "NextRound" )
                , ( "name", Encode.string name )
                , ( "vote", Data.encodeVote vote )
                ]

        Finish vote ->
            Encode.object
                [ ( "msg", Encode.string "FinishGame" )
                , ( "vote", Data.encodeVote vote )
                ]

        Restart ->
            Encode.object
                [ ( "msg", Encode.string "RestartRound" ) ]

        KickPlayer { id } ->
            Encode.object
                [ ( "msg", Encode.string "KickPlayer" )
                , ( "id", Encode.int id )
                ]


sendMsg : Msg -> Cmd msg
sendMsg =
    emit << Encode.encode 0 << encodeMsg
