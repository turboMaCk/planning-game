port module Stream exposing (Event(..), StreamError(..), connect, disconnect, observe)

import Data exposing (Player)
import Json.Decode as Decode exposing (Decoder)


port observe_ : (String -> msg) -> Sub msg


port streamError : (String -> msg) -> Sub msg


port connect : String -> Cmd msg


port disconnect : () -> Cmd msg


type StreamError
    = CantConnect
    | Disconnected
    | DecodingError Decode.Error


streamErrorDecoder : String -> Maybe StreamError
streamErrorDecoder str =
    let
        result (val, c) acc =
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
    | UserStatusUpdate Player


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
    Decode.oneOf
        [ eventField "UserJoined" PlayerJoin (Decode.field "player" Data.playerDecoder)
        , eventField "UserStatusUpdate" UserStatusUpdate (Decode.field "player" Data.playerDecoder)
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
