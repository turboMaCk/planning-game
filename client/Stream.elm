port module Stream exposing (Event(..), StreamError(..), connect, disconnect, observe)

import Data exposing (User)
import Json.Decode as Decode exposing (Decoder)


port observe_ : (String -> msg) -> Sub msg


port streamError : (() -> msg) -> Sub msg


port connect : String -> Cmd msg


port disconnect : () -> Cmd msg


type StreamError
    = SocketError
    | DecodingError Decode.Error


type Event
    = UserJoin User
    | UserStatusUpdate User


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
        [ eventField "UserJoined" UserJoin Data.userDecoder
        , eventField "UserStatusUpdate" UserStatusUpdate Data.userDecoder
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
                << always SocketError
        ]
