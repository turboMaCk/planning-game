port module Stream exposing (Event(..), connect, disconnect, observe)

import Data exposing (User)
import Json.Decode as Decode exposing (Decoder)


port observe_ : (String -> msg) -> Sub msg


port connect : String -> Cmd msg


port disconnect : () -> Cmd msg


type Event
    = UserJoin User
    | UserLeft String


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
        , eventField "UserLeft" UserLeft Decode.string
        ]


observe : (Result Decode.Error Event -> msg) -> Sub msg
observe msg =
    observe_ <| msg << Decode.decodeString eventDecoder
