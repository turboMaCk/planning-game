module Data exposing
    ( Session
    , User
    , createSession
    , createTable
    , join
    , userDecoder
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Url.Builder as Url



-- Session


type alias Session =
    { id : String }


sessionDecoder : Decoder Session
sessionDecoder =
    Decode.succeed Session
        |> Decode.andMap (Decode.field "id" Decode.string)


createSession : (Result Http.Error Session -> msg) -> Cmd msg
createSession msg =
    Http.post
        { url = Url.absolute [ "session" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg sessionDecoder
        }



-- User


type alias User =
    { name : String
    , isConnected : Bool
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "connected" Decode.bool)



-- Table


createTable : String -> (Result Http.Error String -> msg) -> String -> Cmd msg
createTable token msg name =
    Http.request
        { method = "POST"
        , url = Url.absolute [ "tables" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = Http.expectJson msg <| Decode.succeed "aaa"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , timeout = Nothing
        , tracker = Nothing
        }



-- @TODO: fix


join : String -> (Result Http.Error String -> msg) -> String -> Cmd msg
join id msg name =
    Http.post
        { url = Url.absolute [ "tables", id, "join" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = Http.expectJson msg Decode.string
        }
