module Data exposing
    ( Session
    , Table
    , User
    , createSession
    , createTable
    , getMe
    , getSession
    , joinTable
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


getSession : (Result Http.Error Session -> msg) -> String -> Cmd msg
getSession msg token =
    Http.request
        { method = "GET"
        , url = Url.absolute [ "session" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg sessionDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , timeout = Nothing
        , tracker = Nothing
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


getMe : String -> String -> (Result Http.Error User -> msg) -> Cmd msg
getMe token tableId msg =
    Http.request
        { method = "GET"
        , url = Url.absolute [ "tables", tableId, "me" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg userDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , timeout = Nothing
        , tracker = Nothing
        }



-- Table


type alias Table =
    { id : String
    , banker : User
    , players : List User
    }


tableDecoder : Decoder Table
tableDecoder =
    Decode.succeed Table
        |> Decode.andMap (Decode.field "id" Decode.string)
        |> Decode.andMap (Decode.field "banker" userDecoder)
        |> Decode.andMap (Decode.field "players" <| Decode.list userDecoder)


createTable : Session -> (Result Http.Error Table -> msg) -> String -> Cmd msg
createTable session msg name =
    Http.request
        { method = "POST"
        , url = Url.absolute [ "tables" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = Http.expectJson msg tableDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ session.id ]
        , timeout = Nothing
        , tracker = Nothing
        }


joinTable : String -> Session -> (Result Http.Error Table -> msg) -> String -> Cmd msg
joinTable id session msg name =
    Http.request
        { method = "POST"
        , url = Url.absolute [ "tables", id, "join" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = Http.expectJson msg tableDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ session.id ]
        , timeout = Nothing
        , tracker = Nothing
        }
