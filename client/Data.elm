module Data exposing (User, setName, userDecoder, getCurrentUser)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode


type alias User =
    { name : String
    , isConnected : Bool
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "connected" Decode.bool)


setName : (Result Http.Error String -> msg) -> String -> Cmd msg
setName msg name =
    Http.post
        { url = "http://localhost:3000/join"
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = Http.expectJson msg Decode.string
        }


getCurrentUser : String -> (Result Http.Error User -> msg) -> Cmd msg
getCurrentUser token msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , url = "/session"
        , body = Http.emptyBody
        , expect = Http.expectJson msg userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
