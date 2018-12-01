module Data exposing
    ( ApiError
    , Player
    , Session
    , Table
    , TableError(..)
    , createSession
    , createTable
    , errorIs
    , errorMessage
    , getMe
    , getSession
    , joinTable
    , playerDecoder
    )

import Http exposing (Expect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Url.Builder as Url



-- Error Handling


type alias ErrStructure a =
    { error : a
    , message : String
    }


type ApiError a
    = KnownErr (ErrStructure a)
    | HttpErr Http.Error


errorIs : a -> ApiError a -> Bool
errorIs a apiErr =
    case apiErr of
        KnownErr { error } ->
            a == error

        _ ->
            False


errorMessage : ApiError a -> String
errorMessage apiErr =
    case apiErr of
        KnownErr { message } ->
            message

        HttpErr (Http.BadUrl _) ->
            "Bad URL"

        HttpErr Http.Timeout ->
            "Reuqest timeout"

        HttpErr Http.NetworkError ->
            "Can't connect to the server"

        HttpErr (Http.BadStatus int) ->
            "Server respond with status " ++ String.fromInt int

        HttpErr (Http.BadBody str) ->
            str


expectJson : (Result (ApiError e) a -> msg) -> Decoder e -> Decoder a -> Expect msg
expectJson toMsg errorDecoder decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| HttpErr <| Http.BadUrl url

                Http.Timeout_ ->
                    Err <| HttpErr <| Http.Timeout

                Http.NetworkError_ ->
                    Err <| HttpErr <| Http.NetworkError

                Http.BadStatus_ metadata body ->
                    let
                        fullErrorDecoder =
                            Decode.succeed ErrStructure
                                |> Decode.andMap (Decode.field "error" errorDecoder)
                                |> Decode.andMap (Decode.field "message" Decode.string)
                    in
                    case Decode.decodeString fullErrorDecoder body of
                        Ok value ->
                            Err <| KnownErr value

                        Err err ->
                            Err <| HttpErr <| Http.BadStatus metadata.statusCode

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| HttpErr <| Http.BadBody <| Decode.errorToString err



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



-- Table


type alias Table =
    { id : String
    , banker : Player
    , players : List Player
    }


tableDecoder : Decoder Table
tableDecoder =
    Decode.succeed Table
        |> Decode.andMap (Decode.field "id" Decode.string)
        |> Decode.andMap (Decode.field "banker" playerDecoder)
        |> Decode.andMap (Decode.field "players" <| Decode.list playerDecoder)


type TableError
    = TableNotFound
    | NameTaken
    | PlayerNotFound


tableErrorDecoder : Decoder TableError
tableErrorDecoder =
    let
        fromString str =
            case str of
                "NotFound" ->
                    Decode.succeed TableNotFound

                "Conflict" ->
                    Decode.succeed NameTaken

                "Forbidden" ->
                    Decode.succeed PlayerNotFound

                val ->
                    Decode.fail <| "Unknown status " ++ val
    in
    Decode.string
        |> Decode.andThen fromString


createTable : Session -> (Result (ApiError TableError) Table -> msg) -> String -> Cmd msg
createTable session msg name =
    Http.request
        { method = "POST"
        , url = Url.absolute [ "tables" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = expectJson msg tableErrorDecoder tableDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ session.id ]
        , timeout = Nothing
        , tracker = Nothing
        }


joinTable : String -> Session -> (Result (ApiError TableError) Table -> msg) -> String -> Cmd msg
joinTable id session msg name =
    Http.request
        { method = "POST"
        , url = Url.absolute [ "tables", id, "join" ] []
        , body = Http.jsonBody <| Encode.object [ ( "name", Encode.string name ) ]
        , expect = expectJson msg tableErrorDecoder tableDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ session.id ]
        , timeout = Nothing
        , tracker = Nothing
        }



-- Player


type alias Player =
    { name : String
    , isConnected : Bool
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "connected" Decode.bool)


getMe : String -> String -> (Result (ApiError TableError) Player -> msg) -> Cmd msg
getMe token tableId msg =
    Http.request
        { method = "GET"
        , url = Url.absolute [ "tables", tableId, "me" ] []
        , body = Http.emptyBody
        , expect = expectJson msg tableErrorDecoder playerDecoder
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , timeout = Nothing
        , tracker = Nothing
        }
