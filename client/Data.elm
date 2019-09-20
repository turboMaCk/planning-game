module Data exposing
    ( ApiError
    , Game(..)
    , Player
    , PlayerStatus(..)
    , Session
    , Table
    , TableError(..)
    , Vote(..)
    , createSession
    , createTable
    , encodeVote
    , errorIs
    , errorMessage
    , gameDecoder
    , getMe
    , getSession
    , isNewGame
    , isRoundFinished
    , isVoting
    , joinTable
    , playerDecoder
    , tableDecoder
    , tableWithGameDecoder
    , voteDecoder
    , voteToInt
    )

import Dict exposing (Dict)
import Http exposing (Expect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)
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

                        Err _ ->
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
    , dealer : Player
    , players : List Player
    }


tableDecoder : Decoder Table
tableDecoder =
    Decode.succeed Table
        |> Decode.andMap (Decode.field "id" Decode.string)
        |> Decode.andMap (Decode.field "dealer" playerDecoder)
        |> Decode.andMap (Decode.field "players" <| Decode.list playerDecoder)


tableWithGameDecoder : Decoder ( Table, Game )
tableWithGameDecoder =
    Decode.succeed Tuple.pair
        |> Decode.andMap tableDecoder
        |> Decode.andMap
            (Decode.field "game" <|
                Decode.map (Maybe.withDefault NotStarted) <|
                    Decode.maybe gameDecoder
            )


type TableError
    = TableNotFound
    | PlayerNotFound
    | PlayerNameTaken
    | PlayerNameEmpty
    | GameFinished
    | GameVotingEnded


tableErrorDecoder : Decoder TableError
tableErrorDecoder =
    let
        fromString str =
            case str of
                "TableNotFound" ->
                    Decode.succeed TableNotFound

                "PlayerNotFound" ->
                    Decode.succeed PlayerNotFound

                "PlayerError:NameTaken" ->
                    Decode.succeed PlayerNameTaken

                "PlayerError:NameEmpty" ->
                    Decode.succeed PlayerNameEmpty

                "Game:GameFinished" ->
                    Decode.succeed GameFinished

                "Game:VotingEnded" ->
                    Decode.succeed GameVotingEnded

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
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "isActive", Encode.bool True )
                    ]
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


type PlayerStatus
    = Active
    | Idle


playerStatusDecoder : Decoder PlayerStatus
playerStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "active" ->
                        Decode.succeed Active

                    "idle" ->
                        Decode.succeed Idle

                    _ ->
                        Decode.fail <| "Unknown status " ++ s
            )


type alias Player =
    { id : Int
    , name : String
    , isConnected : Bool
    , status : PlayerStatus
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> Decode.andMap (Decode.field "id" Decode.int)
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "connected" Decode.bool)
        |> Decode.andMap (Decode.field "status" playerStatusDecoder)


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



-- Vote


type Vote
    = OnePoint
    | TwoPoints
    | ThreePoints
    | FivePoints
    | EightPoints
    | ThreeteenPoints
    | TwentyPoints
    | FortyPoints
    | HundredPoints
    | InfinityPoints


voteToInt : Vote -> Int
voteToInt vote =
    case vote of
        OnePoint ->
            1

        TwoPoints ->
            2

        ThreePoints ->
            3

        FivePoints ->
            5

        EightPoints ->
            8

        ThreeteenPoints ->
            13

        TwentyPoints ->
            20

        FortyPoints ->
            40

        HundredPoints ->
            100

        InfinityPoints ->
            0


voteDecoder : Decoder Vote
voteDecoder =
    let
        toVote str =
            case str of
                "1" ->
                    Decode.succeed OnePoint

                "2" ->
                    Decode.succeed TwoPoints

                "3" ->
                    Decode.succeed ThreePoints

                "5" ->
                    Decode.succeed FivePoints

                "8" ->
                    Decode.succeed EightPoints

                "20" ->
                    Decode.succeed TwentyPoints

                "13" ->
                    Decode.succeed ThreeteenPoints

                "40" ->
                    Decode.succeed FortyPoints

                "100" ->
                    Decode.succeed HundredPoints

                "Infinity" ->
                    Decode.succeed InfinityPoints

                _ ->
                    Decode.fail "unknown vote"
    in
    Decode.andThen toVote Decode.string


encodeVote : Vote -> Value
encodeVote vote =
    let
        str =
            case vote of
                OnePoint ->
                    "1"

                TwoPoints ->
                    "2"

                ThreePoints ->
                    "3"

                FivePoints ->
                    "5"

                EightPoints ->
                    "8"

                ThreeteenPoints ->
                    "13"

                TwentyPoints ->
                    "20"

                FortyPoints ->
                    "40"

                HundredPoints ->
                    "100"

                InfinityPoints ->
                    "Infinity"
    in
    Encode.string str



-- Game


type Game
    = NotStarted
    | Voting
        { name : String
        , maskedVotes : Set Int
        , totalPoints : Int
        }
    | RoundFinished
        { name : String
        , playerVotes : Dict Int Vote
        , totalPoints : Int
        }
    | Overview
        { totalPoints : Int
        , playerVotes : List ( String, Dict Int Vote )
        , results : Dict String Vote
        }


isNewGame : Game -> Bool
isNewGame game =
    case game of
        NotStarted ->
            True

        _ ->
            False


isVoting : Game -> Bool
isVoting game =
    case game of
        Voting _ ->
            True

        _ ->
            False


isRoundFinished : Game -> Bool
isRoundFinished game =
    case game of
        RoundFinished _ ->
            True

        _ ->
            False


playerPointsDictDecoder : Decoder (Dict Int Vote)
playerPointsDictDecoder =
    let
        itemDecoder =
            Decode.succeed Tuple.pair
                |> Decode.andMap (Decode.field "id" Decode.int)
                |> Decode.andMap (Decode.field "value" voteDecoder)
    in
    Decode.list itemDecoder
        |> Decode.map Dict.fromList


roundPointsDictDecoder : Decoder (Dict String Vote)
roundPointsDictDecoder =
    let
        itemDecoder =
            Decode.succeed Tuple.pair
                |> Decode.andMap (Decode.field "name" Decode.string)
                |> Decode.andMap (Decode.field "value" voteDecoder)
    in
    Decode.list itemDecoder
        |> Decode.map Dict.fromList


votingDecoder : Decoder Game
votingDecoder =
    Decode.succeed (\n v t -> { name = n, maskedVotes = v, totalPoints = t })
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "maskedVotes" <| Decode.map Set.fromList <| Decode.list Decode.int)
        |> Decode.andMap (Decode.field "points" Decode.int)
        |> Decode.map Voting


lockedDecoder : Decoder Game
lockedDecoder =
    Decode.succeed (\n v t -> { name = n, playerVotes = v, totalPoints = t })
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "playerVotes" playerPointsDictDecoder)
        |> Decode.andMap (Decode.field "points" Decode.int)
        |> Decode.map RoundFinished


playerVotesDecoder : Decoder ( String, Dict Int Vote )
playerVotesDecoder =
    let
        votesDecoder =
            Decode.succeed Tuple.pair
                |> Decode.andMap (Decode.field "id" Decode.int)
                |> Decode.andMap (Decode.field "value" voteDecoder)
    in
    Decode.succeed Tuple.pair
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap
            (Decode.field "playerVotes"
                (Decode.map Dict.fromList <| Decode.list votesDecoder)
            )


overviewDecoder : Decoder Game
overviewDecoder =
    Decode.succeed (\r p t -> { results = r, playerVotes = p, totalPoints = t })
        |> Decode.andMap (Decode.field "roundVotes" roundPointsDictDecoder)
        |> Decode.andMap (Decode.field "playerVotes" <| Decode.list playerVotesDecoder)
        |> Decode.andMap (Decode.field "points" Decode.int)
        |> Decode.map Overview


gameDecoder : Decoder Game
gameDecoder =
    let
        choose str =
            case str of
                "Running" ->
                    votingDecoder

                "Locked" ->
                    lockedDecoder

                "Finished" ->
                    overviewDecoder

                _ ->
                    Decode.fail "unknown status"
    in
    Decode.field "status" Decode.string
        |> Decode.andThen choose
