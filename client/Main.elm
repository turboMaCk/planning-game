port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Data exposing (Session, User)
import Home
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Router exposing (Route)
import Stream exposing (Event(..), StreamError(..))
import Task
import Url exposing (Url)


port storeSession : String -> Cmd msg



-- Model


type alias GameModel =
    { players : List User
    }


type Page
    = Home Home.Model
    | Table GameModel
    | NotFound


type Authorize a b
    = Authorized b a
    | Unauthorized (Maybe Http.Error)



{- @TODO:
   Rethink route page and authorize so it's easier to work with
-}


{-| flip Authorized
-}
authorize : a -> b -> Authorize a b
authorize a b =
    Authorized b a


type alias Model =
    { navigationKey : Key
    , page : Authorize Page Session
    , route : Route
    , userName : String -- @TODO
    , sessionId : Result Http.Error (Maybe String)
    }


type alias Flags =
    { sessionId : Maybe String
    }



-- @TODO: this mapping msgs


routePage : Route -> ( Page, Cmd Msg )
routePage route =
    case route of
        Router.Home ->
            Tuple.mapFirst Home Home.init

        Router.Table sId ->
            ( Table { players = [] }
            , Cmd.batch
                [ Ok sId
                    |> Result.map Stream.connect
                    |> Result.withDefault Cmd.none
                ]
            )

        Router.NotFound ->
            ( NotFound, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { sessionId } url key =
    ( { page = Unauthorized Nothing
      , route = Router.route identity url
      , userName = ""
      , sessionId = Ok sessionId
      , navigationKey = key
      }
    , Maybe.map (Data.getSession SessionCreated) sessionId
        |> Maybe.withDefault (Data.createSession SessionCreated)
    )



-- Update


type Msg
    = NoOp
    | RouteTo UrlRequest
    | UpdateName String
    | SubmitName
    | SessionCreated (Result Http.Error Session)
    | StreamEvent (Result StreamError Event)
    | ClearSession
    | HomeMsg Home.Msg


updateUser : User -> List User -> List User
updateUser user =
    List.map
        (\u ->
            if u.name == user.name then
                user

            else
                u
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RouteTo url ->
            case url of
                Internal location ->
                    ( model
                    , Navigation.pushUrl model.navigationKey <| Url.toString location
                    )

                External _ ->
                    ( model, Cmd.none )

        UpdateName str ->
            ( { model | userName = str }
            , Cmd.none
            )

        SubmitName ->
            ( model
            , Cmd.none
            )

        SessionCreated res ->
            let
                ( page, cmd ) =
                    routePage model.route
            in
            -- @TODO: crap
            Result.map (\s -> ( authorize page s, s.id )) res
                |> Result.map
                    (\( p, t ) ->
                        ( { model | page = p }
                        , Cmd.batch
                            [ cmd
                            , storeSession t
                            ]
                        )
                    )
                -- this cycles!
                |> Result.withDefault ( model, Data.createSession SessionCreated )

        ClearSession ->
            ( { model
                | sessionId = Ok Nothing

                -- , page = SetName
              }
            , Stream.disconnect ()
            )

        HomeMsg sMsg ->
            case model.page of
                Authorized session (Home m) ->
                    Home.update model.navigationKey session sMsg m
                        |> Tuple.mapFirst (\a -> { model | page = Authorized session <| Home a })
                        |> Tuple.mapSecond (Cmd.map HomeMsg)

                _ ->
                    ( model, Cmd.none )

        StreamEvent result ->
            case model.page of
                Authorized session (Table { players }) ->
                    case result of
                        Ok event ->
                            case event of
                                UserJoin newUser ->
                                    ( { model
                                        | page = Authorized session <| Table { players = newUser :: players }
                                      }
                                    , Cmd.none
                                    )

                                UserStatusUpdate user ->
                                    ( { model
                                        | page = Authorized session <| Table { players = updateUser user players }
                                      }
                                    , Cmd.none
                                    )

                        Err err ->
                            ( model
                            , case err of
                                CantConnect ->
                                    -- @TODO: replace with cmd-extra?
                                    Task.perform identity <| Task.succeed ClearSession

                                Disconnected ->
                                    Cmd.none

                                DecodingError dErr ->
                                    let
                                        _ =
                                            Debug.log "err" dErr
                                    in
                                    Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sessionId of
        Ok (Just _) ->
            Stream.observe StreamEvent

        _ ->
            Sub.none



-- View


viewUser : User -> Html msg
viewUser { name, isConnected } =
    Html.li []
        [ Html.span []
            [ if isConnected then
                Html.text "o"

              else
                Html.text "x"
            ]
        , Html.text " "
        , Html.text name
        ]


gameView : String -> GameModel -> List (Html Msg)
gameView userName { players } =
    [ Html.aside []
        [ Html.ul [] <|
            List.map viewUser players
        ]
    , Html.aside []
        [ Html.text userName ]
    ]


view : Model -> Document Msg
view model =
    let
        ( title, body ) =
            case model.page of
                Authorized _ page ->
                    case page of
                        Home m ->
                            ( "Agile Poker"
                            , [ Html.map HomeMsg <| Home.view m ]
                            )

                        Table m ->
                            ( "Table | Agile Poker"
                            , gameView model.userName m
                            )

                        NotFound ->
                            ( "404 | Agile Poker"
                            , [ Html.text "404" ]
                            )

                Unauthorized _ ->
                    -- @TODO: error handling?
                    ( "Authorizing | Agile Poker"
                    , []
                    )
    in
    { title = title
    , body = body
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = RouteTo
        , onUrlChange = always NoOp
        }
