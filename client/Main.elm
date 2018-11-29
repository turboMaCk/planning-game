port module Main exposing (main, routePage)

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
import Table
import Task
import Url exposing (Url)


port storeSession : String -> Cmd msg



-- Model


type Page
    = Home Home.Model
    | Table Table.Model
    | NotFound


type Authorize a b
    = Authorized b a
    | Unauthorized (Maybe Http.Error)



{- @TODO:
   Rethink route page and authorize so it's easier to work with
-}


authorize : b -> (b -> c) -> Authorize a b -> Authorize c b
authorize b f auth =
    Authorized b <| f b


forAuthorized : (b -> c) -> Authorize a b -> Authorize c b
forAuthorized f auth =
    case auth of
        Unauthorized err ->
            Unauthorized err

        Authorized b _ ->
            Authorized b <| f b


type alias Model =
    { navigationKey : Key
    , page : Authorize Page Session
    , route : Route
    }


type alias Flags =
    { sessionId : Maybe String
    }



-- @TODO: this mapping msgs


routePage :
    ((Session -> ( Page, Cmd Msg ))
     -> Authorize Page Session
     -> Authorize ( Page, Cmd msg ) Session
    )
    -> Router.Route
    -> Model
    -> ( Model, Cmd msg )
routePage authorizeF route model =
    let
        genPage session =
            case route of
                Router.Home ->
                    Tuple.mapFirst Home Home.init

                Router.Table id ->
                    Table.init session.id id
                        |> Tuple.mapFirst Table
                        |> Tuple.mapSecond (Cmd.map TableMsg)

                -- ( Table { players = [] }
                -- , Cmd.batch
                --     [ Ok sId
                --         |> Result.map Stream.connect
                --         |> Result.withDefault Cmd.none
                --     ]
                -- )
                Router.NotFound ->
                    ( NotFound, Cmd.none )

        newAuthorized =
            authorizeF genPage model.page
    in
    case newAuthorized of
        Authorized ses ( page, cmd ) ->
            ( { model
                | page = Authorized ses page
                , route = route
              }
            , cmd
            )

        Unauthorized _ ->
            ( model, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { sessionId } url key =
    ( { page = Unauthorized Nothing
      , route = Router.route identity url
      , navigationKey = key
      }
    , Maybe.map (Data.getSession SessionCreated) sessionId
        |> Maybe.withDefault (Data.createSession SessionCreated)
    )



-- Update


type Msg
    = NoOp
    | RouteTo UrlRequest
    | UrlChanged Url
    | SessionCreated (Result Http.Error Session)
    | HomeMsg Home.Msg
    | TableMsg Table.Msg


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

        UrlChanged url ->
            let
                route =
                    -- @TODO this is silly
                    Router.route identity url
            in
            routePage forAuthorized route model

        SessionCreated res ->
            case res of
                Ok session ->
                    routePage (authorize session) model.route model

                Err session ->
                    ( model, Data.createSession SessionCreated )

        HomeMsg sMsg ->
            case model.page of
                Authorized session (Home m) ->
                    Home.update model.navigationKey session sMsg m
                        |> Tuple.mapFirst (\a -> { model | page = Authorized session <| Home a })
                        |> Tuple.mapSecond (Cmd.map HomeMsg)

                _ ->
                    ( model, Cmd.none )

        TableMsg _ ->
            ( model, Cmd.none )



-- StreamEvent result ->
--     case model.page of
--         Authorized session (Table { players }) ->
--             case result of
--                 Ok event ->
--                     case event of
--                         UserJoin newUser ->
--                             ( { model
--                                 | page = Authorized session <| Table { players = newUser :: players }
--                               }
--                             , Cmd.none
--                             )
--                         UserStatusUpdate user ->
--                             ( { model
--                                 | page = Authorized session <| Table { players = updateUser user players }
--                               }
--                             , Cmd.none
--                             )
--                 Err err ->
--                     ( model
--                     , case err of
--                         CantConnect ->
--                             -- @TODO: replace with cmd-extra?
--                             Task.perform identity <| Task.succeed ClearSession
--                         Disconnected ->
--                             Cmd.none
--                         DecodingError dErr ->
--                             let
--                                 _ =
--                                     Debug.log "err" dErr
--                             in
--                             Cmd.none
--                     )
--         _ ->
--             ( model, Cmd.none )
-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


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
                            , [ Html.text "table" ]
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
        , onUrlChange = UrlChanged
        }
