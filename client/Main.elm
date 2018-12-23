port module Main exposing (main, routePage)

import Authorize exposing (Authorize(..))
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Css
import Data exposing (Session)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Http
import Page.Join as Join
import Page.Table as Table
import Router exposing (Route)
import Theme
import Url exposing (Url)


port storeSession : String -> Cmd msg



-- Model


type Page
    = Home Join.Model
    | Table Table.Model
    | JoinTable String Join.Model
    | NotFound


type alias Model =
    { navigationKey : Key
    , page : Authorize Page Session
    , route : Route
    }


type alias Flags =
    { sessionId : Maybe String
    }


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
                    Tuple.mapFirst Home Join.init
                        |> Tuple.mapSecond (Cmd.map HomeMsg)

                Router.Table id ->
                    Table.init session.id id
                        |> Tuple.mapFirst Table
                        |> Tuple.mapSecond (Cmd.map TableMsg)

                Router.JoinTable id ->
                    Tuple.mapFirst (JoinTable id) Join.init
                        |> Tuple.mapSecond (Cmd.map JoinTableMsg)

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
    ( { page = Authorize.init
      , route = Router.route url
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
    | HomeMsg Join.Msg
    | TableMsg Table.Msg
    | JoinTableMsg Join.Msg


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

                External url_ ->
                    ( model, Navigation.load url_ )

        UrlChanged url ->
            routePage Authorize.for (Router.route url) model

        SessionCreated res ->
            case res of
                Ok session ->
                    routePage (Authorize.by session) model.route model
                        |> Cmd.add (storeSession session.id)

                Err session ->
                    ( model, Data.createSession SessionCreated )

        HomeMsg sMsg ->
            case model.page of
                Authorized session (Home m) ->
                    Join.update Data.createTable model.navigationKey session sMsg m
                        |> Tuple.mapFirst (\a -> { model | page = Authorized session <| Home a })
                        |> Tuple.mapSecond (Cmd.map HomeMsg)

                _ ->
                    ( model, Cmd.none )

        TableMsg sMsg ->
            case model.page of
                Authorized session (Table m) ->
                    Table.update model.navigationKey sMsg m
                        |> Tuple.mapFirst (\a -> { model | page = Authorized session <| Table a })
                        |> Tuple.mapSecond (Cmd.map TableMsg)

                _ ->
                    ( model, Cmd.none )

        JoinTableMsg sMsg ->
            case model.page of
                Authorized session (JoinTable id m) ->
                    Join.update (Data.joinTable id) model.navigationKey session sMsg m
                        |> Tuple.mapFirst (\a -> { model | page = Authorized session <| JoinTable id a })
                        |> Tuple.mapSecond (Cmd.map JoinTableMsg)

                _ ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Authorized session (Table m) ->
            Table.subscriptions m
                |> Sub.map TableMsg

        _ ->
            Sub.none



-- View


withLayout : Html Msg -> List (Html Msg)
withLayout inner =
    [ Html.styled Html.div
        [ Css.width <| Css.px 1200
        , Css.margin2 Css.zero Css.auto
        ]
        []
        [ Theme.logo
        , inner
        , Html.styled Html.div [ Css.property "clear" "both" ] [] []
        , Html.styled Html.footer
            [ Css.position Css.absolute
            , Css.bottom Css.zero
            , Css.left Css.zero
            , Css.width <| Css.pct 100
            , Css.textAlign Css.center
            , Css.lineHeight <| Css.em 1.5
            , Css.fontSize <| Css.px 14
            ]
            []
            [ Html.p []
                [ Html.text "This work contains "
                , Html.strong [] [ Html.text "derivation" ]
                , Html.text " of "
                , Html.a [ Attrs.href "https://github.com/redbooth/Scrum-poker-cards" ] [ Html.text "Scrum-poker-cards" ]
                , Html.text " by "
                , Html.a [ Attrs.href "https://redbooth.com/" ] [ Html.text "redbooth" ]
                , Html.text " used under "
                , Html.a [ Attrs.href "https://creativecommons.org/licenses/by/3.0/" ] [ Html.text "CC BY 3.0" ]
                , Html.br [] []
                , Html.text "Agile Poker is Free Software released under "
                , Html.a [ Attrs.href "https://www.gnu.org/licenses/agpl-3.0.en.html" ] [ Html.text "AGPLv3 license" ]
                , Html.br [] []
                , Html.text "Souce code is available on "
                , Html.a [ Attrs.href "https://github.com/turboMaCk/agile-poker" ] [ Html.text "GitHub" ]
                ]
            , Html.p []
                [ Html.text "Developed & designed by Marek Fajkus"
                , Html.br [] []
                , Html.a [ Attrs.href "https://github.com/turboMaCk/agile-poker/issues" ] [ Html.text "Report issue or request feature" ]
                ]
            ]
        , Theme.globalStyles
        ]
    ]


joinTitle : String -> Html msg
joinTitle txt =
    Html.styled Html.h2
        [ Theme.heading ]
        []
        [ Html.text txt ]


document : Model -> Document Msg
document model =
    let
        ( title, body ) =
            case model.page of
                Authorized _ page ->
                    case page of
                        Home m ->
                            ( "Agile Poker"
                            , withLayout <|
                                Html.map HomeMsg <|
                                    Join.view (joinTitle "Create Table") m
                            )

                        Table m ->
                            ( "Table | Agile Poker"
                            , withLayout <| Html.map TableMsg <| Table.view m
                            )

                        JoinTable _ m ->
                            ( "Join | Agile Poker"
                            , withLayout <| Html.map JoinTableMsg <| Join.view (joinTitle "Join Table") m
                            )

                        NotFound ->
                            ( "404 | Agile Poker"
                            , withLayout <| Html.text "404"
                            )

                Unauthorized _ ->
                    -- @TODO: error handling?
                    ( "Authorizing | Agile Poker"
                    , []
                    )
    in
    { title = title
    , body = List.map Html.toUnstyled body
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = RouteTo
        , onUrlChange = UrlChanged
        }
