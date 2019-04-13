port module Main exposing (main, routePage)

import Authorize exposing (Authorize(..))
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Css exposing (Style)
import Data exposing (Session)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Page.Join as Join
import Page.Table as Table
import Router exposing (Route)
import Theme
import Url exposing (Url)


port storeSession : String -> Cmd msg


port cookiesNoticeConfirmed : () -> Cmd msg



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
    , showCookiesNotice : Bool
    }


type alias Flags =
    { sessionId : Maybe String
    , showCookiesNotice : Bool
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
        Authorized ses ( page, cmd_ ) ->
            let
                cmd =
                    case model.route of
                        Router.Table _ ->
                            Cmd.batch
                                [ cmd_
                                , Table.leave ()
                                ]

                        _ ->
                            cmd_
            in
            ( { model
                | page = Authorized ses page
                , route = route
              }
            , cmd
            )

        Unauthorized _ ->
            ( model, Cmd.none )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { sessionId, showCookiesNotice } url key =
    ( { page = Authorize.init
      , route = Router.route url
      , navigationKey = key
      , showCookiesNotice = showCookiesNotice
      }
    , Maybe.map (Data.getSession SessionCreated) sessionId
        |> Maybe.withDefault (Data.createSession SessionCreated)
    )



-- Update


type Msg
    = RouteTo UrlRequest
    | UrlChanged Url
    | SessionCreated (Result Http.Error Session)
    | HomeMsg Join.Msg
    | TableMsg Table.Msg
    | JoinTableMsg Join.Msg
    | ConfirmCookiesNotice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteTo url ->
            case url of
                Internal location ->
                    ( model
                    , Navigation.pushUrl model.navigationKey <| Url.toString location
                    )

                External url_ ->
                    ( model
                    , if String.isEmpty url_ then
                        Cmd.none

                      else
                        Navigation.load url_
                    )

        UrlChanged url ->
            routePage Authorize.for (Router.route url) model

        SessionCreated res ->
            case res of
                Ok session ->
                    routePage (Authorize.by session) model.route model
                        |> Cmd.add (storeSession session.id)

                Err _ ->
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

        ConfirmCookiesNotice ->
            ( { model | showCookiesNotice = False }
            , cookiesNoticeConfirmed ()
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Authorized _ (Table m) ->
            Table.subscriptions m
                |> Sub.map TableMsg

        _ ->
            Sub.none



-- View


container : Style
container =
    Css.batch
        [ Css.width <| Css.px 1100
        , Css.margin2 Css.zero Css.auto
        , Css.padding2 Css.zero <| Css.px 16
        ]


viewCookiesNotice : Html Msg
viewCookiesNotice =
    Html.styled Html.div
        [ Css.position Css.fixed
        , Css.left Css.zero
        , Css.right Css.zero
        , Css.top Css.zero
        , Css.backgroundColor <| Css.hex "f5eeb1"
        , Css.color <| Css.hex "4f4e20"
        , Css.padding2 (Css.px 20) Css.zero
        , Css.zIndex <| Css.int 1
        , Css.fontSize <| Css.px 14
        , Css.lineHeight <| Css.px 25
        ]
        []
        [ Html.styled Html.div
            [ container ]
            []
            [ Html.text "This website is using cookies to persist anonymous identifiers of sessions to provide functionality. "
            , Html.a [ Attrs.href "https://github.com/turboMaCk/planning-game/blob/master/docs/COOKIES.md" ] [ Html.text "Learn More" ]
            , Html.styled Html.button
                [ Css.marginLeft <| Css.px 12
                , Css.float Css.right
                , Css.padding2 (Css.px 3) <| Css.px 12
                , Css.color <| Css.hex "4f4e20"
                , Css.border3 (Css.px 0.5) Css.solid <| Css.hex "bab869"
                , Css.backgroundColor <| Css.hex "ffffff"
                , Css.borderRadius <| Css.px 2
                , Css.property "background-image" "linear-gradient(#f3ee98, #d8d76c)"
                ]
                [ Events.onClick ConfirmCookiesNotice ]
                [ Html.text "Agree and close" ]
            ]
        ]


withLayout : Bool -> Html Msg -> List (Html Msg)
withLayout showCookiesNotice inner =
    [ if showCookiesNotice then
        viewCookiesNotice

      else
        Html.text ""
    , Html.styled Html.div
        [ container
        , if showCookiesNotice then
            Css.marginTop <| Css.px 100

          else
            Css.marginTop <| Css.px 35
        ]
        []
        [ Theme.logo
        , inner
        , Html.styled Html.footer
            [ Css.position Css.absolute
            , Css.bottom Css.zero
            , Css.left Css.zero
            , Css.width <| Css.pct 100
            , Css.textAlign Css.center
            , Css.lineHeight <| Css.em 1.5
            , Css.fontSize <| Css.px 14
            , Css.paddingBottom <| Css.px 12
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
                , Html.text "Planning Game is Free Software released under "
                , Html.a [ Attrs.href "https://www.gnu.org/licenses/agpl-3.0.en.html" ] [ Html.text "AGPLv3 license" ]
                , Html.br [] []
                , Html.text "Souce code is available on "
                , Html.a [ Attrs.href "https://github.com/turboMaCk/planning-game" ] [ Html.text "GitHub" ]
                ]
            , Html.p []
                [ Html.text "Developed & designed by Marek Fajkus"
                , Html.br [] []
                , Html.a [ Attrs.href "https://github.com/turboMaCk/planning-game/issues" ] [ Html.text "Report issue or request feature" ]
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
document ({ showCookiesNotice } as model) =
    let
        ( title, body ) =
            case model.page of
                Authorized _ page ->
                    case page of
                        Home m ->
                            ( "Planning Game"
                            , withLayout showCookiesNotice <|
                                Html.map HomeMsg <|
                                    Join.view (joinTitle "Create Table") m
                            )

                        Table m ->
                            ( "Table | Planning Game"
                            , withLayout showCookiesNotice <| Html.map TableMsg <| Table.view m
                            )

                        JoinTable _ m ->
                            ( "Join | Planning Game"
                            , withLayout showCookiesNotice <| Html.map JoinTableMsg <| Join.view (joinTitle "Join Table") m
                            )

                        NotFound ->
                            ( "404 | Planning Game"
                            , withLayout showCookiesNotice <| Html.text "404"
                            )

                Unauthorized _ ->
                    -- @TODO: error handling?
                    ( "Authorizing | Planning Game"
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
