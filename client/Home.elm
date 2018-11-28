module Home exposing (Model, Msg(..), init, update, view)

import Data exposing (Session)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http


type alias Model =
    { userName : String
    }


init : ( Model, Cmd msg )
init =
    ( Model ""
    , Cmd.none
    )


type Msg
    = UpdateName String
    | Submit
    | JoinResponse (Result Http.Error String)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        UpdateName str ->
            ( Model str, Cmd.none )

        Submit ->
            ( model
            , Data.createTable session JoinResponse model.userName
            )

        JoinResponse res ->
            case res of
                Ok str ->
                    ( model
                    , Cmd.none
                    )

                Err _ ->
                    -- @TODO: clear session
                    ( model, Cmd.none )


view : Model -> Html Msg
view { userName } =
    Html.form
        [ Event.onSubmit Submit ]
        [ Html.input
            [ Attr.value userName
            , Event.onInput UpdateName
            ]
            []
        , Html.button [ Attr.type_ "submit" ]
            [ Html.text "Submit" ]
        , Html.a [ Attr.href "/some-room" ] [ Html.text "some room" ]
        ]
