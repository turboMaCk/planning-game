module Table exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Data exposing (Table, User)
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Url.Builder as Url


type alias Model =
    { tableId : String
    , me : Maybe User -- @TODO better error type?
    , banker : Maybe User
    , players : Dict String Bool
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , banker = Nothing
      , players = Dict.empty
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result Http.Error User)


update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update navigationKey msg model =
    case msg of
        Me result ->
            case result of
                Ok player ->
                    { model | me = Just player }
                        |> Cmd.pure

                Err _ ->
                    ( model
                    , Navigation.pushUrl navigationKey <|
                        Url.absolute [ "table", model.tableId, "join" ] []
                    )


view : Model -> Html Msg
view model =
    Html.text <| Maybe.withDefault "" <| Maybe.map .name model.me
