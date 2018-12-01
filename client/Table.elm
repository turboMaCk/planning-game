module Table exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Data exposing (ApiError, Table, TableError(..), User)
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Url.Builder as Url


type alias Model =
    { tableId : String
    , me : Maybe User -- @TODO better error type?
    , banker : Maybe User
    , players : Dict String Bool
    , tableError : Maybe (ApiError TableError)
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , banker = Nothing
      , players = Dict.empty
      , tableError = Nothing
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result (ApiError TableError) User)


update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update navigationKey msg model =
    case msg of
        Me result ->
            case result of
                Ok player ->
                    { model | me = Just player }
                        |> Cmd.pure

                Err err ->
                    ( { model | tableError = Just err }
                    , if Data.errorIs PlayerNotFound err then
                        Navigation.pushUrl navigationKey <|
                            Url.absolute [ "table", model.tableId, "join" ] []

                      else
                        Cmd.none
                    )


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.text <|
            Maybe.withDefault "" <|
                Maybe.map .name model.me
