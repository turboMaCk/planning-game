module Table exposing (Model, Msg, init)

import Data exposing (Table, User)
import Dict exposing (Dict)
import Http


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
