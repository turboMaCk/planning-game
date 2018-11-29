module Router exposing (Route(..), route)

import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser)


type Route
    = Home
    | Table String
    | JoinTable String
    | NotFound


routes : Parser (Route -> a) a
routes =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Table <| Url.s "table" </> Url.string
        , Url.map JoinTable <| Url.s "table" </> Url.string </> Url.s "join"
        ]


route : Url -> Route
route =
    Maybe.withDefault NotFound
        << Url.parse routes
