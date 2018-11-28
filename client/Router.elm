module Router exposing (Route(..), route)

import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser)


type Route
    = Home
    | Table String
    | NotFound


routes : Parser (Route -> a) a
routes =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Table <| Url.s "room" </> Url.string
        ]


route : (Route -> a) -> Url -> a
route f =
    f
        << Maybe.withDefault NotFound
        << Url.parse routes
