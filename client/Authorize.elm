module Authorize exposing (Authorize(..), by, for, init)

import Http


type Authorize a b
    = Authorized b a
    | Unauthorized (Maybe Http.Error)


init : Authorize a b
init =
    Unauthorized Nothing


by : b -> (b -> c) -> Authorize a b -> Authorize c b
by b f auth =
    Authorized b <| f b


for : (b -> c) -> Authorize a b -> Authorize c b
for f auth =
    case auth of
        Unauthorized err ->
            Unauthorized err

        Authorized b _ ->
            Authorized b <| f b
