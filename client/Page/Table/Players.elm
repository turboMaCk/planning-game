module Page.Table.Players exposing (view)

import Css
import Data exposing (Player)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Maybe.Extra as Maybe


viewPlayer : Maybe Player -> Maybe Player -> Player -> Html msg
viewPlayer me banker player =
    Html.styled Html.li
        [ Css.listStyle Css.none
        , Css.margin Css.zero
        ]
        []
        [ Html.text player.name
        , if player.isConnected then
            Html.text " o"

          else
            Html.text " x"
        , if Maybe.map .name banker == Just player.name then
            Html.text " banker"

          else
            Html.text ""
        , if Maybe.map .name me == Just player.name then
            Html.text " change name"

          else
            Html.text ""
        ]


view : Maybe Player -> Maybe Player -> Dict String Player -> Html msg
view me banker players =
    Html.div []
        [ Html.h3 [] [ Html.text "Players:" ]
        , Html.styled Html.ul
            [ Css.listStyle Css.none
            , Css.margin Css.zero
            , Css.padding Css.zero
            ]
            []
          <|
            Maybe.unwrap (Html.text "") (viewPlayer me banker) banker
                :: (List.map (viewPlayer me banker) <|
                        Dict.values players
                   )
        ]
