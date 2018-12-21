module Page.Table.Players exposing (view)

import Css
import Data exposing (Player)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Maybe.Extra as Maybe
import Theme


viewOnlineIndicator : Bool -> Html msg
viewOnlineIndicator isActive =
    let
        size =
            Css.px 8

        text =
            if isActive then
                "online"

            else
                "offlie"
    in
    Html.styled Html.span
        [ Css.display Css.inlineBlock
        , Css.width size
        , Css.height size
        , Css.borderRadius <| Css.pct 100
        , Css.overflow <| Css.hidden
        , Css.textIndent <| Css.px -999
        , Css.margin4 Css.zero (Css.px 6) (Css.px 2) Css.zero
        , Css.backgroundColor <|
            if isActive then
                Theme.values.greenColor

            else
                Theme.values.primaryColor
        ]
        [ Attrs.title text ]
        [ Html.text text
        ]


bankerIndicator : Bool -> Html msg
bankerIndicator isBanker =
    if isBanker then
        Html.styled Html.span
            [ Css.display Css.inlineBlock
            , Css.marginRight <| Css.px 4
            , Css.padding2 Css.zero <| Css.px 4
            , Css.fontSize <| Css.px 12
            , Css.fontWeight <| Css.int 600
            , Css.backgroundColor Theme.values.secondaryColor
            , Css.color Theme.values.lightColor
            ]
            [ Attrs.title "Banker" ]
            [ Html.text "B" ]

    else
        Html.text ""


viewPlayer : Maybe Player -> Maybe Player -> Player -> Html msg
viewPlayer me banker player =
    Html.styled Html.li
        [ Css.listStyle Css.none
        , Css.margin4 Css.zero Css.zero (Css.px 6) Css.zero
        , Css.fontWeight <| Css.int 200
        , Css.overflow Css.hidden
        , Css.textOverflow Css.ellipsis
        ]
        []
        [ viewOnlineIndicator player.isConnected
        , bankerIndicator <| banker == Just player
        , Html.text player.name
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
