module Page.Table.Players exposing (PlayerVote(..), view)

import Css
import Data exposing (Player, Vote)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Maybe.Extra as Maybe
import Theme


type PlayerVote
    = Hidden
    | Unknown
    | Voted Vote


voteIndicator : PlayerVote -> Html msg
voteIndicator playerVote =
    let
        show txt =
            Html.styled Html.span
                [ Theme.pill
                , Css.backgroundColor Theme.values.primaryColor
                , Css.borderRadius <| Css.pct 100
                ]
                [ Attrs.title <| "player voted: " ++ txt ]
                [ Html.text txt ]
    in
    case playerVote of
        Hidden ->
            Html.text ""

        Unknown ->
            show "?"

        Voted vote ->
            show <| String.fromInt <| Data.voteToInt vote


bankerIndicator : Bool -> Html msg
bankerIndicator isBanker =
    if isBanker then
        Html.styled Html.span
            [ Theme.pill
            , Css.backgroundColor Theme.values.secondaryColor
            , Css.marginLeft Css.zero
            ]
            [ Attrs.title "Banker" ]
            [ Html.text "B" ]

    else
        Html.text ""


onlineIndicator : Bool -> Html msg
onlineIndicator isActive =
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


viewPlayer : (Player -> Bool) -> (Player -> PlayerVote) -> (Player -> Bool) -> (Player -> msg) -> Player -> Html msg
viewPlayer isMe toVote isBanker kick player =
    Html.styled Html.li
        [ Css.listStyle Css.none
        , Css.margin4 Css.zero Css.zero (Css.px 6) Css.zero
        , Css.fontWeight <| Css.int 200
        , Css.overflow Css.hidden
        , Css.textOverflow Css.ellipsis
        , if isMe player then
            Css.textDecoration Css.underline

          else
            Css.property "foo" "bar"
        ]
        []
        [ onlineIndicator player.isConnected
        , bankerIndicator <| isBanker player
        , Html.text player.name
        , voteIndicator <| toVote player
        , Html.br [] []
        , Html.span [ Events.onClick <| kick player ]
            [ Html.text "kick" ]
        ]


type alias Config msg =
    { isMe : Player -> Bool
    , toVote : Player -> PlayerVote
    , kick : Player -> msg
    }


view : Config msg -> Maybe Player -> Dict String Player -> Html msg
view { isMe, toVote, kick } banker players =
    let
        isBanker =
            (==) banker << Just
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Players:" ]
        , Html.styled Html.ul
            [ Css.listStyle Css.none
            , Css.margin Css.zero
            , Css.padding Css.zero
            ]
            []
          <|
            Maybe.unwrap (Html.text "") (viewPlayer isMe toVote isBanker kick) banker
                :: (List.map (viewPlayer isMe toVote isBanker kick) <|
                        Dict.values players
                   )
        ]
