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


type alias Config msg =
    { isMe : Player -> Bool
    , isDealer : Player -> Bool
    , toVote : Player -> PlayerVote
    , kick : Player -> msg
    }


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


dealerIndicator : Bool -> Html msg
dealerIndicator isDealer =
    if isDealer then
        Html.styled Html.span
            [ Theme.pill
            , Css.backgroundColor Theme.values.secondaryColor
            , Css.marginLeft Css.zero
            ]
            [ Attrs.title "Dealer" ]
            [ Html.text "D" ]

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


viewPlayer : Config msg -> Bool -> Player -> Html msg
viewPlayer { isMe, toVote, kick, isDealer } amIDealer player =
    let
        showKick =
            amIDealer && not (isMe player)
    in
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
        , if showKick then
            Css.hover
                [ Css.textDecoration Css.lineThrough
                , Css.cursor Css.pointer
                ]

          else
            Css.hover []
        ]
        (if showKick then
            [ Events.onClick <| kick player ]

         else
            []
        )
        [ onlineIndicator player.isConnected
        , dealerIndicator <| isDealer player
        , Html.text player.name
        , voteIndicator <| toVote player
        ]


view : Config msg -> Bool -> Dict Int Player -> Html msg
view config amIDealer players =
    Html.div []
        [ Html.styled Html.ul
            [ Css.listStyle Css.none
            , Css.margin Css.zero
            , Css.padding Css.zero
            ]
            []
          <|
            (List.map (viewPlayer config amIDealer) <|
                Dict.values players
            )
        ]
