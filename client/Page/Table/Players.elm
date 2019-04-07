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


viewPlayer : Config msg -> (Player -> Bool) -> Bool -> Player -> Html msg
viewPlayer { isMe, toVote, kick } isBanker showKick player =
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
    <|
        [ onlineIndicator player.isConnected
        , bankerIndicator <| isBanker player
        , Html.text player.name
        , voteIndicator <| toVote player
        ]
            ++ (if showKick then
                    [ Html.styled Html.span
                        [ Css.textDecoration Css.underline
                        , Css.fontSize <| Css.px 12
                        , Css.display Css.block
                        , Css.cursor Css.pointer
                        ]
                        [ Events.onClick <| kick player ]
                        [ Html.text "kick out" ]
                    ]

                else
                    []
               )


type alias Config msg =
    { isMe : Player -> Bool
    , toVote : Player -> PlayerVote
    , kick : Player -> msg
    }


view : Config msg -> Maybe Player -> Dict String Player -> Html msg
view config banker players =
    let
        isBanker =
            (==) banker << Just

        amIBanker =
            Maybe.unwrap False config.isMe banker
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
            Maybe.unwrap (Html.text "") (viewPlayer config isBanker False) banker
                :: (List.map (viewPlayer config isBanker amIBanker) <|
                        Dict.values players
                   )
        ]
