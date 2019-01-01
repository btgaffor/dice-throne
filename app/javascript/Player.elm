module Player exposing (..)

import Html exposing (Html, div, h6, text, button, img)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Character exposing (Character, barbarian, moonElf)


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    }


initialPlayerOne : Player
initialPlayerOne =
    { health = 50
    , combatPoints = 2
    , character = barbarian
    }


initialPlayerTwo : Player
initialPlayerTwo =
    { health = 50
    , combatPoints = 2
    , character = moonElf
    }



-- update


type Message
    = AdjustHealth Int
    | AdjustCombatPoints Int


update : Message -> Player -> Player
update message player =
    case message of
        AdjustHealth amount ->
            { player | health = clamp 0 99 (player.health + amount) }

        AdjustCombatPoints amount ->
            { player | combatPoints = clamp 0 15 (player.combatPoints + amount) }



-- View


renderStats : Player -> Html Message
renderStats player =
    div [ class "flex-row", style "margin-bottom" "16px" ]
        [ div [ class "flex-col", style "margin-left" "16px", style "margin-top" "16px" ]
            [ h6 [] [ text "Health" ]
            , div [ class "flex-row" ]
                [ minusButton AdjustHealth
                , valueDisplay player.health
                , plusButton AdjustHealth
                ]
            ]
        , div [ class "flex-col", style "margin-left" "16px", style "margin-top" "16px" ]
            [ h6 [] [ text "CP" ]
            , div [ class "flex-row" ]
                [ minusButton AdjustCombatPoints
                , valueDisplay player.combatPoints
                , plusButton AdjustCombatPoints
                ]
            ]
        ]


valueDisplay value =
    div [ class "value-display" ] [ text <| String.fromInt value ]


minusButton message =
    button [ class "btn btn-secondary", onClick (message -1) ] [ text "-" ]


plusButton message =
    button [ class "btn btn-secondary", onClick (message 1) ] [ text "+" ]


renderBoards : Player -> Html message
renderBoards player =
    div [ class "flex-row" ]
        [ img [ src player.character.guideImage, class "guide-board" ] []
        , img [ src player.character.actionImage, class "action-board" ] []
        ]
