module Player exposing (..)

import Html exposing (Html, div, h6, text, button, img)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Maybe
import Character exposing (Character, barbarian, moonElf)


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    , statusEffects : StatusEffects
    }


type alias StatusEffects =
    Dict String Int


initialPlayerOne : Player
initialPlayerOne =
    { health = 50
    , combatPoints = 2
    , character = barbarian
    , statusEffects = initialStatusEffects
    }


initialPlayerTwo : Player
initialPlayerTwo =
    { health = 50
    , combatPoints = 2
    , character = moonElf
    , statusEffects = initialStatusEffects
    }


initialStatusEffects : Dict String Int
initialStatusEffects =
    Dict.fromList
        [ ( "concussion", 0 )
        , ( "stun", 0 )
        , ( "blind", 0 )
        , ( "entangle", 0 )
        , ( "evasive", 0 )
        , ( "targeted", 0 )
        ]



-- update


type Message
    = AdjustHealth Int
    | AdjustCombatPoints Int
    | AdjustStatusEffect String Int


update : Message -> Player -> Player
update message player =
    case message of
        AdjustHealth amount ->
            { player | health = clamp 0 99 (player.health + amount) }

        AdjustCombatPoints amount ->
            { player | combatPoints = clamp 0 15 (player.combatPoints + amount) }

        AdjustStatusEffect statusEffect amount ->
            { player
                | statusEffects = Dict.update statusEffect (Maybe.map <| \count -> count + amount) player.statusEffects
            }



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
