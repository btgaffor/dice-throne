module Dice exposing (renderDiceSection)

import Html exposing (Html, div, text, button, img, sup)
import Html.Attributes exposing (class, src, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Model exposing (Model, Die, Roll, RollState(..))
import Update exposing (Message(..))


-- main entry point


renderDiceSection : Model -> Array String -> Html Message
renderDiceSection model dieIcons =
    let
        isSelectingDice =
            model.rollState == SelectingNumber

        noDiceSelected =
            not <| List.any .selected model.roll
    in
        div []
            [ renderRollCount model.rollState model.rollCount
            , (renderDice model.rollState dieIcons model.roll)
            , rollButton <| isSelectingDice || noDiceSelected
            , selectButton isSelectingDice <| not (List.isEmpty model.roll) && List.all .selected model.roll
            , increaseSelectedDiceButton <| isSelectingDice || noDiceSelected
            , decreaseSelectedDiceButton <| isSelectingDice || noDiceSelected
            , newRollButton isSelectingDice
            ]



-- helpers


renderRollCount : RollState -> Int -> Html Message
renderRollCount rollState count =
    case rollState of
        SelectingNumber ->
            div [ class "roll-count" ] [ text "Choose number of dice to roll" ]

        Rolling ->
            div [ class "roll-count" ] [ text <| "Rolls: " ++ (String.fromInt count) ]


renderDice : RollState -> Array String -> Roll -> Html Message
renderDice rollState dieIcons dice =
    case rollState of
        SelectingNumber ->
            div [ class "flex-row dice" ]
                (List.map
                    (\number -> button [ class "btn btn-secondary die-button", onClick (SelectDiceAmount number) ] [ text <| String.fromInt number ])
                    (List.range 1 5)
                )

        Rolling ->
            div [ class "flex-row dice" ] (List.indexedMap (renderDie dieIcons) dice)


rollButton : Bool -> Html Message
rollButton isDisabled =
    button [ class "btn btn-primary roll", disabled isDisabled, onClick DoRoll ] [ text "Roll" ]


renderDie : Array String -> Int -> Die -> Html Message
renderDie dieIcons index die =
    button [ class "btn", class (dieColor die.selected), class "die-button", onClick (ToggleSelected index) ]
        [ img [ src <| Maybe.withDefault "" <| Array.get die.result dieIcons, class "die-icon" ] []
        , sup []
            [ text <| String.fromInt die.result
            ]
        ]


dieColor : Bool -> String
dieColor selected =
    if selected then
        "btn-danger"
    else
        "btn-success"


selectButton : Bool -> Bool -> Html Message
selectButton isDisabled allSelected =
    if allSelected then
        button [ class "btn btn-secondary toolbar-button", onClick SelectNone, disabled isDisabled, class "select-all" ] [ text "Select None" ]
    else
        button [ class "btn btn-secondary toolbar-button", onClick SelectAll, disabled isDisabled, class "select-all" ] [ text "Select All" ]


increaseSelectedDiceButton : Bool -> Html Message
increaseSelectedDiceButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", disabled isDisabled, onClick IncreaseSelectedDice ] [ text "Increase" ]


decreaseSelectedDiceButton : Bool -> Html Message
decreaseSelectedDiceButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", disabled isDisabled, onClick DecreaseSelectedDice ] [ text "Decrease" ]


newRollButton : Bool -> Html Message
newRollButton isDisabled =
    button [ class "btn btn-success toolbar-button", disabled isDisabled, onClick NewRoll ] [ text "New Roll" ]
