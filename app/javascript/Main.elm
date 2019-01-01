module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img, sup)
import Html.Attributes exposing (style, class, src, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Update exposing (Message(..), update)
import Model exposing (Model, Die, RollState(..))
import Player exposing (Player, initialPlayerOne, initialPlayerTwo)


-- INIT


init : ( Model, Cmd Message )
init =
    ( Model [] SelectingNumber 0 [ initialPlayerOne, initialPlayerTwo ] 0, Cmd.none )



-- VIEW


dieColor : Bool -> String
dieColor selected =
    if selected then
        "btn-danger"
    else
        "btn-success"


dieIcon : Int -> Array String -> String
dieIcon number icons =
    Maybe.withDefault "" <| Array.get number icons


renderDie dieIcons index die =
    button [ class "btn", class <| dieColor die.selected, class "die-button", onClick (ToggleSelected index) ]
        [ img [ src <| dieIcon die.result dieIcons, class "die-icon" ] []
        , sup []
            [ text <| String.fromInt die.result
            ]
        ]


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


rollButton isDisabled =
    button [ class "btn btn-primary roll", disabled isDisabled, onClick DoRoll ] [ text "Roll" ]


selectNoneButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", onClick SelectNone, disabled isDisabled, class "select-all" ] [ text "Select None" ]


selectAllButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", onClick SelectAll, disabled isDisabled, class "select-all" ] [ text "Select All" ]


increaseSelectedDiceButton : Bool -> Html Message
increaseSelectedDiceButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", disabled isDisabled, onClick IncreaseSelectedDice ] [ text "Increase" ]


decreaseSelectedDiceButton isDisabled =
    button [ class "btn btn-secondary toolbar-button", disabled isDisabled, onClick DecreaseSelectedDice ] [ text "Decrease" ]


newRollButton isDisabled =
    button [ class "btn btn-success toolbar-button", disabled isDisabled, onClick NewRoll ] [ text "New Roll" ]


minusButton message =
    button [ class "btn btn-secondary", onClick (message -1) ] [ text "-" ]


plusButton message =
    button [ class "btn btn-secondary", onClick (message 1) ] [ text "+" ]


renderRollCount rollStatus count =
    case rollStatus of
        SelectingNumber ->
            div [ class "roll-count" ] [ text "Choose number of dice to roll" ]

        Rolling ->
            div [ class "roll-count" ] [ text <| "Rolls: " ++ (String.fromInt count) ]


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
            , if (not (List.isEmpty model.roll)) && (List.all .selected model.roll) then
                selectNoneButton isSelectingDice
              else
                selectAllButton isSelectingDice
            , increaseSelectedDiceButton <| isSelectingDice || noDiceSelected
            , decreaseSelectedDiceButton <| isSelectingDice || noDiceSelected
            , newRollButton isSelectingDice
            ]


valueDisplay value =
    div [ class "value-display" ] [ text <| String.fromInt value ]


renderPlayer : Model -> Html Message
renderPlayer model =
    div []
        (List.indexedMap
            (\index player ->
                if index == model.currentPlayer then
                    div [ class "flex-col" ]
                        [ renderDiceSection model player.character.dieIcons
                        , div [ class "flex-row", style "margin-bottom" "16px" ]
                            [ div [ class "flex-col", style "margin-left" "16px", style "margin-top" "16px" ]
                                [ h6 [] [ text "Health" ]
                                , div [ class "flex-row" ]
                                    [ minusButton (UpdatePlayer model.currentPlayer << Player.AdjustHealth)
                                    , valueDisplay player.health
                                    , plusButton (UpdatePlayer model.currentPlayer << Player.AdjustHealth)
                                    ]
                                ]
                            , div [ class "flex-col", style "margin-left" "16px", style "margin-top" "16px" ]
                                [ h6 [] [ text "CP" ]
                                , div [ class "flex-row" ]
                                    [ minusButton (UpdatePlayer model.currentPlayer << Player.AdjustCombatPoints)
                                    , valueDisplay player.combatPoints
                                    , plusButton (UpdatePlayer model.currentPlayer << Player.AdjustCombatPoints)
                                    ]
                                ]
                            ]
                        , div [ class "flex-row" ]
                            [ img [ src player.character.guideImage, class "guide-board" ] []
                            , img [ src player.character.actionImage, class "action-board" ] []
                            ]
                        ]
                else
                    text ""
            )
            model.players
        )


renderPlayerSidebarItem : Int -> Int -> Player -> Html Message
renderPlayerSidebarItem currentPlayer index player =
    let
        backgroundColor =
            if currentPlayer == index then
                "lightgreen"
            else
                "white"
    in
        div [ class "flex-col", class "player-sidebar-item", style "background-color" backgroundColor, onClick (SelectPlayer index) ]
            [ div [] [ text <| "HP: " ++ (String.fromInt player.health) ]
            , div [] [ text <| "CP: " ++ (String.fromInt player.combatPoints) ]
            ]


view : Model -> Html Message
view model =
    div [ class "flex-row" ]
        [ div [ style "flex-grow" "1" ]
            [ renderPlayer model
            ]
        , div [ style "flex-basis" "250px" ]
            [ div [ class "flex-col" ]
                (List.indexedMap (renderPlayerSidebarItem model.currentPlayer) model.players)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- MAIN


main : Program (Maybe {}) Model Message
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
