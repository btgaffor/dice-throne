module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Random


-- MODEL


type alias Die =
    { result : Int
    , reroll : Bool
    }


newDie : Int -> Die
newDie result =
    { result = result, reroll = False }


type alias Roll =
    -- 5 dice
    List Die


type alias Model =
    { roll : Roll
    , showAdjust : Bool
    , health : Int
    , combatPoints : Int
    }



-- INIT


init : ( Model, Cmd Message )
init =
    ( Model [ newDie 1, newDie 2, newDie 3, newDie 4, newDie 5 ] False 50 2, Cmd.none )



-- VIEW


renderDie showAdjust index die =
    let
        color =
            if die.reroll then
                "btn-danger"
            else
                "btn-success"
    in
        div [ style "display" "flex", style "flex-direction" "column" ]
            [ if showAdjust then
                button [ class "btn", class "btn-secondary", style "margin-left" "4px", style "margin-bottom" "4px", onClick (AdjustDie index -1) ]
                    [ text "-"
                    ]
              else
                text ""
            , button [ class "btn", class color, style "margin-left" "4px", onClick (ToggleReroll index) ]
                [ text <| String.fromInt die.result
                ]
            , if showAdjust then
                button [ class "btn", class "btn-secondary", style "margin-left" "4px", style "margin-top" "4px", onClick (AdjustDie index 1) ]
                    [ text "+"
                    ]
              else
                text ""
            ]


renderDice showAdjust dice =
    div [ style "display" "flex", style "flex-direction" "row", style "margin-top" "16px", style "margin-left" "12px" ]
        (List.indexedMap (renderDie showAdjust) dice)


rollButton =
    button [ class "btn", class "btn-primary", style "margin-top" "16px", style "margin-left" "16px", onClick DoRoll ] [ text "Roll" ]


selectAllButton =
    button [ class "btn", class "btn-secondary", onClick SelectAll, style "margin-top" "16px", style "margin-left" "4px" ] [ text "Select All" ]


toggleAdjustButton =
    button [ class "btn", class "btn-secondary", onClick ToggleAdjust, style "margin-top" "16px", style "margin-left" "4px" ] [ text "Adjust" ]


minusButton message =
    button [ class "btn", class "btn-secondary", onClick (message -1) ]
        [ text "-" ]


valueDisplay value =
    div [ style "padding" "8px" ]
        [ text <| String.fromInt value
        ]


plusButton message =
    button [ class "btn", class "btn-secondary", onClick (message 1) ]
        [ text "+" ]


view : Model -> Html Message
view model =
    div [ style "display" "flex", style "flex-direction" "column" ]
        [ div []
            [ (renderDice model.showAdjust model.roll)
            , rollButton
            , selectAllButton
            , toggleAdjustButton
            ]
        , div [ style "display" "flex", style "flex-direction" "row" ]
            [ div [ style "display" "flex", style "flex-direction" "column", style "margin-left" "16px", style "margin-top" "16px" ]
                [ h6 [] [ text "Health" ]
                , div [ style "display" "flex", style "flex-direction" "row" ]
                    [ minusButton AdjustHealth
                    , valueDisplay model.health
                    , plusButton AdjustHealth
                    ]
                ]
            , div [ style "display" "flex", style "flex-direction" "column", style "margin-left" "16px", style "margin-top" "16px" ]
                [ h6 [] [ text "CP" ]
                , div [ style "display" "flex", style "flex-direction" "row" ]
                    [ minusButton AdjustCombatPoints
                    , valueDisplay model.combatPoints
                    , plusButton AdjustCombatPoints
                    ]
                ]
            ]
        ]



-- MESSAGE


rollDie : Random.Generator Die
rollDie =
    Random.map newDie <| Random.int 1 6


rollDice : Int -> Random.Generator (List Die)
rollDice rollCount =
    Random.list rollCount rollDie


type Message
    = DoRoll
    | NewRoll (List Die)
    | ToggleReroll Int
    | SelectAll
    | ToggleAdjust
    | AdjustDie Int Int
    | AdjustHealth Int
    | AdjustCombatPoints Int



-- UPDATE


rerollCount dice =
    List.length <| List.filter .reroll dice


notRerolledDice dice =
    List.filter (not << .reroll) dice


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DoRoll ->
            ( model, Random.generate NewRoll <| rollDice <| rerollCount model.roll )

        NewRoll newRoll ->
            ( { model | roll = List.sortBy .result (List.concat [ (notRerolledDice model.roll), newRoll ]) }, Cmd.none )

        ToggleReroll index ->
            ( { model
                | roll =
                    List.indexedMap
                        (\dieIndex die ->
                            if dieIndex == index then
                                { die | reroll = not die.reroll }
                            else
                                die
                        )
                        model.roll
              }
            , Cmd.none
            )

        SelectAll ->
            ( { model
                | roll = List.map (\die -> { die | reroll = True }) model.roll
              }
            , Cmd.none
            )

        ToggleAdjust ->
            ( { model | showAdjust = not model.showAdjust }, Cmd.none )

        AdjustDie index amount ->
            ( { model
                | roll =
                    List.indexedMap
                        (\dieIndex die ->
                            if dieIndex == index then
                                { die | result = clamp 1 6 (die.result + amount) }
                            else
                                die
                        )
                        model.roll
              }
            , Cmd.none
            )

        AdjustHealth amount ->
            ( { model | health = clamp 0 99 (model.health + amount) }, Cmd.none )

        AdjustCombatPoints amount ->
            ( { model | combatPoints = clamp 0 15 (model.combatPoints + amount) }, Cmd.none )



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
