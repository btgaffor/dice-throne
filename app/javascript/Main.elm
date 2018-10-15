module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, text, button)
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
    }



-- INIT


init : ( Model, Cmd Message )
init =
    ( Model [ newDie 1, newDie 2, newDie 3, newDie 4, newDie 5 ], Cmd.none )



-- VIEW


renderDie index die =
    let
        color =
            if die.reroll then
                "btn-danger"
            else
                "btn-success"
    in
        button [ class "btn", class color, style "margin-left" "4px", onClick (ToggleReroll index) ]
            [ text <| String.fromInt die.result
            ]


renderDice dice =
    List.indexedMap renderDie dice


rollButton =
    button [ class "btn", class "btn-primary", style "margin-top" "16px", style "margin-left" "16px", onClick DoRoll ] [ text "Roll" ]


selectAllButton =
    button [ class "btn", class "btn-secondary", onClick SelectAll, style "margin-top" "16px", style "margin-left" "4px" ] [ text "Select ALL" ]


view : Model -> Html Message
view model =
    div []
        [ div [ style "margin-top" "16px", style "margin-left" "12px" ] (renderDice model.roll)
        , rollButton
        , selectAllButton
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
