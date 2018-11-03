module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Random
import Array exposing (Array)


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
    , players : Array Player
    }


type alias Player =
    { health : Int
    , combatPoints : Int
    }



-- INIT


init : ( Model, Cmd Message )
init =
    ( Model [ newDie 1, newDie 2, newDie 3, newDie 4, newDie 5 ] False (Array.fromList [ Player 50 2 ]), Cmd.none )



-- VIEW


flexRow attributes children =
    div (List.concat [ attributes, [ style "display" "flex", style "flex-direction" "row" ] ]) children


flexCol attributes children =
    div (List.concat [ attributes, [ style "display" "flex", style "flex-direction" "column" ] ]) children


renderDiePlus : Bool -> Int -> Html Message
renderDiePlus showAdjust index =
    if showAdjust then
        button [ class "btn", class "btn-secondary", style "margin-left" "4px", style "margin-bottom" "4px", onClick (AdjustDie index 1) ]
            [ text "+"
            ]
    else
        text ""


renderDieMinus : Bool -> Int -> Html Message
renderDieMinus showAdjust index =
    if showAdjust then
        button [ class "btn", class "btn-secondary", style "margin-left" "4px", style "margin-top" "4px", onClick (AdjustDie index -1) ]
            [ text "-"
            ]
    else
        text ""


dieColor : Bool -> String
dieColor reroll =
    if reroll then
        "btn-danger"
    else
        "btn-success"


renderDie die index =
    button [ class "btn", class <| dieColor die.reroll, style "margin-left" "4px", onClick (ToggleReroll index) ]
        [ text <| String.fromInt die.result
        ]


renderDieWithAdjust showAdjust index die =
    flexCol []
        [ renderDiePlus showAdjust index
        , renderDie die index
        , renderDieMinus showAdjust index
        ]


renderDice showAdjust dice =
    flexRow [ style "margin-top" "16px", style "margin-left" "12px" ]
        (List.indexedMap (renderDieWithAdjust showAdjust) dice)


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


actionBoard url =
    img [ src url, style "width" "400px", style "height" "336px", style "display" "inline" ] []


guideBoard url =
    img [ src url, style "width" "200px", style "height" "314px", style "display" "inline" ] []


renderPlayer : Model -> Html Message
renderPlayer model =
    flexCol []
        [ div []
            [ (renderDice model.showAdjust model.roll)
            , rollButton
            , selectAllButton
            , toggleAdjustButton
            ]
        , flexRow []
            (case Array.get 0 model.players of
                Just player ->
                    [ flexCol [ style "margin-left" "16px", style "margin-top" "16px" ]
                        [ h6 [] [ text "Health" ]
                        , flexRow []
                            [ minusButton ((UpdatePlayer 0) << AdjustHealth)
                            , valueDisplay player.health
                            , plusButton ((UpdatePlayer 0) << AdjustHealth)
                            ]
                        ]
                    , flexCol [ style "margin-left" "16px", style "margin-top" "16px" ]
                        [ h6 [] [ text "CP" ]
                        , flexRow []
                            [ minusButton ((UpdatePlayer 0) << AdjustCombatPoints)
                            , valueDisplay player.combatPoints
                            , plusButton ((UpdatePlayer 0) << AdjustCombatPoints)
                            ]
                        ]
                    ]

                Nothing ->
                    []
            )
        , flexRow []
            [ guideBoard "barbarian_guide.jpg"
            , actionBoard "barbarian_actions.png"
            ]
        ]


view : Model -> Html Message
view model =
    renderPlayer model



-- MESSAGE


rollDie : Random.Generator Die
rollDie =
    Random.map newDie <| Random.int 1 6


rollDice : Int -> Random.Generator (List Die)
rollDice rollCount =
    Random.list rollCount rollDie


type PlayerMessage
    = AdjustHealth Int
    | AdjustCombatPoints Int


type Message
    = DoRoll
    | NewRoll (List Die)
    | ToggleReroll Int
    | SelectAll
    | ToggleAdjust
    | AdjustDie Int Int
    | UpdatePlayer Int PlayerMessage



-- UPDATE


rerollCount dice =
    List.length <| List.filter .reroll dice


notRerolledDice dice =
    List.filter (not << .reroll) dice


updatePlayer : PlayerMessage -> Player -> Player
updatePlayer message player =
    case message of
        AdjustHealth amount ->
            { player | health = clamp 0 99 (player.health + amount) }

        AdjustCombatPoints amount ->
            { player | combatPoints = clamp 0 15 (player.combatPoints + amount) }


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

        UpdatePlayer playerIndex playerMessage ->
            let
                updatedPlayers =
                    Array.indexedMap
                        (\index player ->
                            if index == playerIndex then
                                updatePlayer playerMessage player
                            else
                                player
                        )
                        model.players
            in
                ( { model | players = updatedPlayers }, Cmd.none )



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
