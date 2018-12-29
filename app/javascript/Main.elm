module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img, sup)
import Html.Attributes exposing (style, class, src)
import Html.Events exposing (onClick)
import Random
import List.Extra
import Array exposing (Array)


-- MODEL


type alias Die =
    { result : Int
    , selected : Bool
    }


newDie : Int -> Die
newDie result =
    { result = result, selected = False }


type alias Roll =
    List Die


type RollState
    = SelectingNumber
    | Rolling


type alias Model =
    { roll : Roll
    , rollState : RollState
    , rollCount : Int
    , players : List Player
    , currentPlayer : Int
    }


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    }


type alias Character =
    { guideImage : String
    , actionImage : String
    , dieIcons : Array.Array String
    }


barbarian : Character
barbarian =
    { guideImage = "barbarian_guide.jpg"
    , actionImage = "barbarian_actions.png"
    , dieIcons =
        Array.fromList
            [ ""
            , "barbarian_sword.png"
            , "barbarian_sword.png"
            , "barbarian_sword.png"
            , "barbarian_heart.png"
            , "barbarian_heart.png"
            , "barbarian_pow.png"
            ]
    }


moonElf : Character
moonElf =
    { guideImage = "moon_elf_guide.jpg"
    , actionImage = "moon_elf_actions.png"
    , dieIcons =
        Array.fromList
            [ ""
            , "moon_elf_arrow.png"
            , "moon_elf_arrow.png"
            , "moon_elf_arrow.png"
            , "moon_elf_foot.png"
            , "moon_elf_foot.png"
            , "moon_elf_moon.png"
            ]
    }



-- INIT


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


init : ( Model, Cmd Message )
init =
    ( Model [] SelectingNumber 0 [ initialPlayerOne, initialPlayerTwo ] 0, Cmd.none )



-- VIEW


barbarianDieIcons =
    Array.fromList
        [ ""
        , "barbarian_sword.png"
        , "barbarian_sword.png"
        , "barbarian_sword.png"
        , "barbarian_heart.png"
        , "barbarian_heart.png"
        , "barbarian_pow.png"
        ]


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


rollButton =
    button [ class "btn btn-primary", class "roll", onClick DoRoll ] [ text "Roll" ]


selectButton allSelected =
    if allSelected then
        selectNoneButton
    else
        selectAllButton


selectNoneButton =
    button [ class "btn btn-secondary toolbar-button", onClick SelectNone, class "select-all" ] [ text "Select None" ]


selectAllButton =
    button [ class "btn btn-secondary toolbar-button", onClick SelectAll, class "select-all" ] [ text "Select All" ]


increaseSelectedDiceButton =
    button [ class "btn btn-secondary toolbar-button", onClick IncreaseSelectedDice ] [ text "Increase" ]


decreaseSelectedDiceButton =
    button [ class "btn btn-secondary toolbar-button", onClick DecreaseSelectedDice ] [ text "Decrease" ]


newRollButton =
    button [ class "btn btn-success toolbar-button", onClick NewRoll ] [ text "New Roll" ]


minusButton message =
    button [ class "btn btn-secondary", onClick (message -1) ]
        [ text "-" ]


plusButton message =
    button [ class "btn btn-secondary", onClick (message 1) ] [ text "+" ]


renderRollCount count =
    div [ class "roll-count" ] [ text <| "Rolls: " ++ (String.fromInt count) ]


renderDiceSection model dieIcons =
    div []
        [ renderRollCount model.rollCount
        , (renderDice model.rollState dieIcons model.roll)
        , rollButton
        , selectButton <| (not (List.isEmpty model.roll)) && (List.all .selected model.roll)
        , increaseSelectedDiceButton
        , decreaseSelectedDiceButton
        , newRollButton
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
                                    [ minusButton (UpdatePlayer model.currentPlayer << AdjustHealth)
                                    , valueDisplay player.health
                                    , plusButton (UpdatePlayer model.currentPlayer << AdjustHealth)
                                    ]
                                ]
                            , div [ class "flex-col", style "margin-left" "16px", style "margin-top" "16px" ]
                                [ h6 [] [ text "CP" ]
                                , div [ class "flex-row" ]
                                    [ minusButton (UpdatePlayer model.currentPlayer << AdjustCombatPoints)
                                    , valueDisplay player.combatPoints
                                    , plusButton (UpdatePlayer model.currentPlayer << AdjustCombatPoints)
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
    | RollResult (List Die)
    | ToggleSelected Int
    | SelectAll
    | SelectNone
    | AdjustDie Int Int
    | IncreaseSelectedDice
    | DecreaseSelectedDice
    | UpdatePlayer Int PlayerMessage
    | SelectPlayer Int
    | SelectDiceAmount Int
    | NewRoll



-- UPDATE


rerollCount dice =
    List.length <| List.filter .selected dice


notRerolledDice dice =
    List.filter (not << .selected) dice


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
            ( model, Random.generate RollResult <| rollDice <| rerollCount model.roll )

        RollResult rollResult ->
            ( { model
                | roll = List.sortBy .result (List.concat [ (notRerolledDice model.roll), rollResult ])
                , rollCount =
                    if List.isEmpty rollResult then
                        model.rollCount
                    else
                        model.rollCount + 1
              }
            , Cmd.none
            )

        ToggleSelected index ->
            ( { model
                | roll =
                    List.Extra.updateAt
                        index
                        (\die -> { die | selected = not die.selected })
                        model.roll
              }
            , Cmd.none
            )

        SelectAll ->
            ( { model
                | roll = List.map (\die -> { die | selected = True }) model.roll
              }
            , Cmd.none
            )

        SelectNone ->
            ( { model
                | roll = List.map (\die -> { die | selected = False }) model.roll
              }
            , Cmd.none
            )

        AdjustDie index amount ->
            ( { model
                | roll =
                    List.Extra.updateAt
                        index
                        (\die -> { die | result = clamp 1 6 (die.result + amount) })
                        model.roll
              }
            , Cmd.none
            )

        IncreaseSelectedDice ->
            ( { model
                | roll =
                    List.map
                        (\die ->
                            if die.selected then
                                { die | result = clamp 1 6 (die.result + 1) }
                            else
                                die
                        )
                        model.roll
              }
            , Cmd.none
            )

        DecreaseSelectedDice ->
            ( { model
                | roll =
                    List.map
                        (\die ->
                            if die.selected then
                                { die | result = clamp 1 6 (die.result - 1) }
                            else
                                die
                        )
                        model.roll
              }
            , Cmd.none
            )

        UpdatePlayer playerIndex playerMessage ->
            ( { model
                | players = List.Extra.updateAt playerIndex (updatePlayer playerMessage) model.players
              }
            , Cmd.none
            )

        SelectPlayer selectedPlayer ->
            ( { model | currentPlayer = selectedPlayer }, Cmd.none )

        -- immediately roll with the number of dice selected
        SelectDiceAmount number ->
            ( { model | rollState = Rolling }, Random.generate RollResult <| rollDice <| number )

        NewRoll ->
            ( { model | rollState = SelectingNumber, roll = [], rollCount = 0 }, Cmd.none )



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
