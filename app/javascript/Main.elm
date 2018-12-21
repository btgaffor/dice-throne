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
    , reroll : Bool
    }


newDie : Int -> Die
newDie result =
    { result = result, reroll = False }


type alias Roll =
    List Die


type alias Model =
    { roll : Roll
    , showAdjust : Bool
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
    ( Model [ newDie 1, newDie 2, newDie 3, newDie 4, newDie 5 ] False [ initialPlayerOne, initialPlayerTwo ] 0, Cmd.none )



-- VIEW


flexRow attributes children =
    div (List.concat [ attributes, [ style "display" "flex", style "flex-direction" "row" ] ]) children


flexCol attributes children =
    div (List.concat [ attributes, [ style "display" "flex", style "flex-direction" "column" ] ]) children


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


dieIcon : Int -> Array String -> String
dieIcon number icons =
    case Array.get number icons of
        Just icon ->
            icon

        Nothing ->
            ""


renderDie die index dieIcons =
    button [ class "btn", class <| dieColor die.reroll, style "width" "50px", style "height" "50px", style "margin-left" "4px", onClick (ToggleReroll index) ]
        [ img [ src <| dieIcon die.result dieIcons, style "width" "20px", style "height" "20px", style "display" "inline" ] []
        , sup []
            [ text <| String.fromInt die.result
            ]
        ]


renderDieWithAdjust showAdjust dieIcons index die =
    flexCol []
        [ renderDiePlus showAdjust index
        , renderDie die index dieIcons
        , renderDieMinus showAdjust index
        ]


renderDice showAdjust dieIcons dice =
    flexRow [ style "margin-top" "16px", style "margin-left" "12px" ]
        (List.indexedMap (renderDieWithAdjust showAdjust dieIcons) dice)


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
    img [ src url, style "width" "214px", style "height" "336px", style "display" "inline" ] []


renderDiceSection model dieIcons =
    div []
        [ (renderDice model.showAdjust dieIcons model.roll)
        , rollButton
        , selectAllButton
        , toggleAdjustButton
        ]


renderPlayer : Model -> Html Message
renderPlayer model =
    div []
        (List.indexedMap
            (\index player ->
                if index == model.currentPlayer then
                    flexCol []
                        [ renderDiceSection model player.character.dieIcons
                        , flexRow [ style "margin-bottom" "16px" ]
                            [ flexCol [ style "margin-left" "16px", style "margin-top" "16px" ]
                                [ h6 [] [ text "Health" ]
                                , flexRow []
                                    [ minusButton (UpdatePlayer model.currentPlayer << AdjustHealth)
                                    , valueDisplay player.health
                                    , plusButton (UpdatePlayer model.currentPlayer << AdjustHealth)
                                    ]
                                ]
                            , flexCol [ style "margin-left" "16px", style "margin-top" "16px" ]
                                [ h6 [] [ text "CP" ]
                                , flexRow []
                                    [ minusButton (UpdatePlayer model.currentPlayer << AdjustCombatPoints)
                                    , valueDisplay player.combatPoints
                                    , plusButton (UpdatePlayer model.currentPlayer << AdjustCombatPoints)
                                    ]
                                ]
                            ]
                        , flexRow []
                            [ guideBoard player.character.guideImage
                            , actionBoard player.character.actionImage
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
        flexCol [ style "border-bottom" "solid 1px black", style "border-left" "1px solid black", style "padding" "12px", style "background-color" backgroundColor, onClick (SelectPlayer index) ]
            [ div [] [ text <| "HP: " ++ (String.fromInt player.health) ]
            , div [] [ text <| "CP: " ++ (String.fromInt player.combatPoints) ]
            ]


view : Model -> Html Message
view model =
    flexRow []
        [ div [ style "flex-grow" "1" ]
            [ renderPlayer model
            ]
        , div [ style "flex-basis" "300px" ]
            [ flexCol []
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
    | NewRoll (List Die)
    | ToggleReroll Int
    | SelectAll
    | ToggleAdjust
    | AdjustDie Int Int
    | UpdatePlayer Int PlayerMessage
    | SelectPlayer Int



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
                    List.Extra.updateAt
                        index
                        (\die -> { die | reroll = not die.reroll })
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
                    List.Extra.updateAt
                        index
                        (\die -> { die | result = clamp 1 6 (die.result + amount) })
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
