module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img, span)
import Html.Attributes exposing (style, class, src, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import List.Extra
import Json.Decode as JD
import Http
import Dict
import Update exposing (Message(..), update)
import Model exposing (Model, Die, RollState(..))
import Player exposing (Player, initialPlayerOne, initialPlayerTwo)
import Dice exposing (renderDiceSection)
import Websocket exposing (..)
import Encoders exposing (decodeModel)


-- INIT


init : { csrfToken : String } -> ( Model, Cmd Message )
init flags =
    ( Model flags.csrfToken [] SelectingNumber 0 [ initialPlayerOne, initialPlayerTwo ] 0 False, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    div [ class "flex-row" ]
        [ div [ style "flex-grow" "1" ] [ playArea model ]
        , div [ style "flex-basis" "250px" ] [ playersSidebar model ]
        ]


playArea model =
    List.Extra.getAt model.currentPlayer model.players
        |> Maybe.map
            (\player ->
                div [ class "flex-col" ]
                    [ div [ class "flex-row" ]
                        [ div []
                            [ renderDiceSection model player.character.dieIcons
                            , Player.renderStats player |> Html.map (UpdatePlayer model.currentPlayer)
                            ]
                        , div [ class "flex-col status-effects" ]
                            [ button [ onClick OpenStatusEffectsModal ] [ text "Adjust Status Effects" ]
                            , div [] <|
                                Dict.foldl
                                    (\statusEffect count memo ->
                                        if count > 0 then
                                            div [] [ text <| statusEffect ++ ": " ++ (String.fromInt count) ] :: memo
                                        else
                                            memo
                                    )
                                    []
                                    player.statusEffects
                            ]
                        ]
                    , Player.renderBoards player
                    , renderStatusEffectsModal player model
                    ]
            )
        |> Maybe.withDefault (text "")


renderStatusEffectsModal : Player -> Model -> Html Message
renderStatusEffectsModal player model =
    if model.statusEffectsModalOpen then
        div [ class "status-effects-modal" ] <|
            [ button [ class "modal-close", onClick CloseStatusEffectsModal ] [ text "Close" ]
            , div [] <|
                Dict.foldl
                    (\statusEffect count memo ->
                        div [ class "status-effect-row" ]
                            [ button
                                [ disabled (count <= 0)
                                , class "status-effect-subtract"
                                , onClick (UpdatePlayer model.currentPlayer (Player.AdjustStatusEffect statusEffect -1))
                                ]
                                [ text "-" ]
                            , button [ class "status-effect-add", onClick (UpdatePlayer model.currentPlayer (Player.AdjustStatusEffect statusEffect 1)) ] [ text "+" ]
                            , span [ class "status-effect-text" ] [ text <| statusEffect ++ ": " ++ (String.fromInt count) ]
                            ]
                            :: memo
                    )
                    []
                    player.statusEffects
            ]
    else
        text ""


playersSidebar model =
    div [ class "flex-col" ] (List.indexedMap (renderPlayerSidebarItem model.currentPlayer) model.players)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    setFromServer SetFromServer



-- setFromServer (\jsonValue -> SetFromServer <| JD.decodeValue (decodeModel model.csrfToken) jsonValue)
-- MAIN


main : Program { csrfToken : String } Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
