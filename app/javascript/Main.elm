module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img)
import Html.Attributes exposing (style, class, src, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Update exposing (Message(..), update)
import Model exposing (Model, Die, RollState(..))
import Player exposing (Player, initialPlayerOne, initialPlayerTwo)
import Dice exposing (renderDiceSection)


-- INIT


init : ( Model, Cmd Message )
init =
    ( Model [] SelectingNumber 0 [ initialPlayerOne, initialPlayerTwo ] 0, Cmd.none )



-- VIEW


renderPlayer : Model -> Html Message
renderPlayer model =
    div []
        (List.indexedMap
            (\index player ->
                if index == model.currentPlayer then
                    div [ class "flex-col" ]
                        [ renderDiceSection model player.character.dieIcons
                        , Player.renderStats player |> Html.map (UpdatePlayer model.currentPlayer)
                        , Player.renderBoards player
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
            [ div [ class "flex-col" ] (List.indexedMap (renderPlayerSidebarItem model.currentPlayer) model.players)
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
