module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h6, text, button, img)
import Html.Attributes exposing (style, class, src, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import List.Extra
import Json.Decode
import Http
import Update exposing (Message(..), update)
import Model exposing (Model, Die, RollState(..))
import Player exposing (Player, initialPlayerOne, initialPlayerTwo)
import Dice exposing (renderDiceSection)
import Websocket exposing (..)


-- INIT


getSave : Cmd Message
getSave =
    Http.send GotText <| Http.get "http://localhost:5000/save" Json.Decode.string


init : ( Model, Cmd Message )
init =
    ( Model [] SelectingNumber 0 [ initialPlayerOne, initialPlayerTwo ] 0, Cmd.none )



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
                    [ renderDiceSection model player.character.dieIcons
                    , Player.renderStats player |> Html.map (UpdatePlayer model.currentPlayer)
                    , Player.renderBoards player
                    ]
            )
        |> Maybe.withDefault (text "")


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



-- MAIN


main : Program (Maybe {}) Model Message
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
