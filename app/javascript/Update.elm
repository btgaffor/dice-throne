module Update exposing (..)

import Random
import List.Extra
import Result
import Http
import Json.Encode as JE
import Json.Decode as JD
import Model exposing (Model, Roll, Die, RollState(..))
import Player
import Encoders exposing (encodeModel, decodeModel)


type Message
    = DoRoll
    | RollResult (List Die)
    | ToggleSelected Int
    | SelectAll
    | SelectNone
    | IncreaseSelectedDice
    | DecreaseSelectedDice
    | UpdatePlayer Int Player.Message
    | SelectPlayer Int
    | SelectDiceAmount Int
    | NewRoll
    | SetFromServer JD.Value
    | GotText (Result Http.Error ())


rollDie : Random.Generator Die
rollDie =
    Random.map (Die False) <| Random.int 1 6


rollGenerator : Int -> Random.Generator (List Die)
rollGenerator rollCount =
    Random.list rollCount rollDie


rerollCount dice =
    List.length <| List.filter .selected dice


notRerolledDice dice =
    List.filter (not << .selected) dice


save model =
    ( model, Http.send GotText <| Http.post "save" (Http.jsonBody <| encodeModel model) (JD.succeed ()) )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DoRoll ->
            ( model, Random.generate RollResult <| rollGenerator (rerollCount model.roll) )

        RollResult rollResult ->
            { model
                | roll = List.sortBy .result (List.concat [ (notRerolledDice model.roll), rollResult ])
                , rollCount =
                    if List.isEmpty rollResult then
                        model.rollCount
                    else
                        model.rollCount + 1
            }
                |> save

        ToggleSelected index ->
            { model
                | roll =
                    List.Extra.updateAt
                        index
                        (\die -> { die | selected = not die.selected })
                        model.roll
            }
                |> save

        SelectAll ->
            { model
                | roll = List.map (\die -> { die | selected = True }) model.roll
            }
                |> save

        SelectNone ->
            { model
                | roll = List.map (\die -> { die | selected = False }) model.roll
            }
                |> save

        IncreaseSelectedDice ->
            { model
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
                |> save

        DecreaseSelectedDice ->
            { model
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
                |> save

        UpdatePlayer playerIndex playerMessage ->
            { model | players = List.Extra.updateAt playerIndex (Player.update playerMessage) model.players } |> save

        SelectPlayer selectedPlayer ->
            { model | currentPlayer = selectedPlayer } |> save

        -- immediately roll with the number of dice selected
        SelectDiceAmount number ->
            ( { model | rollState = Rolling }, Random.generate RollResult (rollGenerator number) )

        NewRoll ->
            { model | rollState = SelectingNumber, roll = [], rollCount = 0 } |> save

        SetFromServer result ->
            case JD.decodeValue (decodeModel model.csrfToken) result of
                Ok newModel ->
                    ( newModel, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "SetFromServer error" error
                    in
                        ( model, Cmd.none )

        GotText result ->
            case result of
                Ok text ->
                    ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )
