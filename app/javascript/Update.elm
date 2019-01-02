module Update exposing (..)

import Random
import List.Extra
import Result
import Http
import Json.Decode
import Model exposing (Model, Die, RollState(..))
import Player


type Message
    = DoRoll
    | RollResult (List Die)
    | ToggleSelected Int
    | SelectAll
    | SelectNone
    | AdjustDie Int Int
    | IncreaseSelectedDice
    | DecreaseSelectedDice
    | UpdatePlayer Int Player.Message
    | SelectPlayer Int
    | SelectDiceAmount Int
    | NewRoll
    | SetFromServer Int
    | GotText (Result Http.Error String)
    | DoSave


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


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DoRoll ->
            ( model, Random.generate RollResult <| rollGenerator (rerollCount model.roll) )

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
            ( { model | players = List.Extra.updateAt playerIndex (Player.update playerMessage) model.players }
            , Cmd.none
            )

        SelectPlayer selectedPlayer ->
            ( { model | currentPlayer = selectedPlayer }, Cmd.none )

        -- immediately roll with the number of dice selected
        SelectDiceAmount number ->
            ( { model | rollState = Rolling }, Random.generate RollResult (rollGenerator number) )

        NewRoll ->
            ( { model | rollState = SelectingNumber, roll = [], rollCount = 0 }, Cmd.none )

        SetFromServer n ->
            let
                _ =
                    Debug.log "SetFromServer" n
            in
                ( model, Cmd.none )

        GotText result ->
            case result of
                Ok text ->
                    let
                        _ =
                            Debug.log "result" text
                    in
                        ( model, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "error" err
                    in
                        ( model, Cmd.none )

        DoSave ->
            ( model, Http.send GotText <| Http.get "http://localhost:5000/save" Json.Decode.string )
