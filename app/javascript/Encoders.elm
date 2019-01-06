module Encoders exposing (..)

import Json.Encode as JE
import Json.Decode as JD
import Model exposing (Model, Roll, RollState(..))
import Player exposing (Player)
import Character exposing (Character)


encodeModel : Model -> JE.Value
encodeModel model =
    JE.object
        [ ( "authenticity_token", JE.string model.csrfToken )
        , ( "game"
          , JE.object
                [ ( "roll", encodeRoll model.roll )
                , ( "rollState", encodeRollState model.rollState )
                , ( "rollCount", JE.int model.rollCount )
                , ( "players", encodePlayers model.players )
                , ( "currentPlayer", JE.int model.currentPlayer )
                ]
          )
        ]


encodeRoll : Roll -> JE.Value
encodeRoll roll =
    JE.list
        (\die -> JE.object [ ( "selected", JE.bool die.selected ), ( "result", JE.int die.result ) ])
        roll


encodeRollState : RollState -> JE.Value
encodeRollState rollState =
    case rollState of
        SelectingNumber ->
            JE.string "SelectingNumber"

        Rolling ->
            JE.string "Rolling"


encodePlayers : List Player -> JE.Value
encodePlayers players =
    JE.list
        (\player ->
            JE.object
                [ ( "health", JE.int player.health )
                , ( "combatPoints", JE.int player.combatPoints )
                , ( "character", encodeCharacter player.character )
                ]
        )
        players


encodeCharacter : Character -> JE.Value
encodeCharacter character =
    JE.object
        [ ( "guideImage", JE.string character.guideImage )
        , ( "actionImage", JE.string character.actionImage )
        , ( "dieIcons", JE.array JE.string character.dieIcons )
        ]
