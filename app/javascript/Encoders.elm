module Encoders exposing (..)

import Json.Encode as JE
import Json.Decode as JD
import Model exposing (Model, Roll, RollState(..), Die)
import Player exposing (Player, StatusEffects)
import Character exposing (Character)


-- encode


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
                , ( "statusEffectsModalOpen", JE.bool model.statusEffectsModalOpen )
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
                , ( "statusEffects", JE.dict identity JE.int player.statusEffects )
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



-- decode


decodeModel : String -> JD.Decoder Model
decodeModel csrfToken =
    JD.map7
        Model
        (JD.succeed csrfToken)
        (JD.at [ "roll" ] decodeRoll)
        (JD.at [ "rollState" ] decodeRollState)
        (JD.at [ "rollCount" ] JD.int)
        (JD.at [ "players" ] decodePlayers)
        (JD.at [ "currentPlayer" ] JD.int)
        (JD.at [ "statusEffectsModalOpen" ] JD.bool)


decodeRoll : JD.Decoder Roll
decodeRoll =
    JD.list <| JD.map2 Die (JD.at [ "selected" ] JD.bool) (JD.at [ "result" ] JD.int)


decodeRollState : JD.Decoder RollState
decodeRollState =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "SelectingNumber" ->
                        JD.succeed SelectingNumber

                    "Rolling" ->
                        JD.succeed Rolling

                    other ->
                        JD.fail <| "Unknown roll state: " ++ other
            )


decodePlayers : JD.Decoder (List Player)
decodePlayers =
    JD.list <|
        JD.map4
            Player
            (JD.at [ "health" ] JD.int)
            (JD.at [ "combatPoints" ] JD.int)
            (JD.at [ "character" ] decodeCharacter)
            (JD.at [ "statusEffects" ] (JD.dict JD.int))


decodeCharacter : JD.Decoder Character
decodeCharacter =
    JD.map3
        Character
        (JD.at [ "guideImage" ] JD.string)
        (JD.at [ "actionImage" ] JD.string)
        (JD.at [ "dieIcons" ] <| JD.array JD.string)
