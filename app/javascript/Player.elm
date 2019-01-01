module Player exposing (..)

import Character exposing (Character, barbarian, moonElf)


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    }


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



-- update


type Message
    = AdjustHealth Int
    | AdjustCombatPoints Int


update : Message -> Player -> Player
update message player =
    case message of
        AdjustHealth amount ->
            { player | health = clamp 0 99 (player.health + amount) }

        AdjustCombatPoints amount ->
            { player | combatPoints = clamp 0 15 (player.combatPoints + amount) }
