module Player exposing (..)

import Character exposing (Character)


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    }
