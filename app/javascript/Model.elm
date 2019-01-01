module Model exposing (..)

import Character exposing (Character)


type alias Die =
    { selected : Bool
    , result : Int
    }


type alias Roll =
    List Die


type RollState
    = SelectingNumber
    | Rolling


type alias Model =
    { roll : Roll
    , rollState : RollState
    , rollCount : Int
    , players : List Player
    , currentPlayer : Int
    }


type alias Player =
    { health : Int
    , combatPoints : Int
    , character : Character
    }
