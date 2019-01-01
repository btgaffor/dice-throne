module Model exposing (..)

import Player exposing (Player)


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
