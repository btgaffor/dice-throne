port module Websocket exposing (..)

import Update exposing (Message(..))


port setFromServer : (Int -> msg) -> Sub msg
