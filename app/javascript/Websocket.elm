port module Websocket exposing (..)

import Json.Decode
import Update exposing (Message(..))


port setFromServer : (Json.Decode.Value -> msg) -> Sub msg
