port module Canvas exposing (..)

import Json.Encode exposing (Value)
import Model exposing (Graphics)

port render : Graphics -> Cmd msg
port loadImage : String -> Cmd msg
port clientDims : (Value -> msg) -> Sub msg
port navNext : (Value -> msg) -> Sub msg
port navPrev : (Value -> msg) -> Sub msg
