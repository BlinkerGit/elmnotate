port module Canvas exposing (..)

import Json.Encode exposing (Value)
import Model exposing (Graphics)

-- send points to be rendered
port render : Graphics -> Cmd msg

-- listen for image size
port loadImage : String -> Cmd msg
port imageSize : (Value -> msg) -> Sub msg
