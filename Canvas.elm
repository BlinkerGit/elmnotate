port module Canvas exposing (..)

import Json.Encode exposing (Value)
import Model exposing (Point)

-- send points to be rendered
port renderPoints : (List Point) -> Cmd msg
port renderLines : (List Point) -> Cmd msg

-- listen for image size
port loadImage : String -> Cmd msg
port imageSize : (Value -> msg) -> Sub msg
