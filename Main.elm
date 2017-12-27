module Main exposing (..)

import Array
import Char
import Html exposing (program)
import Json.Decode exposing (decodeValue, decodeString, array, int)
import Json.Encode exposing (Value)
import Keyboard
import Mouse
import Window

import Model exposing (Model, Point, Offset)
import View exposing (view)
import Update exposing (Msg(..), update)
import Canvas exposing (clientDims)

init : (Model, Cmd Msg)
init =
    (Model.init, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ clientDims (decodeClientDims)
        , Window.resizes (\{height, width} -> WindowResized (Offset width height))
        , Keyboard.presses (\code -> KeyPressed (Char.fromCode code))
        , Mouse.moves (\{x, y} -> MouseMoved (Point x y))
        , Mouse.downs (\{x, y} -> MouseDown (Point x y))
        , Mouse.ups (\{x, y} -> MouseUp (Point x y))
        ]

decodeClientDims : Value -> Msg
decodeClientDims value =
    let
        result =
            decodeValue (array int) value
    in
    case result of
        Ok dims ->
            let
                imgW =
                    Array.get 0 dims |> Maybe.withDefault -1
                imgH =
                    Array.get 1 dims |> Maybe.withDefault -1
                imgSize =
                    Offset imgW imgH
                pnlW =
                    Array.get 2 dims |> Maybe.withDefault -1
                pnlH =
                    Array.get 3 dims |> Maybe.withDefault -1
                pnlSize =
                    Offset pnlW pnlH
            in
            ClientDims imgSize pnlSize
        Err _ ->
            NoOp


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
