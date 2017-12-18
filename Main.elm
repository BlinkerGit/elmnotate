module Main exposing (..)

import Array
import Html exposing (program)
import Json.Encode exposing (Value)
import Json.Decode exposing (decodeValue, decodeString, array, int)
import Window
import Model exposing (Model, Offset)
import View exposing (view)
import Update exposing (Msg(..), update)
import Canvas exposing (imageSize)

init : (Model, Cmd Msg)
init =
    (Model.init, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    -- todo: add mouse clicks, downs, ups, and moves (if dragging)
    Sub.batch
        [ imageSize (decodeImageSize)
        , Window.resizes (\{height, width} -> WindowResized (Offset width height))
        ]

decodeImageSize : Value -> Msg
decodeImageSize value =
    let
        result =
            decodeValue (array int) value
    in
    case result of
        Ok dims ->
            let
                w =
                    Array.get 0 dims |> Maybe.withDefault -1
                h =
                    Array.get 1 dims |> Maybe.withDefault -1
                offset =
                    Offset w h
            in
            ImageSize offset
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
