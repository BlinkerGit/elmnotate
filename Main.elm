module Main exposing (..)

import Array
import Html exposing (program)
import Json.Encode exposing (Value)
import Json.Decode exposing (decodeValue, decodeString, array, int)
import Window
import Model exposing (Model, Offset)
import View exposing (view)
import Update exposing (Msg(..), update)
import Canvas exposing (clientDims)

init : (Model, Cmd Msg)
init =
    (Model.init, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    -- todo: add mouse clicks, downs, ups, and moves (if dragging)
    Sub.batch
        [ clientDims (decodeClientDims)
        , Window.resizes (\{height, width} -> WindowResized (Offset width height))
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
