module Main exposing (..)

import Html exposing (program)
import Model exposing (Model)
import View exposing (view)
import Update exposing (Msg(..), update)

init : (Model, Cmd Msg)
init =
    (Model [] [], Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
