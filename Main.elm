module Main exposing (main)

import Array
import Browser
import Browser.Events as BrowserEvents
import Canvas exposing (clientDims, navNext, navPrev)
import Json.Decode exposing (array, decodeValue, int)
import Json.Encode exposing (Value)
import Model exposing (Model, Offset)
import Update exposing (Msg(..), update)
import View exposing (view)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ clientDims decodeClientDims
        , BrowserEvents.onResize (\width height -> WindowResized (Offset width height))
        , navNext (\_ -> NavNext)
        , navPrev (\_ -> NavPrev)
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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Model.init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
