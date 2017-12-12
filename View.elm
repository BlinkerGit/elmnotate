module View exposing (view)

import Html exposing (Html, div, nav, span, text, a)
import Html.Attributes exposing (class, type_, href, downloadAs)
import Http exposing (encodeUri)
import Model exposing (Model)
import Update exposing (Msg)


view : Model -> Html Msg
view model =
    div []
        [ header model
        , body model
        , footer model
        ]


header : Model -> Html Msg
header model =
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ span [ class "navbar-brand" ]
               [ text "Annotate"]

        ]

body : Model -> Html Msg
body model =
    let
        todo =
            List.length model.pending
        done =
            List.length model.processed
    in
    case (todo, done) of
        (0, 0) ->
            getStarted model
        (0, _) ->
            complete model
        (_, _) ->
            inProcess model

getStarted : Model -> Html Msg
getStarted model =
    -- todo: dropzone
    text "to get started, drop a .txt file of URLs, one per line, here"

complete : Model -> Html Msg
complete model =
    text "all done!  download your data below:"

inProcess : Model -> Html Msg
inProcess model =
    text "TODO: canvas w/current image"

footer : Model -> Html Msg
footer model =
    let
        json =
            "{\"some\":\"json\"}"
        encoded =
            encodeUri json
    in

        nav [ class "navbar fixed-bottom navbar-light bg-light justify-content-end" ]
            [ div [ class "float-right" ]
                  [ a [ class "btn btn-primary btn-sm"
                      , href <| "data:application/json;charset=utf-8," ++ encoded
                      , downloadAs "data.json"
                      ]
                      [ text "Download" ]
                  ]
            ]
