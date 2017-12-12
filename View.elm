module View exposing (view)

import DropZone exposing (DropZoneMessage, dropZoneEventHandlers)
import FileReader exposing (NativeFile)
import Html exposing (Html, div, nav, span, text, a, canvas)
import Html.Attributes exposing (class, type_, href, downloadAs, style)
import Http exposing (encodeUri)
import Model exposing (Model, Image)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ header model
        , body model
        , footer model
        ]


header : Model -> Html Msg
header model =
    nav [ class "navbar fixed-top navbar-light bg-light" ]
        [ span [ class "navbar-brand" ]
               [ text "Annotate"]

        ]

body : Model -> Html Msg
body model =
    div [ class "body-content" ]
        [ bodyContent model ]

bodyContent : Model -> Html Msg
bodyContent model =
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
    Html.map DnD
        (div (dzAttributes model.dropZone)
             [ text "to get started, drop a .txt file of URLs, one per line, here"
             ]
        )

dzAttributes : DropZone.Model -> List (Html.Attribute (DropZoneMessage (List NativeFile)))
dzAttributes dropZoneModel =
    (if DropZone.isHovering dropZoneModel then
        dropZoneHover
     else
        dropZoneDefault
    ) :: dropZoneEventHandlers FileReader.parseDroppedFiles

dropZoneHover : Html.Attribute a
dropZoneHover =
    style
        [ ( "height", "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed blue" )
        ]

dropZoneDefault : Html.Attribute a
dropZoneDefault =
    style
        [ ( "height", "120px" )
        , ( "border-radius", "10px" )
        , ( "border", "3px dashed steelblue" )
        ]

complete : Model -> Html Msg
complete model =
    text "all done!  download your data below:"

inProcess : Model -> Html Msg
inProcess model =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault (Image "http://placekitten.com" [])
    in
    canvas [ style
               [ ( "background-image", ("url(" ++ img.url ++ ")") )
               , ( "background-size", "contain" )
               , ( "background-repeat", "no-repeat" )
               ]
           ]
           []

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
