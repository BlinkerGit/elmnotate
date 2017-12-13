module View exposing (view)

import DropZone exposing (DropZoneMessage, dropZoneEventHandlers)
import FileReader exposing (NativeFile)
import Html exposing (Html, div, nav, span, text, a, canvas, button)
import Html.Attributes exposing (class, type_, href, downloadAs, style, disabled, id, width, height)
import Html.Events exposing (onClick)
import MouseEvents as ME
import Http exposing (encodeUri)
import Model exposing (Model, Image, PendingGeometry(..))
import Update exposing (Msg(..))
import Serialization


view : Model -> Html Msg
view model =
    div []
        [ header model
        , body model
        , footer model
        ]


header : Model -> Html Msg
header model =
    let
        todo =
            List.length model.pending
        done =
            List.length model.processed
        total =
            todo + done
        msg =
            case (total, todo) of
                (0, _) -> ""
                (_, 0) -> ""
                (_, _) ->
                    ((toString (done + 1)) ++ " of " ++ (toString total))
    in
    nav [ class "navbar fixed-top navbar-light bg-light justify-content-between" ]
        [ span [ class "navbar-brand" ]
               [ text "Annotate"]
        , maybeShapeButtons (todo > 0)
        , span []
               [ text msg ]
        ]

maybeShapeButtons : Bool -> Html Msg
maybeShapeButtons flag =
    if flag then
        div  []
             [ button [ class "btn btn-primary btn-xs mr-1"
                      , onClick <| NewShape (PendingRect [])
                      ]
                      [ text "R" ]
             , button [ class "btn btn-primary btn-xs"
                      , onClick <| NewShape (PendingQuad [])
                      ]
                      [ text "Q" ]
             ]
    else
        Html.text ""

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
                |> Maybe.withDefault (Image "https://placekitten.com/g/720/540" [])
        url =
            "url(" ++ img.url ++ ")"
    in
    canvas [ style
               [ ( "background-image",   url )
               , ( "background-size",   "contain" )
               , ( "background-repeat", "no-repeat" )
               ]
           , ME.onClick AddPoint
           , id "annotate-canvas"
           , width model.width
           , height model.height
           ]
           []

footer : Model -> Html Msg
footer model =
    let
        json =
            Serialization.toJson model
        encoded =
            encodeUri json
        cantNext =
            List.isEmpty model.pending
        cantPrev =
            List.isEmpty model.processed
    in
    nav [ class "navbar fixed-bottom navbar-light bg-light justify-content-between" ]
        [ div [ class "float-left"]
              [ button [ class "btn btn-outline-primary btn-sm mr-1"
                       , onClick NavPrev
                       , disabled cantPrev
                       ]
                       [ text "Prev" ]
              , button [ class "btn btn-outline-primary btn-sm"
                       , onClick NavNext
                       , disabled cantNext
                       ]
                       [ text "Next" ]
              ]
        , div [ class "float-right" ]
              [ maybeButton cantPrev encoded
              ]
        ]

maybeButton : Bool -> String -> Html Msg
maybeButton disabled json =
    if disabled then
        Html.text ""
    else
        a [ class "btn btn-primary btn-sm"
          , href <| "data:application/json;charset=utf-8," ++ json
          , downloadAs "data.json"
          ]
          [ text "Download" ]
