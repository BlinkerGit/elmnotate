module View exposing (view)

import DropZone exposing (DropZoneMessage, dropZoneEventHandlers)
import FileReader exposing (NativeFile)
import Html exposing (Html, div, nav, span, text, a, canvas, button, h6, table, tr, td, input)
import Html.Attributes exposing (class, type_, href, downloadAs, style, disabled, id, width, height)
import Html.Events exposing (onClick)
import MouseEvents as ME
import Http exposing (encodeUri)
import Model exposing (Model, Image, PendingGeometry(..), Shape, Geometry(..))
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
        , maybeShapeButtons (todo > 0) model.pendingGeom
        , span []
               [ text msg ]
        ]

maybeShapeButtons : Bool -> PendingGeometry -> Html Msg
maybeShapeButtons flag pend =
    if flag then
        let
            baseClass =
                "btn btn-sm mr-1 "
            rectClass =
                case pend of
                    PendingRect _ -> baseClass ++ "btn-primary"
                    _ -> baseClass ++ "btn-outline-primary"
            quadClass =
                case pend of
                    PendingQuad _ -> baseClass ++ "btn-primary"
                    _ -> baseClass ++ "btn-outline-primary"
        in
        div  []
             [ button [ class rectClass
                      , onClick <| NewShape (PendingRect [])
                      ]
                      [ text "R" ]
             , button [ class quadClass
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
    div [ class "center-pad" ]
        [ Html.map DnD
            (div (dzAttributes model.dropZone)
                [ text "to get started, drop a .txt file of URLs, one per line, here"
                ]
            )
        ]

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
    div [ class "center-pad" ]
        [ text "all done!  download your data below:"
        ]

inProcess : Model -> Html Msg
inProcess model =
    div [ class "row" ]
        [ div [ class "col-8 drawing-panel" ]
              [ drawing model
              ]
        , div [ class "col-4 shape-list-panel" ]
              [ shapeList model ]
        ]

drawing : Model -> Html Msg
drawing model =
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

shapeList : Model -> Html Msg
shapeList model =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault (Image "" [])
    in
    div []
        [ h6 [] [ text "Shapes"]
        , table [ class "table table-sm table-bordered table-striped" ]
                (List.indexedMap shapeRow img.shapes)
        ]

shapeRow : Int -> Shape -> Html Msg
shapeRow index s =
    let
        geomType s =
            case s of
                Rect _ _-> "rect"
                Quad _ _ _ _ -> "quad"
    in
    tr []
       [ td []
            [ text <| geomType s.geom
            , maybeConvertButton s.geom index
            ]
       , td []
            [ input [ class "form-control" ]
                    []
            ]
       , td []
            [ button [ class "btn btn-danger btn-sm"
                     , onClick <| DeleteShape index
                     ]
                     [ text "Delete" ]
            ]
       ]

maybeConvertButton : Geometry -> Int -> Html Msg
maybeConvertButton g index =
    case g of
        Rect _ _ ->
            a [ class "btn btn-warning btn-sm"
              , onClick (ConvertRect index)
              ]
              [ text "convert" ]
        Quad _ _ _ _ ->
            text ""

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
