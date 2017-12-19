module View exposing (view)

import DropZone exposing (DropZoneMessage, dropZoneEventHandlers)
import FileReader exposing (NativeFile)
import Html exposing (Html, div, nav, span, text, a, canvas, button, h6, table, tbody, tr, td, input, hr, ul, li)
import Html.Attributes exposing (class, type_, href, downloadAs, style, disabled, id, width, height, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Http exposing (encodeUri)
import Model exposing (Model, Image, PendingGeometry(..), Shape, Geometry(..), LabelClass)
import Update exposing (Msg(..))
import Serialization


view : Model -> Html Msg
view model =
    div [ class "full-screen" ]
        [ div [ id "header" ]
              [ header model ]
        , div [ id "main" ]
              ( body model )
        , div [ id "footer" ]
              [ footer model ]
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
    nav [ class "navbar navbar-light bg-light justify-content-between" ]
        [ span [ class "navbar-brand" ]
               [ text "Annotate"]
        , span []
               [ text msg ]
        ]

body : Model -> List (Html Msg)
body model =
    let
        todo =
            List.length model.pending
        done =
            List.length model.processed
    in
    case (todo, done) of
        (0, 0) ->
            [getStarted model]
        (0, _) ->
            [complete model]
        (_, _) ->
            inProcess model

getStarted : Model -> Html Msg
getStarted model =
    div [ id "get-started"
        , class "center-pad"
        ]
        [ Html.map DnD
            (div (dzAttributes model.dropZone)
                [ text "To get started, drop a data file here.  Accepted formats are .txt (one URL per line) and .json (with the same structure as the output)."
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
    div [ id "complete"
        , class "center-pad"
        ]
        [ text "all done!  download your data below:"
        ]

inProcess : Model -> List (Html Msg)
inProcess model =
    [ div [ id "canvas-panel" ]
          [ drawing model ]
    , div [ id "sidebar" ]
          [ sidebar model ]
    ]

drawing : Model -> Html Msg
drawing model =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault (Image "https://placekitten.com/g/720/540" [])
        url =
            "url(" ++ img.url ++ ")"
        w =
            (toFloat model.imageSize.w) * model.scale |> round
        h =
            (toFloat model.imageSize.h) * model.scale |> round
        cursor =
            case (model.hoverPoint, model.dragPoint) of
                (Nothing, Nothing) ->
                    "crosshair"
                (_, _) ->
                    "move"
    in
    canvas [ style
               [ ( "background-image",   url )
               , ( "background-size",   "contain" )
               , ( "background-repeat", "no-repeat" )
               , ( "cursor",            cursor )
               ]
           , id "annotate-canvas"
           , width w
           , height h
           ]
           []

sidebar : Model -> Html Msg
sidebar model =
    div []
        [ h6 [] [ text "Classes"]
        , classList model
        , hr [] []
        , h6 [] [ text "Shapes"]
        , shapeList model
        {-
        , table [ class "table table-sm table-bordered table-striped" ]
                [ tbody []
                        (List.indexedMap shapeRow img.shapes)
                ]
        -}
        ]

pGeomLabel : PendingGeometry -> String
pGeomLabel pg =
    case pg of
        NoShape       -> "Shape"
        PendingRect _ -> "Rect"
        PendingQuad _ -> "Quad"

geomLabel : Geometry -> String
geomLabel g =
    case g of
        Rect _ _     -> "Rect"
        Quad _ _ _ _ -> "Quad"

classList : Model -> Html Msg
classList model =
    let
        menuClass =
            if model.pendingClass.active then
                "dropdown-menu show"
            else
                "dropdown-menu"
    in
    ul [ class "list-group" ]
       ( (List.indexedMap classListItem model.labelClasses)
       ++ [ li [ class "list-group-item pending-label form-inline" ]
               [ div [ class "btn-group btn-group-xs mr-2" ]
                     [ button [ class "btn btn-fw btn-outline-primary dropdown-toggle"
                              , onClick ToggleGeomMenu ]
                              [ text (pGeomLabel model.pendingClass.geom) ]
                     , div [ class menuClass ]
                           [ a [ class "dropdown-item"
                               , onClick SelectQuad
                               ]
                               [ text "Quad" ]
                           , a [ class "dropdown-item"
                               , onClick SelectRect
                               ]
                               [ text "Rect" ]
                           ]
                     ]
                , input [ class "form-control form-control-xs mr-2"
                        , value model.pendingClass.label
                        , onInput SetLabel
                        , placeholder "label"
                        ]
                        []
                , button [ class "btn btn-primary btn-xs"
                         , onClick AddLabelClass
                         ]
                         [ text "Add" ]
               ]
       ]
       )

classListItem : Int -> LabelClass -> Html Msg
classListItem index lc =
    let
        buttonClass =
            if lc.active then
                "btn btn-fw btn-xs btn-primary mr-2"
            else
                "btn btn-fw btn-xs btn-outline-primary mr-2"
    in
    li [ class "list-group-item form-inline" ]
       [ button [ class buttonClass
                , onClick (ActivateLabel index)
                ]
                [ text (pGeomLabel lc.geom) ]
       , input [ class "form-control form-control-xs mr-2"
               , placeholder "label"
               , disabled True
               , value lc.label
               ]
               []
       , button [ class "btn btn-outline-danger btn-xs"
                , onClick <| DeleteClass index
                ]
                [ text "Delete" ]
       ]

shapeList : Model -> Html Msg
shapeList model =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault (Image "" [])
    in
    ul [ class "list-group" ]
       (List.indexedMap shapeListItem img.shapes)

shapeListItem : Int -> Shape -> Html Msg
shapeListItem index s =
    let
        buttonClass =
            if s.active then
                "btn btn-fw btn-xs btn-primary mr-2"
            else
                "btn btn-fw btn-xs btn-outline-primary mr-2"
    in
    li [ class "list-group-item form-inline" ]
       [ button [ class buttonClass
                , onClick (ActivateShape index)
                ]
                [ text (geomLabel s.geom) ]
       , input [ class "form-control form-control-xs mr-2"
               , placeholder "label"
               , disabled True
               , value s.label
               ]
               []
       , button [ class "btn btn-outline-danger btn-xs"
                , onClick <| DeleteShape index
                ]
                [ text "Delete" ]
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
