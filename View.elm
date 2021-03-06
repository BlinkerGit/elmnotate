module View exposing (view)

import Dialog
import Dict
import File
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , canvas
        , div
        , h6
        , input
        , li
        , nav
        , option
        , select
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , disabled
        , height
        , id
        , placeholder
        , rows
        , selected
        , style
        , value
        , width
        )
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Model
    exposing
        ( Geometry(..)
        , LabelClass
        , LabelType(..)
        , Model
        , PendingGeometry(..)
        , Shape
        , initImage
        )
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ class "full-screen" ]
        [ div [ id "header" ]
            [ header model ]
        , div [ id "main" ]
            (body model)
        , div [ id "footer" ]
            [ footer model ]
        , maybeShowModal model
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
            case ( total, todo ) of
                ( 0, _ ) ->
                    ""

                ( _, 0 ) ->
                    ""

                ( _, _ ) ->
                    String.fromInt (done + 1) ++ " of " ++ String.fromInt total
    in
    nav [ class "navbar navbar-light bg-light justify-content-between" ]
        [ span [ class "navbar-brand" ]
            [ text "Elmnotate" ]
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
    case ( todo, done ) of
        ( 0, 0 ) ->
            [ getStarted model ]

        ( 0, _ ) ->
            [ complete model ]

        ( _, _ ) ->
            inProcess model


maybeShowModal : Model -> Html Msg
maybeShowModal model =
    let
        shouldDisplay =
            not (String.isEmpty model.editingSelectName)
    in
    Dialog.view
        (if shouldDisplay then
            Just (dialogConfig model)

         else
            Nothing
        )


dialogConfig : Model -> Dialog.Config Msg
dialogConfig model =
    let
        headerText =
            "Editing options for select '" ++ model.editingSelectName ++ "'"
    in
    { closeMessage = Just CancelDialog
    , containerClass = Nothing
    , header = Just (h6 [ style "position" "absolute" ] [ text headerText ])
    , body = Just (optionEditor model)
    , footer =
        [ button
            [ class "btn btn-primary"
            , onClick SaveEditingSelect
            ]
            [ text "OK" ]
        ]
    }


optionEditor : Model -> Html Msg
optionEditor model =
    div []
        [ text "Enter options, one per line:"
        , textarea
            [ class "form-control"
            , rows 10
            , value model.editingSelectOptions
            , onInput UpdateSelectOptions
            ]
            []
        ]


getStarted : Model -> Html Msg
getStarted model =
    let
        color =
            if model.draggingFiles then
                "blue"

            else
                "steelblue"

        dropZoneStyle =
            [ style "height" "120px"
            , style "border-radius" "10px"
            , style "border" ("3px dashed " ++ color)
            ]
    in
    div
        (dropZoneStyle
            ++ [ id "get-started"
               , class "center-pad"
               , hijackOn "dragenter" (D.succeed FileDragEnter)
               , hijackOn "dragover" (D.succeed FileDragEnter)
               , hijackOn "dragleave" (D.succeed FileDragLeave)
               , hijackOn "drop" dropDecoder
               ]
        )
        [ div
            []
            [ text "To get started, drop a data file here.  Accepted formats are .txt (one URL per line) and .json (with the same structure as the output)."
            ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore FileDrop File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


complete : Model -> Html Msg
complete model =
    let
        displayText =
            if model.resetRequested then
                "WARNING, this will clear all data.  Are you sure?"

            else
                "All done!  Download your data below."
    in
    div
        [ id "complete"
        , class "center-pad"
        ]
        [ text displayText
        , br [] []
        , maybeCancelResetButton model
        , button
            [ class "btn btn-large btn-danger"
            , onClick RequestReset
            ]
            [ text "Start Over" ]
        ]


maybeCancelResetButton : Model -> Html Msg
maybeCancelResetButton model =
    if model.resetRequested then
        button
            [ class "btn btn-large btn-outline-primary mr-2"
            , onClick CancelReset
            ]
            [ text "Cancel" ]

    else
        text ""


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
                |> Maybe.withDefault initImage

        url =
            "url(" ++ img.url ++ ")"

        w =
            toFloat model.imageSize.w * model.scale |> round

        h =
            toFloat model.imageSize.h * model.scale |> round

        cursor =
            case ( model.hoverPoint, model.dragPoint ) of
                ( Nothing, Nothing ) ->
                    "crosshair"

                ( _, _ ) ->
                    "move"
    in
    canvas
        [ style "background-image" url
        , style "background-size" "contain"
        , style "background-repeat" "no-repeat"
        , style "cursor" cursor
        , id "annotate-canvas"
        , width w
        , height h
        , Mouse.onDown (\event -> MouseDown event.offsetPos)
        , Mouse.onMove (\event -> MouseMoved event.offsetPos)
        , Mouse.onUp (\event -> MouseUp event.offsetPos)
        ]
        []


sidebar : Model -> Html Msg
sidebar model =
    div []
        [ classList model
        , shapeList model
        , labelList model
        ]


pGeomLabel : PendingGeometry -> String
pGeomLabel pg =
    case pg of
        PendingLabel ->
            "Label"

        PendingDropDown ->
            "Select"

        PendingRect _ ->
            "Rect"

        PendingQuad _ ->
            "Quad"


geomLabel : Geometry -> String
geomLabel g =
    case g of
        Rect _ _ ->
            "Rect"

        Quad _ _ _ _ ->
            "Quad"


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
        (li [ class "list-group-item header" ] [ text "Classes" ]
            :: (List.indexedMap classListItem model.labelClasses
                    ++ [ li [ class "list-group-item pending-label form-inline" ]
                            [ div [ class "btn-group btn-group-xs mr-2" ]
                                [ button
                                    [ class "btn btn-fw btn-outline-primary dropdown-toggle"
                                    , onClick ToggleGeomMenu
                                    ]
                                    [ text <| pGeomLabel model.pendingClass.geom ]
                                , div [ class menuClass ]
                                    [ a
                                        [ class "dropdown-item"
                                        , onClick SelectQuad
                                        ]
                                        [ text "Quad" ]
                                    , a
                                        [ class "dropdown-item"
                                        , onClick SelectRect
                                        ]
                                        [ text "Rect" ]
                                    , a
                                        [ class "dropdown-item"
                                        , onClick SelectLabel
                                        ]
                                        [ text "Label" ]
                                    , a
                                        [ class "dropdown-item"
                                        , onClick SelectDropDown
                                        ]
                                        [ text "Select" ]
                                    ]
                                ]
                            , input
                                [ class "form-control form-control-xs mr-2"
                                , value model.pendingClass.name
                                , onInput SetLabelClassName
                                , placeholder "label"
                                ]
                                []
                            , button
                                [ class "btn btn-primary btn-xs"
                                , onClick AddLabelClass
                                ]
                                [ text "Add" ]
                            ]
                       ]
               )
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
        [ case lc.geom of
            PendingLabel ->
                span [ class "btn btn-xs btn-fw mr-2" ]
                    [ text "Label" ]

            PendingDropDown ->
                button
                    [ class buttonClass
                    , onClick (EditSelectOptions lc.name)
                    ]
                    [ text "Select" ]

            g ->
                button
                    [ class buttonClass
                    , onClick (ActivateLabel index)
                    ]
                    [ text (pGeomLabel g) ]
        , input
            [ class "form-control form-control-xs mr-2"
            , placeholder "label"
            , disabled True
            , value lc.name
            ]
            []
        , button
            [ class "btn btn-outline-danger btn-xs"
            , onClick <| DeleteClass index
            ]
            [ text "Delete" ]
        ]


shapeList : Model -> Html Msg
shapeList model =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault initImage
    in
    ul [ class "list-group" ]
        (li [ class "list-group-item header" ] [ text "Shapes" ]
            :: List.indexedMap shapeListItem img.shapes
        )


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
        [ button
            [ class buttonClass
            , onClick (ActivateShape index)
            ]
            [ text (geomLabel s.geom) ]
        , input
            [ class "form-control form-control-xs mr-2"
            , placeholder "label"
            , disabled True
            , value s.label
            ]
            []
        , button
            [ class "btn btn-outline-danger btn-xs"
            , onClick <| DeleteShape index
            ]
            [ text "Delete" ]
        ]


labelList : Model -> Html Msg
labelList model =
    if List.isEmpty model.labelClasses then
        text ""

    else
        ul [ class "list-group" ]
            (li [ class "list-group-item header" ] [ text "Labels" ]
                :: List.map (labelListItem model) model.labelClasses
            )


labelListItem : Model -> LabelClass -> Html Msg
labelListItem model labelClass =
    let
        img =
            List.head model.pending
                |> Maybe.withDefault initImage

        key =
            labelClass.name

        val =
            Dict.get key img.labels
                |> Maybe.withDefault ""
    in
    case labelClass.geom of
        PendingLabel ->
            li [ class "list-group-item form-inline" ]
                [ span [ class "btn btn-xs btn-fw mr-2" ]
                    [ text key ]
                , labelListItemInput key val
                ]

        PendingDropDown ->
            li [ class "list-group-item form-inline" ]
                [ span [ class "btn btn-xs btn-fw mr-2" ]
                    [ text key ]
                , labelListItemDropDown key val model.metaData.dropdowns
                ]

        _ ->
            text ""


labelListItemInput : String -> String -> Html Msg
labelListItemInput key val =
    input
        [ class "form-control form-control-xs mr-2"
        , placeholder "value"
        , value val
        , onInput (SetImageLabel key)
        ]
        []


labelListItemDropDown : String -> String -> Dict.Dict String (List String) -> Html Msg
labelListItemDropDown key currentValue dropdown_data =
    select
        [ class "form-control form-control-xs mr-2"
        , onInput (SetImageLabel key)
        ]
        (case Dict.get key dropdown_data of
            Nothing ->
                []

            Just optionList ->
                makeOption currentValue "" :: List.map (makeOption currentValue) optionList
        )


makeOption : String -> String -> Html Msg
makeOption currentValue optionValue =
    option
        [ value optionValue
        , selected (currentValue == optionValue)
        ]
        [ text optionValue ]


footer : Model -> Html Msg
footer model =
    let
        cantNext =
            List.isEmpty model.pending

        cantPrev =
            List.isEmpty model.processed
    in
    nav [ class "navbar fixed-bottom navbar-light bg-light justify-content-between" ]
        [ div [ class "float-left" ]
            [ button
                [ class "btn btn-outline-primary btn-sm mr-1"
                , onClick NavPrev
                , disabled cantPrev
                ]
                [ text "Prev" ]
            , button
                [ class "btn btn-outline-primary btn-sm"
                , onClick NavNext
                , disabled cantNext
                ]
                [ text "Next" ]
            ]
        , div [ class "float-right" ]
            [ maybeButton cantPrev
            ]
        ]


maybeButton : Bool -> Html Msg
maybeButton disabled =
    if disabled then
        Html.text ""

    else
        a
            [ class "btn btn-primary btn-sm"
            , onClick DownloadData
            ]
            [ text "Download" ]
