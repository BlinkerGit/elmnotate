module View exposing (view)

import DropZone exposing (DropZoneMessage, dropZoneEventHandlers)
import FileReader exposing (NativeFile)
import Html exposing (Html, div, nav, span, text, a, canvas)
import Html.Attributes exposing (class, type_, href, downloadAs, style, disabled)
import Html.Events as HEvt
import MouseEvents exposing (onClick)
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
    let
        todo =
            List.length model.pending
        done =
            List.length model.processed
        total =
            todo + done
        msg =
            case total of
                0 -> ""
                _ -> 
                    ((toString (done + 1)) ++ " of " ++ (toString total))
    in
    nav [ class "navbar fixed-top navbar-light bg-light justify-content-between" ]
        [ span [ class "navbar-brand" ]
               [ text "Annotate"]
        , span []
               [ text msg ]
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
           , onClick AddPoint
           ]
           []

footer : Model -> Html Msg
footer model =
    let
        json =
            "{\"some\":\"json\"}"
        encoded =
            encodeUri json
        cantNext =
            List.isEmpty model.pending
        cantPrev =
            List.isEmpty model.processed
    in

        nav [ class "navbar fixed-bottom navbar-light bg-light justify-content-between" ]
            [ div [ class "float-left"]
                  [ a [ class "btn btn-outline-primary btn-sm mr-1"
                      , HEvt.onClick NavPrev
                      , disabled cantPrev
                      ]
                      [ text "Prev" ]
                  , a [ class "btn btn-outline-primary btn-sm"
                      , HEvt.onClick NavNext
                      , disabled cantNext
                      ]
                      [ text "Next" ]
                  ]
            , div [ class "float-right" ]
                  [ a [ class "btn btn-primary btn-sm"
                      , href <| "data:application/json;charset=utf-8," ++ encoded
                      , downloadAs "data.json"
                      ]
                      [ text "Download" ]
                  ]
            ]
