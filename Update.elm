module Update exposing (..)

import DropZone exposing (DropZoneMessage(..))
import FileReader exposing (NativeFile)
import MouseEvents
import Task
import Model exposing (Model, Image, Point, Offset)
import Canvas exposing (renderPoints, loadImage)

type Msg
    = NoOp
    | DnD (DropZoneMessage (List NativeFile))
    | OnFileContent (Result FileReader.Error String)
    | ImageSize Offset
    | AddPoint MouseEvents.MouseEvent
    | NavPrev
    | NavNext

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        DnD (Drop files) ->
            ( { model | dropZone = DropZone.update (Drop files) model.dropZone }
            , Cmd.batch <| List.map uploadHandler files
            )
        DnD msg ->
            (model, Cmd.none)
        OnFileContent (Ok content) ->
            let
                urls =
                    String.trim content
                        |> String.split("\n")
                new_images =
                    List.map (\u -> Image u []) urls
                pending =
                    new_images ++ model.pending
                -- TODO dedupe?
                cmd =
                    loadImageCmd pending
            in
            ( { model | pending = pending }
            , cmd
            )
        OnFileContent (Err error) ->
            (model, Cmd.none)
        ImageSize point ->
            (model, Cmd.none)
        AddPoint mouse ->
            (model, renderPoints [Point 10 10])
        NavPrev ->
            let
                processed =
                    List.drop 1 model.processed
                img =
                    List.head model.processed
                pending =
                    case img of
                        Nothing ->
                            model.pending
                        Just i ->
                            i :: model.pending
                cmd =
                    loadImageCmd pending
            in
            ( { model | pending = pending, processed = processed }
            , cmd
            )
        NavNext ->
            let
                pending =
                    List.drop 1 model.pending
                img =
                    List.head model.pending
                processed =
                    case img of
                        Nothing ->
                            model.processed
                        Just i ->
                            i :: model.processed
                cmd =
                    loadImageCmd pending
            in
            ( { model | pending = pending, processed = processed }
            , cmd
            )

loadImageCmd : List Image -> Cmd Msg
loadImageCmd pending =
    List.head pending
        |> Maybe.map (\i -> loadImage i.url)
        |> Maybe.withDefault Cmd.none

uploadHandler : NativeFile -> Cmd Msg
uploadHandler file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt OnFileContent
