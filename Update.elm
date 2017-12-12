module Update exposing (..)

import DropZone exposing (DropZoneMessage(..))
import FileReader exposing (NativeFile)
import MouseEvents
import Task
import Model exposing (Model, Image)

type Msg
    = NoOp
    | DnD (DropZoneMessage (List NativeFile))
    | OnFileContent (Result FileReader.Error String)
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
            in
            ( { model | pending = pending }
            , Cmd.none
            )
        OnFileContent (Err error) ->
            (model, Cmd.none)
        AddPoint mouse ->
            let
                log =
                    Debug.log "AddPoint" mouse
            in
            (model, Cmd.none)
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
            in
            ( { model | pending = pending, processed = processed }
            , Cmd.none
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
            in
            ( { model | pending = pending, processed = processed }
            , Cmd.none
            )

uploadHandler : NativeFile -> Cmd Msg
uploadHandler file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt OnFileContent
