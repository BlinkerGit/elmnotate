module Update exposing (..)

import DropZone exposing (DropZoneMessage(..))
import FileReader exposing (NativeFile)
import MouseEvents
import Task
import Model exposing (Model, Image, Point, Offset, Shape, Geometry(..), PendingGeometry(..), graphics)
import Canvas exposing (render, loadImage)

type Msg
    = NoOp
    | DnD (DropZoneMessage (List NativeFile))
    | OnFileContent (Result FileReader.Error String)
    | ImageSize Offset
    | NewShape PendingGeometry
    | DeleteShape Int
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
        ImageSize offset ->
            ( { model | width = offset.w, height = offset.h }
            , render <| graphics model
            )
        NewShape s ->
            ( { model | pendingGeom = s }
            , render <| graphics model
            )
        DeleteShape index ->
            let
                img_ =
                    List.head model.pending
                        |> Maybe.withDefault (Image "" [])
                h =
                    List.take index img_.shapes
                t =
                    List.drop (index + 1) img_.shapes
                s =
                    h ++ t
                img =
                    { img_ | shapes = s }
                pending =
                    img :: (List.drop 1 model.pending)
                updated =
                    { model | pending = pending }
            in
            ( updated
            , render <| graphics updated
            )
        AddPoint mouse ->
            let
                x =
                    mouse.clientPos.x - mouse.targetPos.x
                y =
                    mouse.clientPos.y - mouse.targetPos.y
                p =
                    Point x y
                updated =
                    addPoint model p
            in
            (updated, render <| graphics updated)
        NavPrev ->
            let
                nextPending =
                    case model.pendingGeom of
                        NoShape -> NoShape
                        PendingRect _ -> PendingRect []
                        PendingQuad _ -> PendingQuad []
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
            ( { model | pending = pending, processed = processed, pendingGeom = nextPending }
            , cmd
            )
        NavNext ->
            let
                nextPending =
                    case model.pendingGeom of
                        NoShape -> NoShape
                        PendingRect _ -> PendingRect []
                        PendingQuad _ -> PendingQuad []
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
            ( { model | pending = pending, processed = processed, pendingGeom = nextPending }
            , cmd
            )

addPoint : Model -> Point -> Model
addPoint m p =
    let
        pg =
            case m.pendingGeom of
                NoShape ->
                    NoShape
                PendingRect points ->
                    PendingRect (points ++ [p])
                PendingQuad points ->
                    PendingQuad (points ++ [p])
        nextPending =
            case m.pendingGeom of
                NoShape ->
                    NoShape
                PendingRect points ->
                    PendingRect []
                PendingQuad points ->
                    PendingQuad []
        g =
            completedShape pg
        current =
            List.head m.pending
                |> Maybe.withDefault (Image "" [])
        current_ =
            case g of
                Nothing -> current
                Just s -> { current | shapes = (Shape "" s) :: current.shapes }
        newPending =
            current_ :: (List.drop 1 m.pending)
        updated =
            case g of
                Nothing ->
                    { m | pendingGeom = pg }
                Just cg ->
                    { m | pendingGeom = nextPending, pending = newPending }
    in
    updated

completedShape : PendingGeometry -> Maybe Geometry
completedShape pg =
    case pg of
        NoShape -> Nothing
        PendingRect points ->
            if List.length points < 2 then
                Nothing
            else
                let
                    tl =
                        List.head points
                            |> Maybe.withDefault (Point -1 -1)
                    br =
                        List.drop 1 points
                            |> List.head
                            |> Maybe.withDefault (Point -1 -1)
                    o =
                        Offset (br.x - tl.x) (br.y - tl.y)
                in
                Just (Rect tl o)
        PendingQuad points ->
            if List.length points < 4 then
                Nothing
            else
                let
                    tl =
                        List.head points
                            |> Maybe.withDefault (Point -1 -1)
                    tr =
                        List.drop 1 points
                            |> List.head
                            |> Maybe.withDefault (Point -1 -1)
                    br =
                        List.drop 2 points
                            |> List.head
                            |> Maybe.withDefault (Point -1 -1)
                    bl =
                        List.drop 3 points
                            |> List.head
                            |> Maybe.withDefault (Point -1 -1)
                in
                Just (Quad tl tr br bl)

loadImageCmd : List Image -> Cmd Msg
loadImageCmd pending =
    List.head pending
        |> Maybe.map (\i -> loadImage i.url)
        |> Maybe.withDefault Cmd.none

uploadHandler : NativeFile -> Cmd Msg
uploadHandler file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt OnFileContent
