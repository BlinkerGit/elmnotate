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
    | ClientDims Offset Offset
    | WindowResized Offset
    | NewShape PendingGeometry
    | DeleteShape Int
    | AddPoint MouseEvents.MouseEvent
    | ConvertRect Int
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
        ClientDims imgSize pnlSize ->
            let
                log =
                    Debug.log "ClientDims" [imgSize, pnlSize]
                ratioW =
                    (toFloat pnlSize.w) / (toFloat imgSize.w)
                ratioH =
                    (toFloat pnlSize.h) / (toFloat imgSize.h)
                scale =
                    List.minimum [ratioW, ratioH] |> Maybe.withDefault 1.0
                log2 =
                    Debug.log "scale" scale
                updated = { model | imageSize = imgSize , panelSize = pnlSize , scale = scale }
            in
            ( updated
            , render <| graphics model
            )
        WindowResized offset ->
            let
                log =
                    Debug.log "Window resized" offset
                pnlSize =
                    Offset (offset.w - 300) (offset.h - 100)
                ratioW =
                    (toFloat pnlSize.w) / (toFloat model.imageSize.w)
                ratioH =
                    (toFloat pnlSize.h) / (toFloat model.imageSize.h)
                scale =
                    List.minimum [ratioW, ratioH] |> Maybe.withDefault 1.0
                log2 =
                    Debug.log "scale" scale
                updated = { model | panelSize = pnlSize , scale = scale }
            in
            ( updated
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
                    toFloat (mouse.clientPos.x - mouse.targetPos.x)
                        / model.scale
                        |> round
                y =
                    toFloat (mouse.clientPos.y - mouse.targetPos.y)
                        / model.scale
                        |> round
                p =
                    Point x y
                updated =
                    addPoint model p
            in
            (updated, render <| graphics updated)
        ConvertRect index ->
            let
                img_ =
                    List.head model.pending
                        |> Maybe.withDefault (Image "" [])
                h =
                    List.take index img_.shapes
                s_ =
                    List.drop index img_.shapes
                        |> List.head
                        |> Maybe.withDefault (Shape "" (Rect (Point 0 0) (Offset 0 0)))
                s =
                    { s_ | geom = toQuad s_.geom }
                t =
                    List.drop (index + 1) img_.shapes
                img =
                    { img_ | shapes = h ++ [s] ++ t }
                pending =
                    img :: (List.drop 1 model.pending)
                updated =
                    { model | pending = pending }
            in
            ( updated
            , Cmd.none
            )
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

toQuad : Geometry -> Geometry
toQuad g =
    case g of
        Rect p o ->
            let
                tl = p
                tr = { tl | x = p.x + o.w }
                br = { tr | y = p.y + o.h }
                bl = { tl | y = p.y + o.h }
            in
            Quad tl tr br bl
        Quad _ _ _ _ -> g

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
