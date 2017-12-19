module Update exposing (..)

import DropZone exposing (DropZoneMessage(..))
import FileReader exposing (NativeFile)
import MimeType
import Task
import Model exposing (Model, Image, Point, Offset, LabelClass, Shape, Geometry(..), PendingGeometry(..), graphics, unscalePoint, FocusPoint(..))
import Canvas exposing (render, loadImage)
import Serialization exposing (fromJson)

type Msg
    = NoOp
    | DnD (DropZoneMessage (List NativeFile))
    | OnTextContent (Result FileReader.Error String)
    | OnJsonContent (Result FileReader.Error String)
    | ClientDims Offset Offset
    | WindowResized Offset
    | MouseMoved Point
    | MouseDown Point
    | MouseUp Point
    | DeleteShape Int
    | DeleteClass Int
    | ConvertRect Int
    | ToggleGeomMenu
    | SelectQuad
    | SelectRect
    | SetLabel String
    | AddLabelClass
    | ActivateLabel Int
    | ActivateShape Int
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
        OnTextContent (Ok content) ->
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
            , (loadImageCmd pending)
            )
        OnTextContent (Err error) ->
            (model, Cmd.none)
        OnJsonContent (Ok content) ->
            let
                pending =
                    case fromJson content of
                        Ok p_ -> p_
                        Err _ -> []
            in
            ( { model | pending = pending }
            , (loadImageCmd pending)
            )
        OnJsonContent (Err error) ->
            (model, Cmd.none)
        ClientDims imgSize pnlSize ->
            let
                ratioW =
                    (toFloat pnlSize.w) / (toFloat imgSize.w)
                ratioH =
                    (toFloat pnlSize.h) / (toFloat imgSize.h)
                scale =
                    List.minimum [ratioW, ratioH] |> Maybe.withDefault 1.0
                updated = { model | imageSize = imgSize , panelSize = pnlSize , scale = scale }
            in
            ( updated
            , render <| graphics updated
            )
        WindowResized offset ->
            let
                pnlSize =
                    -- subtract sidebar width, header height
                    Offset (offset.w - 300) (offset.h - 100)
                ratioW =
                    (toFloat pnlSize.w) / (toFloat model.imageSize.w)
                ratioH =
                    (toFloat pnlSize.h) / (toFloat model.imageSize.h)
                scale =
                    List.minimum [ratioW, ratioH] |> Maybe.withDefault 1.0
                updated = { model | panelSize = pnlSize , scale = scale }
            in
            ( updated
            , render <| graphics updated
            )
        MouseMoved pt ->
            let
                canvasPoint =
                    -- subtract header height
                    Point pt.x (pt.y - 50)
                point =
                    unscalePoint model.scale canvasPoint
                img =
                    List.head model.pending
                        |> Maybe.withDefault (Image "" [])
                indexedShapes =
                    img.shapes
                        |> List.map (.geom)
                        |> List.indexedMap (,)
                keepNear (_, _, {x, y}) =
                    (abs (x - point.x) < 5) && (abs (y - point.y) < 5)
                indexedPoints =
                    indexedShapes
                        |> List.concatMap (\(gi,g) ->
                            case g of
                                Rect p o ->
                                    [ (gi, 0, p)
                                    , (gi, 1, Point (p.x + o.w) (p.y + o.h))
                                    ]
                                Quad tl tr br bl ->
                                    [ (gi, 0, tl)
                                    , (gi, 1, tr)
                                    , (gi, 2, br)
                                    , (gi, 3, bl)
                                    ]
                        )
                        |> List.filter keepNear
                hover_ =
                    List.head indexedPoints
                        |> Maybe.andThen (\(gi,pi,_) -> Just (FocusPoint gi pi))
                pending =
                    case model.pending of
                        [] -> []
                        x :: xs ->
                            moveDraggingPoint model.dragPoint point x :: xs
            in
            ( { model | hoverPoint = hover_, pending = pending }
            , render <| graphics model
            )
        MouseDown point ->
            ( { model | dragPoint = model.hoverPoint }
            , render <| graphics model
            )
        MouseUp pt ->
            let
                canvasPoint =
                    -- subtract header height
                    Point pt.x (pt.y - 50)
                point =
                    unscalePoint model.scale canvasPoint
                updated =
                    if inCanvas canvasPoint model.panelSize then
                        case model.dragPoint of
                            Nothing -> addPoint model point
                            Just p -> { model | dragPoint = Nothing }
                    else
                        model

            in
            ( updated
            , render <| graphics updated
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
        DeleteClass index ->
            let
                h =
                    List.take index model.labelClasses
                t =
                    List.drop (index + 1) model.labelClasses
                lc =
                    h ++ t
                updated =
                    { model | labelClasses = lc }
            in
            ( updated
            , Cmd.none
            )
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
                        |> Maybe.withDefault (Shape "" (Rect (Point 0 0) (Offset 0 0)) False)
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
        ToggleGeomMenu ->
            let
                p_ =
                    model.pendingClass
                p =
                    { p_ | active = not p_.active }
            in
            ( { model | pendingClass = p }, Cmd.none )
        SelectQuad ->
            let
                p_ =
                    model.pendingClass
                p =
                    { p_ | active = False, geom = (PendingQuad []) }
            in
            ( { model | pendingClass = p }, Cmd.none )
        SelectRect ->
            let
                p_ =
                    model.pendingClass
                p =
                    { p_ | active = False, geom = (PendingRect []) }
            in
            ( { model | pendingClass = p }, Cmd.none )
        SetLabel l ->
            let
                p_ =
                    model.pendingClass
                p =
                    { p_ | label = l }
            in
            ( { model | pendingClass = p }, Cmd.none )
        AddLabelClass ->
            let
                c =
                    model.labelClasses ++ [model.pendingClass]
                p =
                    LabelClass "" NoShape False
            in
            ( { model | labelClasses = c, pendingClass = p }, Cmd.none )
        ActivateLabel i ->
            let
                setActive ai lc =
                    { lc | active = ai == i }
                c =
                    List.indexedMap setActive model.labelClasses
                pg =
                    case List.filter .active c of
                        [] -> NoShape
                        x :: xs -> x.geom
            in
            ( { model | labelClasses = c, pendingGeom = pg }, Cmd.none )
        ActivateShape i ->
            let
                setActive ai s =
                    let
                        a =
                            if s.active then
                                False
                            else
                                ai == i
                    in
                    { s | active = a }
                img =
                    List.head model.pending
                        |> Maybe.withDefault (Image "" [])
                shapes =
                    List.indexedMap setActive img.shapes
                p =
                    case model.pending of
                        [] -> []
                        x :: xs -> { x | shapes = shapes } :: xs
            in
            ( { model | pending = p }, Cmd.none )
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

inCanvas : Point -> Offset -> Bool
inCanvas p o =
    p.x >= 0 && p.y >= 0 && p.x <= o.w && p.y <= o.h

moveDraggingPoint : Maybe FocusPoint -> Point -> Image -> Image
moveDraggingPoint drag point image =
    case drag of
        Nothing -> image
        Just (FocusPoint shapeIdx pointIdx) ->
            let
                updateShape i shape =
                    if i == shapeIdx then
                        let
                            g =
                                case shape.geom of
                                    Rect p o ->
                                        case pointIdx of
                                            0 ->
                                                let
                                                    delta =
                                                        Offset (point.x - p.x) (point.y - p.y)
                                                    offset =
                                                        Offset (o.w - delta.w) (o.h - delta.h)
                                                in
                                                Rect point offset
                                            1 -> Rect p (Offset (point.x - p.x) (point.y - p.y))
                                            _ -> shape.geom
                                    Quad tl tr br bl ->
                                        case pointIdx of
                                            0 -> Quad point tr br bl
                                            1 -> Quad tl point br bl
                                            2 -> Quad tl tr point bl
                                            3 -> Quad tl tr br point
                                            _ -> shape.geom
                        in
                        { shape | geom = g }
                    else
                        shape
            in
            { image | shapes = List.indexedMap updateShape image.shapes }

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
        label =
            case List.filter .active m.labelClasses of
                [] -> ""
                x :: xs -> x.label
        g =
            completedShape pg
        current =
            List.head m.pending
                |> Maybe.withDefault (Image "" [])
        current_ =
            case g of
                Nothing -> current
                Just s -> { current | shapes = (Shape label s False) :: current.shapes }
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
    let
        cmd =
            case file.mimeType of
                Nothing -> OnTextContent
                Just t ->
                    case MimeType.toString t of
                        "application/json" -> OnJsonContent
                        _ -> OnTextContent
    in
    FileReader.readAsTextFile file.blob
        |> Task.attempt cmd
