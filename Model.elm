module Model exposing (..)

import DropZone

type alias Point =
    { x: Int
    , y: Int
    }

type alias Line =
    { start: Point
    , end: Point
    }

type alias Graphics =
    { points: List Point
    , lines: List Line
    , anchors: List Point
    }

type alias Offset =
    { w: Int
    , h: Int
    }

type Geometry
    = Rect Point Offset
    | Quad Point Point Point Point

type PendingGeometry
    = NoShape
    | PendingRect (List Point)
    | PendingQuad (List Point)

-- index of geom in shape, point in geom for hover/drag
type FocusPoint = FocusPoint Int Int

type alias Shape =
    { label: String
    , geom: Geometry
    }

type alias LabelClass =
    { label: String
    , geom: PendingGeometry
    , selectOpen: Bool
    }

type alias Image =
    { url: String
    , shapes: List Shape
    }

type alias Model =
    { pending: List Image
    , processed: List Image
    , pendingGeom: PendingGeometry
    , labelClasses: List LabelClass
    , pendingClass: LabelClass
    , dragPoint: Maybe FocusPoint
    , hoverPoint: Maybe FocusPoint
    , imageSize: Offset
    , panelSize: Offset
    , scale: Float
    , dropZone: DropZone.Model
    }

init : Model
init =
    Model
        []
        []
        NoShape
        []
        (LabelClass "" NoShape False)
        Nothing
        Nothing
        (Offset 2000 2000)
        (Offset 2000 2000)
        1.0
        DropZone.init

graphics : Model -> Graphics
graphics m =
    let
        current =
            List.head m.pending
                |> Maybe.withDefault (Image "" [])
        lines =
            List.map .geom current.shapes
                |> List.map toLines
                |> List.concat
        anchors =
            List.map .geom current.shapes
                |> List.map toAnchors
                |> List.concat
        points =
            case m.pendingGeom of
                NoShape -> []
                PendingRect l -> l
                PendingQuad l -> l
    in
    Graphics (scalePoints m.scale points) (scaleLines m.scale lines) (scalePoints m.scale anchors)

unscalePoint : Float -> Point -> Point
unscalePoint s p =
    let
        unscaled i =
            (round ((toFloat i) / s))
    in
    Point (unscaled p.x) (unscaled p.y)

scalePoint : Float -> Point -> Point
scalePoint s p =
    let
        scaled i =
            (round ((toFloat i) * s))
    in
    Point (scaled p.x) (scaled p.y)

scalePoints : Float -> List Point -> List Point
scalePoints s points =
    List.map (scalePoint s) points

scaleLines : Float -> List Line -> List Line
scaleLines s lines =
    let
        scaleLine {start, end} =
            Line (scalePoint s start) (scalePoint s end)
    in
    List.map scaleLine lines

toLines : Geometry -> List Line
toLines g =
    case g of
        Rect p o ->
            rectLines p o
        Quad p1 p2 p3 p4 ->
            quadLines p1 p2 p3 p4

toAnchors : Geometry -> List Point
toAnchors g =
    case g of
        Rect p o ->
            [ p
            , Point (p.x + o.w) (p.y + o.h)
            ]
        Quad p1 p2 p3 p4 ->
            [p1, p2, p3, p4]

rectLines : Point -> Offset -> List Line
rectLines p o =
    let
        tl = p
        tr = Point (p.x + o.w) p.y
        br = Point (p.x + o.w) (p.y + o.h)
        bl = Point p.x (p.y + o.h)
    in
    quadLines tl tr br bl

quadLines : Point -> Point -> Point -> Point -> List Line
quadLines tl tr br bl =
        [ Line tl tr
        , Line tr br
        , Line br bl
        , Line bl tl
        ]
