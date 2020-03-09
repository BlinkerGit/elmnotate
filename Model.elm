module Model exposing (..)

import Dict exposing (Dict)
import File exposing (File)


type alias Point =
    { x : Int
    , y : Int
    }


type alias Line =
    { start : Point
    , end : Point
    }


type alias Graphics =
    { points : List Point
    , lines : List Line
    , anchors : List Point
    , highlight : List Line
    }


type alias Offset =
    { w : Int
    , h : Int
    }


type Geometry
    = Rect Point Offset
    | Quad Point Point Point Point


type PendingGeometry
    = PendingLabel
    | PendingDropDown
    | PendingRect (List Point)
    | PendingQuad (List Point)



-- index of geom in shape, point in geom for hover/drag


type FocusPoint
    = FocusPoint Int Int


type alias Shape =
    { label : String
    , geom : Geometry
    , active : Bool
    }


type alias LabelClass =
    { name : String
    , geom : PendingGeometry
    , active : Bool
    }


initLabelClass : LabelClass
initLabelClass =
    { name = ""
    , geom = PendingLabel
    , active = False
    }


type LabelType
    = Label
    | DropDown


type alias MetaData =
    { dropdowns : Dict String (List String)
    , labels : List String
    , quads : List String
    , rects : List String
    }


initMetaData : MetaData
initMetaData =
    { dropdowns = Dict.empty
    , labels = []
    , quads = []
    , rects = []
    }


type alias Document =
    { data : List Image
    , meta : MetaData
    }


initDocument : Document
initDocument =
    { data = []
    , meta = initMetaData
    }


type alias Image =
    { url : String
    , shapes : List Shape
    , labels : Dict String String
    }


initImage : Image
initImage =
    { url = ""
    , shapes = []
    , labels = Dict.empty
    }


type alias DropDownData =
    Dict String (List String)


type alias Model =
    { pending : List Image
    , processed : List Image
    , resetRequested : Bool
    , pendingGeom : Maybe PendingGeometry
    , labelClasses : List LabelClass
    , pendingClass : LabelClass
    , dragPoint : Maybe FocusPoint
    , hoverPoint : Maybe FocusPoint
    , draggingFiles : Bool
    , droppedFiles : List File
    , imageSize : Offset
    , panelSize : Offset
    , scale : Float
    , metaData : MetaData
    , editingSelectName : String
    , editingSelectOptions : String
    }


init : Model
init =
    { pending = []
    , processed = []
    , resetRequested = False
    , pendingGeom = Nothing
    , labelClasses = []
    , pendingClass = initLabelClass
    , dragPoint = Nothing
    , hoverPoint = Nothing
    , draggingFiles = False
    , droppedFiles = []
    , imageSize = Offset 2000 2000
    , panelSize = Offset 2000 2000
    , scale = 1.0
    , metaData = initMetaData
    , editingSelectName = ""
    , editingSelectOptions = ""
    }


graphics : Model -> Graphics
graphics m =
    let
        current =
            List.head m.pending
                |> Maybe.withDefault initImage

        lines =
            List.map .geom current.shapes
                |> List.concatMap toLines

        anchors =
            List.map .geom current.shapes
                |> List.concatMap toAnchors

        points =
            case m.pendingGeom of
                Just (PendingRect l) ->
                    l

                Just (PendingQuad l) ->
                    l

                _ ->
                    []

        highlight =
            List.filter .active current.shapes
                |> List.concatMap (.geom >> toLines)
    in
    Graphics
        (scalePoints m.scale points)
        (scaleLines m.scale lines)
        (scalePoints m.scale anchors)
        (scaleLines m.scale highlight)


unscalePoint : Float -> Point -> Point
unscalePoint s p =
    let
        unscaled i =
            round (toFloat i / s)
    in
    Point (unscaled p.x) (unscaled p.y)


scalePoint : Float -> Point -> Point
scalePoint s p =
    let
        scaled i =
            round (toFloat i * s)
    in
    Point (scaled p.x) (scaled p.y)


scalePoints : Float -> List Point -> List Point
scalePoints s points =
    List.map (scalePoint s) points


scaleLines : Float -> List Line -> List Line
scaleLines s lines =
    let
        scaleLine { start, end } =
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
            [ p1, p2, p3, p4 ]


rectLines : Point -> Offset -> List Line
rectLines p o =
    let
        tl =
            p

        tr =
            Point (p.x + o.w) p.y

        br =
            Point (p.x + o.w) (p.y + o.h)

        bl =
            Point p.x (p.y + o.h)
    in
    quadLines tl tr br bl


quadLines : Point -> Point -> Point -> Point -> List Line
quadLines tl tr br bl =
    [ Line tl tr
    , Line tr br
    , Line br bl
    , Line bl tl
    ]
