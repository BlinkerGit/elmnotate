module Model exposing (..)

type alias Point =
    { x: Int
    , y: Int
    }

type alias Offset =
    { w: Int
    , h: Int
    }

type Geometry
    = Rect Point Offset
    | Quad Point Point Point Point

type alias Shape =
    { label: String
    , geom: Geometry
    }

type alias Image =
    { url: String
    , shapes: List Shape
    }

type alias Model =
    { pending: List Image
    , processed: List Image
    }
