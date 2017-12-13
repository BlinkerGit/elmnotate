module Serialization exposing (..)

import Json.Encode exposing (Value, list, object, int, string, encode)
import Model exposing (Model, Image, Shape, Geometry(..))

toJson : Model -> String
toJson m =
    serialized m
        |> toString
    
toString : Value -> String
toString v =
    encode 0 v

serialized : Model -> Value
serialized m =
    list <| List.map serializedImage m.processed

serializedImage : Image -> Value
serializedImage i =
    object
        [ ("url",    string i.url)
        , ("shapes", list <| List.map serializedShape i.shapes)
        ]

serializedShape : Shape -> Value
serializedShape s =
    let
        shapeType =
            case s.geom of
                Rect _ _     -> "rect"
                Quad _ _ _ _ -> "quad"
    in
    object
        [ ("label",   string s.label)
        , (shapeType, serializedGeom s.geom)
        ]

serializedGeom : Geometry -> Value
serializedGeom g =
    case g of
        Rect point offset ->
            list <| List.map int [point.x, point.y, offset.w, offset.h]
        Quad p1 p2 p3 p4 ->
            list <| List.map int [p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y]
