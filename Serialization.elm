module Serialization exposing (..)

import Array
import Dict
import Json.Encode exposing (Value, list, object, int, string, encode)
import Json.Decode as Dec
import Model exposing (Model, Image, Shape, Point, Offset, Geometry(..))

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
        , ("labels", object <| List.map (\(k,v) -> (k, string v))
                            <| Dict.toList i.labels)
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


fromJson : String -> Result String (List Image)
fromJson s =
    Dec.decodeString (Dec.list decodeImage) s

decodeImage : Dec.Decoder Image
decodeImage =
    Dec.map3 Image
        (Dec.field "url"    Dec.string)
        (Dec.field "shapes" (Dec.list decodeShape))
        (Dec.oneOf
            [ (Dec.field "labels" (Dec.dict Dec.string))
            , Dec.succeed Dict.empty
            ]
            )


decodeShape : Dec.Decoder Shape
decodeShape =
    Dec.map3 Shape
        (Dec.field "label"  Dec.string)
        (Dec.oneOf
            [ (Dec.field "rect" (Dec.list Dec.int))
                |> Dec.andThen decodeRect
            , (Dec.field "quad" (Dec.list Dec.int))
                |> Dec.andThen decodeQuad
            ])
        (Dec.succeed False)

decodeRect : List Int -> Dec.Decoder Geometry
decodeRect points =
    let
        a =
            Array.fromList points
        x =
           Array.get 0 a |> Maybe.withDefault -1
        y =
           Array.get 1 a |> Maybe.withDefault -1
        w =
           Array.get 2 a |> Maybe.withDefault -1
        h =
            Array.get 3 a |> Maybe.withDefault -1
    in
    Dec.succeed <| Rect (Point x y) (Offset w h)

decodeQuad : List Int -> Dec.Decoder Geometry
decodeQuad points =
    let
        a =
            Array.fromList points
        x1 =
           Array.get 0 a |> Maybe.withDefault -1
        y1 =
           Array.get 1 a |> Maybe.withDefault -1
        x2 =
           Array.get 2 a |> Maybe.withDefault -1
        y2 =
           Array.get 3 a |> Maybe.withDefault -1
        x3 =
           Array.get 4 a |> Maybe.withDefault -1
        y3 =
           Array.get 5 a |> Maybe.withDefault -1
        x4 =
           Array.get 6 a |> Maybe.withDefault -1
        y4 =
           Array.get 7 a |> Maybe.withDefault -1
    in
    Dec.succeed <| Quad
        (Point x1 y1)
        (Point x2 y2)
        (Point x3 y3)
        (Point x4 y4)
