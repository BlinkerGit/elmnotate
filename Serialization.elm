module Serialization exposing (fromJson, toJson)

import Array
import Dict
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Model exposing (Document, Geometry(..), Image, LabelType(..), MetaData, Model, Offset, Point, Shape)


toJson : Model -> String
toJson m =
    serialized m
        |> toString


toString : Value -> String
toString v =
    E.encode 0 v


serialized : Model -> Value
serialized m =
    E.object
        [ ( "data", E.list serializedImage m.processed )
        , ( "meta"
          , E.object
                [ ( "dropdowns"
                  , E.object <|
                        List.map (\( k, v ) -> ( k, E.list E.string v )) <|
                            Dict.toList m.metaData.dropdowns
                  )
                , ( "labels", E.list E.string m.metaData.labels )
                , ( "quads", E.list E.string m.metaData.quads )
                , ( "rects", E.list E.string m.metaData.rects )
                ]
          )
        ]


serializedImage : Image -> Value
serializedImage i =
    E.object
        [ ( "url", E.string i.url )
        , ( "shapes", E.list serializedShape i.shapes )
        , ( "labels"
          , E.object <|
                List.map (\( k, v ) -> ( k, E.string v )) <|
                    Dict.toList i.labels
          )
        ]


serializedShape : Shape -> Value
serializedShape s =
    let
        shapeType =
            case s.geom of
                Rect _ _ ->
                    "rect"

                Quad _ _ _ _ ->
                    "quad"
    in
    E.object
        [ ( "label", E.string s.label )
        , ( shapeType, serializedGeom s.geom )
        ]


serializedGeom : Geometry -> Value
serializedGeom g =
    case g of
        Rect point offset ->
            E.list E.int [ point.x, point.y, offset.w, offset.h ]

        Quad p1 p2 p3 p4 ->
            E.list E.int [ p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y ]


fromJson : String -> Result D.Error Document
fromJson s =
    D.decodeString decodeDocument s


require : String -> Decoder a -> (a -> Decoder b) -> Decoder b
require fieldName decoder continuation =
    D.field fieldName decoder
        |> D.andThen continuation


allow : String -> Decoder a -> a -> (a -> Decoder b) -> Decoder b
allow fieldName decoder fallback continuation =
    D.oneOf
        [ D.field fieldName decoder
        , D.succeed fallback
        ]
        |> D.andThen continuation


decodeDocument : D.Decoder Document
decodeDocument =
    require "data" (D.list decodeImage) <|
        \data ->
            require "meta" decodeMeta <|
                \meta ->
                    D.succeed
                        { data = data
                        , meta = meta
                        }


decodeMeta : D.Decoder MetaData
decodeMeta =
    allow "dropdowns" (D.dict (D.list D.string)) Dict.empty <|
        \dropdowns ->
            allow "labels" (D.list D.string) [] <|
                \labels ->
                    allow "quads" (D.list D.string) [] <|
                        \quads ->
                            allow "rects" (D.list D.string) [] <|
                                \rects ->
                                    D.succeed
                                        { dropdowns = dropdowns
                                        , labels = labels
                                        , quads = quads
                                        , rects = rects
                                        }


decodeImage : D.Decoder Image
decodeImage =
    require "url" D.string <|
        \url ->
            allow "shapes" (D.list decodeShape) [] <|
                \shapes ->
                    allow "labels" (D.dict D.string) Dict.empty <|
                        \labels ->
                            D.succeed
                                { url = url
                                , shapes = shapes
                                , labels = labels
                                }


decodeShape : D.Decoder Shape
decodeShape =
    D.map3 Shape
        (D.field "label" D.string)
        (D.oneOf
            [ D.field "rect" (D.list D.int)
                |> D.andThen decodeRect
            , D.field "quad" (D.list D.int)
                |> D.andThen decodeQuad
            ]
        )
        (D.succeed False)


decodeRect : List Int -> D.Decoder Geometry
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
    D.succeed <| Rect (Point x y) (Offset w h)


decodeQuad : List Int -> D.Decoder Geometry
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
    D.succeed <|
        Quad
            (Point x1 y1)
            (Point x2 y2)
            (Point x3 y3)
            (Point x4 y4)
