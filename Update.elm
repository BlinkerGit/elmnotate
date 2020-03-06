module Update exposing (Msg(..), update)

import Canvas exposing (loadImage, render)
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import Model
    exposing
        ( FocusPoint(..)
        , Geometry(..)
        , Image
        , LabelClass
        , LabelType(..)
        , Model
        , Offset
        , PendingGeometry(..)
        , Point
        , Shape
        , graphics
        , initDocument
        , initImage
        , unscalePoint
        )
import Serialization exposing (fromJson)
import Set
import Task


type Msg
    = NoOp
    | FileDragEnter
    | FileDragLeave
    | FileDrop File (List File)
    | FileLoaded String String
    | RequestReset
    | CancelReset
    | ClientDims Offset Offset
    | WindowResized Offset
    | MouseDown ( Float, Float )
    | MouseMoved ( Float, Float )
    | MouseUp ( Float, Float )
    | DeleteShape Int
    | DeleteClass Int
    | ConvertRect Int
    | ToggleGeomMenu
    | SelectQuad
    | SelectRect
    | SelectLabel
    | SelectDropDown
    | SetLabelClassLabel String
    | SetImageLabel String String
    | SetImageDropDown String String
    | AddLabelClass
    | ActivateLabel Int
    | ActivateShape Int
    | EditSelectOptions String
    | CancelDialog
    | SaveEditingSelect
    | UpdateSelectOptions String
    | DownloadData
    | NavPrev
    | NavNext


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FileDragEnter ->
            ( { model | draggingFiles = True }, Cmd.none )

        FileDragLeave ->
            ( { model | draggingFiles = False }, Cmd.none )

        FileDrop file files ->
            ( { model
                | droppedFiles = file :: files
                , draggingFiles = False
              }
            , Task.perform (FileLoaded <| File.name file) (File.toString file)
            )

        FileLoaded filename content ->
            let
                newModel =
                    loadPendingImages filename
                        content
                        { model
                            | droppedFiles = []
                        }
            in
            ( newModel
            , loadImageCmd newModel.pending
            )

        RequestReset ->
            let
                updated =
                    if model.resetRequested then
                        let
                            i =
                                Model.init
                        in
                        { i | labelClasses = model.labelClasses }

                    else
                        { model | resetRequested = True }
            in
            ( updated, Cmd.none )

        CancelReset ->
            ( { model | resetRequested = False }, Cmd.none )

        ClientDims imgSize pnlSize ->
            let
                ratioW =
                    toFloat pnlSize.w / toFloat imgSize.w

                ratioH =
                    toFloat pnlSize.h / toFloat imgSize.h

                scale =
                    List.minimum [ ratioW, ratioH ] |> Maybe.withDefault 1.0

                updated =
                    { model | imageSize = imgSize, panelSize = pnlSize, scale = scale }
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
                    toFloat pnlSize.w / toFloat model.imageSize.w

                ratioH =
                    toFloat pnlSize.h / toFloat model.imageSize.h

                scale =
                    List.minimum [ ratioW, ratioH ] |> Maybe.withDefault 1.0

                updated =
                    { model | panelSize = pnlSize, scale = scale }
            in
            ( updated
            , render <| graphics updated
            )

        MouseMoved ( mx, my ) ->
            let
                canvasPoint =
                    Point (round mx) (round my)

                point =
                    unscalePoint model.scale canvasPoint

                img =
                    currentImage model

                indexedShapes =
                    img.shapes
                        |> List.map .geom
                        |> List.indexedMap (\a b -> ( a, b ))

                keepNear ( _, _, { x, y } ) =
                    (abs (x - point.x) < 5) && (abs (y - point.y) < 5)

                indexedPoints =
                    indexedShapes
                        |> List.concatMap
                            (\( gi, g ) ->
                                case g of
                                    Rect p o ->
                                        [ ( gi, 0, p )
                                        , ( gi, 1, Point (p.x + o.w) (p.y + o.h) )
                                        ]

                                    Quad tl tr br bl ->
                                        [ ( gi, 0, tl )
                                        , ( gi, 1, tr )
                                        , ( gi, 2, br )
                                        , ( gi, 3, bl )
                                        ]
                            )
                        |> List.filter keepNear

                hover_ =
                    List.head indexedPoints
                        |> Maybe.andThen (\( gi, pi, _ ) -> Just (FocusPoint gi pi))

                pending =
                    case model.pending of
                        [] ->
                            []

                        x :: xs ->
                            moveDraggingPoint model.dragPoint point x :: xs

                updated =
                    { model | hoverPoint = hover_, pending = pending }
            in
            ( updated
            , render <| graphics updated
            )

        MouseDown point ->
            let
                updated =
                    { model | dragPoint = model.hoverPoint }
            in
            ( updated
            , render <| graphics updated
            )

        MouseUp ( x, y ) ->
            let
                canvasPoint =
                    Point (round x) (round y)

                point =
                    unscalePoint model.scale canvasPoint

                updated =
                    if inCanvas canvasPoint model.panelSize then
                        case model.dragPoint of
                            Nothing ->
                                addPoint model point

                            Just _ ->
                                { model | dragPoint = Nothing }

                    else
                        model
            in
            ( updated
            , render <| graphics updated
            )

        DeleteShape index ->
            let
                img_ =
                    currentImage model

                h =
                    List.take index img_.shapes

                t =
                    List.drop (index + 1) img_.shapes

                s =
                    h ++ t

                img =
                    { img_ | shapes = s }

                pending =
                    img :: List.drop 1 model.pending

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
                    currentImage model

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
                    { img_ | shapes = h ++ (s :: t) }

                pending =
                    img :: List.drop 1 model.pending

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
                    { p_ | active = False, geom = PendingQuad [] }
            in
            ( { model | pendingClass = p }, Cmd.none )

        SelectRect ->
            let
                p_ =
                    model.pendingClass

                p =
                    { p_ | active = False, geom = PendingRect [] }
            in
            ( { model | pendingClass = p }, Cmd.none )

        SelectLabel ->
            let
                p_ =
                    model.pendingClass

                p =
                    { p_ | active = False, geom = PendingLabel }
            in
            ( { model | pendingClass = p }, Cmd.none )

        SelectDropDown ->
            let
                p_ =
                    model.pendingClass

                p =
                    { p_ | active = False, geom = PendingDropDown }
            in
            ( { model | pendingClass = p }, Cmd.none )

        SetLabelClassLabel l ->
            let
                p_ =
                    model.pendingClass

                p =
                    { p_ | label = l }
            in
            ( { model | pendingClass = p }, Cmd.none )

        SetImageLabel key value ->
            let
                img_ =
                    currentImage model

                img =
                    { img_ | labels = Dict.insert key value img_.labels }

                updated =
                    case model.pending of
                        [] ->
                            model

                        _ :: xs ->
                            { model | pending = img :: xs }
            in
            ( updated, Cmd.none )

        SetImageDropDown key value ->
            let
                img_ =
                    currentImage model

                img =
                    { img_ | labels = Dict.insert key value img_.labels }

                updated =
                    case model.pending of
                        [] ->
                            model

                        _ :: xs ->
                            { model | pending = img :: xs }
            in
            ( updated, Cmd.none )

        AddLabelClass ->
            let
                c =
                    model.labelClasses ++ [ model.pendingClass ]

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
                        [] ->
                            NoShape

                        x :: _ ->
                            x.geom
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
                    currentImage model

                shapes =
                    List.indexedMap setActive img.shapes

                p =
                    case model.pending of
                        [] ->
                            []

                        x :: xs ->
                            { x | shapes = shapes } :: xs
            in
            ( { model | pending = p }, Cmd.none )

        EditSelectOptions name ->
            let
                optionsString =
                    Dict.get name model.metaData.dropdown
                        |> Maybe.withDefault []
                        |> String.join "\n"

                updated =
                    { model
                        | editingSelectName = name
                        , editingSelectOptions = optionsString
                    }
            in
            ( updated, Cmd.none )

        CancelDialog ->
            let
                updated =
                    { model
                        | editingSelectName = ""
                        , editingSelectOptions = ""
                    }
            in
            ( updated, Cmd.none )

        SaveEditingSelect ->
            let
                optionList =
                    String.split "\n" model.editingSelectOptions
                        |> List.map String.trim
                        |> List.filter (not << String.isEmpty)

                m =
                    model.metaData

                d =
                    m.dropdown

                metaData =
                    { m
                        | dropdown = Dict.insert model.editingSelectName optionList d
                    }

                updated =
                    { model
                        | metaData = metaData
                        , editingSelectName = ""
                        , editingSelectOptions = ""
                    }
            in
            ( updated, Cmd.none )

        UpdateSelectOptions options ->
            ( { model | editingSelectOptions = options }, Cmd.none )

        DownloadData ->
            ( model
            , Download.string "data.json" "application/json" <| Serialization.toJson model
            )

        NavPrev ->
            navigateToPrevious model

        NavNext ->
            navigateToNext model


loadPendingImages : String -> String -> Model -> Model
loadPendingImages filename content model =
    case extension filename of
        ".txt" ->
            loadText content model

        ".json" ->
            loadJson content model

        _ ->
            model



-- TODO: error message


loadText : String -> Model -> Model
loadText content model =
    let
        images =
            content
                |> String.trim
                |> String.split "\n"
                |> Set.fromList
                |> Set.toList
                |> List.map (\url -> { initImage | url = url })
    in
    { model | pending = model.pending ++ images }


loadJson : String -> Model -> Model
loadJson content model =
    let
        document =
            case fromJson content of
                Ok p_ ->
                    p_

                Err _ ->
                    initDocument

        toLabelClass shape =
            let
                pg =
                    case shape.geom of
                        Rect _ _ ->
                            PendingRect []

                        Quad _ _ _ _ ->
                            PendingQuad []
            in
            ( shape.label, pg )

        labelClasses =
            List.concatMap .shapes document.data
                |> List.map toLabelClass
                -- unique labels only
                |> Dict.fromList
                |> Dict.toList
                |> List.map (\( l, g ) -> LabelClass l g False)

        labels =
            List.concatMap (.labels >> Dict.toList) document.data
                |> Dict.fromList
                |> Dict.toList
                |> List.map (\( k, _ ) -> LabelClass k (pendingTypeFromKey k document.meta.dropdown) False)
    in
    { model
        | pending = document.data
        , labelClasses = labelClasses ++ labels
        , metaData = document.meta
    }


splitExtension : String -> ( String, String )
splitExtension path =
    case String.reverse path |> String.split "." of
        [] ->
            ( "", "" )

        [ a ] ->
            ( String.reverse a, "" )

        x :: xs ->
            ( String.reverse <| String.join "." xs, "." ++ String.reverse x )


extension : String -> String
extension =
    splitExtension >> Tuple.second


pendingTypeFromKey : String -> Dict String (List String) -> PendingGeometry
pendingTypeFromKey key dropdowns =
    case Dict.get key dropdowns of
        Nothing ->
            PendingLabel

        Just _ ->
            PendingDropDown


navigateToPrevious : Model -> ( Model, Cmd Msg )
navigateToPrevious model =
    let
        nextPending =
            case model.pendingGeom of
                NoShape ->
                    NoShape

                PendingLabel ->
                    NoShape

                PendingDropDown ->
                    PendingDropDown

                PendingRect _ ->
                    PendingRect []

                PendingQuad _ ->
                    PendingQuad []

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


navigateToNext : Model -> ( Model, Cmd Msg )
navigateToNext model =
    let
        nextPending =
            case model.pendingGeom of
                NoShape ->
                    NoShape

                PendingLabel ->
                    NoShape

                PendingDropDown ->
                    PendingDropDown

                PendingRect _ ->
                    PendingRect []

                PendingQuad _ ->
                    PendingQuad []

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
        Nothing ->
            image

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

                                            1 ->
                                                Rect p (Offset (point.x - p.x) (point.y - p.y))

                                            _ ->
                                                shape.geom

                                    Quad tl tr br bl ->
                                        case pointIdx of
                                            0 ->
                                                Quad point tr br bl

                                            1 ->
                                                Quad tl point br bl

                                            2 ->
                                                Quad tl tr point bl

                                            3 ->
                                                Quad tl tr br point

                                            _ ->
                                                shape.geom
                        in
                        { shape | geom = g }

                    else
                        shape
            in
            { image | shapes = List.indexedMap updateShape image.shapes }


currentImage : Model -> Image
currentImage model =
    List.head model.pending
        |> Maybe.withDefault initImage


toQuad : Geometry -> Geometry
toQuad g =
    case g of
        Rect p o ->
            let
                tl =
                    p

                tr =
                    { tl | x = p.x + o.w }

                br =
                    { tr | y = p.y + o.h }

                bl =
                    { tl | y = p.y + o.h }
            in
            Quad tl tr br bl

        Quad _ _ _ _ ->
            g


addPoint : Model -> Point -> Model
addPoint m p =
    let
        pg =
            case m.pendingGeom of
                NoShape ->
                    NoShape

                PendingLabel ->
                    NoShape

                PendingDropDown ->
                    PendingDropDown

                PendingRect points ->
                    PendingRect (points ++ [ p ])

                PendingQuad points ->
                    PendingQuad (points ++ [ p ])

        nextPending =
            case m.pendingGeom of
                NoShape ->
                    NoShape

                PendingDropDown ->
                    PendingDropDown

                PendingLabel ->
                    NoShape

                PendingRect _ ->
                    PendingRect []

                PendingQuad _ ->
                    PendingQuad []

        label =
            case List.filter .active m.labelClasses of
                [] ->
                    ""

                x :: _ ->
                    x.label

        g =
            completedShape pg

        current =
            currentImage m

        current_ =
            case g of
                Nothing ->
                    current

                Just s ->
                    { current | shapes = Shape label s False :: current.shapes }

        newPending =
            current_ :: List.drop 1 m.pending

        updated =
            case g of
                Nothing ->
                    { m | pendingGeom = pg }

                Just _ ->
                    { m | pendingGeom = nextPending, pending = newPending }
    in
    updated


completedShape : PendingGeometry -> Maybe Geometry
completedShape pg =
    case pg of
        NoShape ->
            Nothing

        PendingLabel ->
            Nothing

        PendingDropDown ->
            Nothing

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
