module Baumvisualisierung exposing (..)

import Browser
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events
import Http
import Json.Decode
import Tree exposing (Tree)
import TreeLayout
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types


type alias Model =
    { wide : Float
    , height : Float
    , radius : Float
    , distance : Float
    , tree : Tree String
    , error : String
    }


type Msg
    = ChangeWide String
    | ChangeHeight String
    | ChangeRadius String
    | ChangeDistance String
    | GotFlare (Result Http.Error (Tree String))
    | Error String

treeDecoder : Json.Decode.Decoder (Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    Tree.tree name []

                Just c ->
                    Tree.tree name c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )


initialModel : () -> ( Model, Cmd Msg )
initialModel () =
    ( { wide = 3000, height = 2000, radius = 100, distance = 20, tree = Tree.singleton "", error = "shit...." }
    , Http.get { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest2/main/Daten/BaumStudent.json?token=GHSAT0AAAAAAB3NMVWF2HO33GGJYOYRRXVYY4ZZEJA", expect = Http.expectJson GotFlare treeDecoder }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeWide wide ->
            case String.toFloat wide of
                Just w ->
                    ( { model | wide = w, error = "" }
                    , Cmd.none
                    )

                Nothing ->
                    update (Error "Wide must be a float!") model

        ChangeHeight height ->
            case String.toFloat height of
                Just h ->
                    ( { model | height = h, error = "" }
                    , Cmd.none
                    )

                Nothing ->
                    update (Error "Height must be a float!") model

        ChangeRadius radius ->
            case String.toFloat radius of
                Just r ->
                    ( { model | radius = r, error = "" }
                    , Cmd.none
                    )

                Nothing ->
                    update (Error "Radius must be a float!") model

        ChangeDistance distance ->
            case String.toFloat distance of
                Just d ->
                    ( { model | distance = d, error = "" }
                    , Cmd.none
                    )

                Nothing ->
                    update (Error "Distance must be a float!") model

        GotFlare load ->
            case load of
                Ok newTree ->
                    ( { model | tree = newTree }, Cmd.none )

                Err error ->
                    ( { model
                        | tree = Tree.singleton ""
                        , error =
                            case error of
                                Http.BadBody newErrorMsg ->
                                    newErrorMsg

                                _ ->
                                    "Tree-Loading-Error"
                      }
                    , Cmd.none
                    )

        Error e ->
            ( { model | error = e }
            , Cmd.none
            )


drawCircle : String -> Float -> Float -> Float -> Svg Msg
drawCircle name x y r =
    TypedSvg.g []
        [ TypedSvg.circle
            [ TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Color.rgb255 128 128 128))
            , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint (Color.rgb255 0 0 0))
            , TypedSvg.Attributes.InPx.cx x
            , TypedSvg.Attributes.InPx.cy y
            , TypedSvg.Attributes.InPx.r r
            ]
            []
        , TypedSvg.text_
            [ TypedSvg.Attributes.InPx.x x
            , TypedSvg.Attributes.InPx.y (y + 5.5)
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            , TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Color.rgb255 0 0 0))
            ]
            [ TypedSvg.Core.text name
            ]
        ]


drawTreeLines : List ( String, Maybe String ) -> Dict String { x : Float, y : Float } -> List (Svg Msg)
drawTreeLines data dict =
    let
        helper : String -> Bool -> Float
        helper name isX =
            case Dict.get name dict of
                Just p ->
                    if isX then
                        p.x

                    else
                        p.y

                Nothing ->
                    0
    in
    List.map
        (\( node, mParent ) ->
            case mParent of
                Just parent ->
                    TypedSvg.line
                        [ TypedSvg.Attributes.InPx.x1 (helper node True)
                        , TypedSvg.Attributes.InPx.y1 (helper node False)
                        , TypedSvg.Attributes.InPx.x2 (helper parent True)
                        , TypedSvg.Attributes.InPx.y2 (helper parent False)
                        , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint (Color.rgb255 0 0 0))
                        ]
                        []

                Nothing ->
                    TypedSvg.g [] []
        )
        data


drawTree : List ( String, Maybe String ) -> Dict String { x : Float, y : Float } -> Float -> Svg Msg
drawTree data dict radius =
    let
        list =
            Dict.toList dict
    in
    TypedSvg.g
        []
        (drawTreeLines data dict
            ++ List.map
                (\( name, { x, y } ) -> drawCircle name x y radius)
                list
        )


deleteDoublesInList : List ( String, Maybe String ) -> List ( String, Maybe String )
deleteDoublesInList list =
    let
        deleteDoublesInListHelper : List ( String, Maybe String ) -> List ( String, Maybe String ) -> List ( String, Maybe String )
        deleteDoublesInListHelper l res =
            case List.head l of
                Just ( headName, headTail ) ->
                    deleteDoublesInListHelper (List.filter (\( x, _ ) -> x /= headName) l) (res ++ [ ( headName, headTail ) ])

                Nothing ->
                    res
    in
    deleteDoublesInListHelper list []


transformTreeData : Tree String -> List ( String, Maybe String )
transformTreeData tree =
    let
        transformTreeDataHelper : Tree String -> List ( String, Maybe String ) -> List ( String, Maybe String )
        transformTreeDataHelper t l =
            case Tree.children t of
                head :: tail ->
                    List.concat (List.map (\el -> transformTreeDataHelper el (l ++ [ ( Tree.label el, Just (Tree.label t) ) ])) ([ head ] ++ tail))

                [] ->
                    l
    in
    deleteDoublesInList ([ ( Tree.label tree, Nothing ) ] ++ transformTreeDataHelper tree [])


treeView : Model -> List (Svg Msg)
treeView model =
    let
        w =
            model.wide

        h =
            model.height

        padding =
            50

        treeData =
            transformTreeData model.tree

        treeTemp =
            TreeLayout.treeLayout
                (2 * model.radius + model.distance)
                treeData

        tree =
            Dict.fromList (List.map (\( name, { x, y } ) -> ( name, { x = x + w / 2, y = (y - 1) * (2 * model.radius + model.distance) } )) (Dict.toList treeTemp))
    in
    [ TypedSvg.svg [ TypedSvg.Attributes.viewBox 0 -100 (w + padding) (h + padding), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.g [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate padding padding ] ]
            [ drawTree treeData tree model.radius ]
        ]
    ]




view : Model -> Html Msg
view model =
    Html.div []
        ([ Html.text model.error
         , Html.text "Breite: "
         , Html.input [ Html.Events.onInput ChangeWide ] []
         , Html.text "Höhe: "
         , Html.input [ Html.Events.onInput ChangeHeight ] []
         , Html.text "Radius: "
         , Html.input [ Html.Events.onInput ChangeRadius ] []
         , Html.text "Abstand: "
         , Html.input [ Html.Events.onInput ChangeDistance ] []
         ]
            ++ treeView model
        )


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }