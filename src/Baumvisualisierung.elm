module Baumvisualisierung exposing (..)

import Dict exposing (Dict)
import Browser
import Statistics
import Color
import Html exposing (Html, text, li, ul)
import Html.Events exposing (..)
import Html.Attributes exposing (href)
import Http
import TreeLayout exposing (treeLayout)
import Json.Decode
import Scale
import Tree exposing (Tree)
import TypedSvg exposing (circle, g, line, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, stroke, textAnchor, transform, viewBox, strokeWidth)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


type alias Model =
    { wide : Float
    , height : Float
    , radius : Float
    , distance : Float
    , tree : Tree String
    , error : String
    }

type alias Coordinate =
    { x : Float
    , y : Float
    }



type Msg
    = ChangeWide String
    | ChangeHeight String
    | ChangeRadius String
    | ChangeDistance String
    | GotFlare (Result Http.Error (Tree String))
    | Error String

padding : Float
padding =
    60

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


xScale : Float -> List Float -> Scale.ContinuousScale Float
xScale wide values =
    Scale.linear ( 0, wide - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


yScale : Float -> List Float -> Scale.ContinuousScale Float
yScale height values =
    Scale.linear ( 0, height - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


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


init : () -> ( Model, Cmd Msg )
init () =
    ( { wide = 4000, height = 2500, radius = 50, distance = 1, tree = Tree.singleton "", error = "" }
    , Http.get { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest2/main/Daten/baumfertig.json", expect = Http.expectJson GotFlare treeDecoder }
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



knoten : Float -> Scale.ContinuousScale Float -> Scale.ContinuousScale Float -> Float -> Float -> String-> Svg msg
knoten  rad scaleX scaleY xValue yValue name =
            g [ class [ "point" ], fontSize <| Px 30.0, fontFamily [ "sans-serif" ] ]       
                [ circle
                    [ cx (Scale.convert scaleX xValue)
                    , cy (Scale.convert scaleY yValue)
                    , r rad              
                    ]
                    [] 
                    , text_
                    [ x (Scale.convert scaleX xValue +5) 
                    , y (Scale.convert scaleY yValue)
                    , textAnchor AnchorMiddle
                    ]
                    [ text name ]
                ]


getKoordinaten: String ->  Dict String Coordinate -> Maybe Coordinate 
getKoordinaten name dictKnoten =
        Dict.get name dictKnoten


kante : Scale.ContinuousScale Float -> Scale.ContinuousScale Float -> Dict String Coordinate -> (String, Maybe String) -> Svg msg
kante xScale1 yScale1 koordinate tuple =
    let
      son: String
      son = Tuple.first tuple
      
      father: String
      father = Tuple.second tuple |> Maybe.withDefault son

      
      koordinatenVater: Coordinate
      koordinatenVater = getKoordinaten father koordinate |> Maybe.withDefault  {x=0.0, y=0.0}
      
      koordinatenSohn: Coordinate
      koordinatenSohn = getKoordinaten son koordinate |> Maybe.withDefault {x=0.0, y=0.0}
      
      
    in
      line
        [ x1 (Scale.convert xScale1 koordinatenVater.x)
        , y1 (Scale.convert yScale1 koordinatenVater.y)
        , x2 (Scale.convert xScale1 koordinatenSohn.x)
        , y2 (Scale.convert yScale1 koordinatenSohn.y)
        , stroke <| Paint <| Color.rgba 0 0 0 1
        ]
        
        []




convertStud : Tree ( String, Maybe String ) -> Tree ( String, Maybe String )
convertStud tree =
    let
        ( currentLabel, _ ) =
            Tree.label tree
    in
    Tree.mapChildren
        (\listChildren ->
            listChildren
                |> List.map
                    (\a ->
                        convertStud a
                            |> Tree.mapLabel (\( b, _ ) -> ( b, Just currentLabel ))
                    )
        )
        tree


treePlot : Float -> Float -> Float -> Float -> List ( String, Maybe String ) -> Svg msg
treePlot wide height rad distance tree =
    let
        layout : Dict String Coordinate
        layout = 
            treeLayout distance tree 
        
        xValues : List Float
        xValues =
          List.map .x (Dict.values layout) 
            

        yValues : List Float
        yValues =
            List.map .y (Dict.values layout) 
            

        xScaleLocal : Scale.ContinuousScale Float
        xScaleLocal =
            xScale wide xValues

        yScaleLocal : Scale.ContinuousScale Float
        yScaleLocal =
            yScale height yValues
        
        
        nameValues: List String
        nameValues = Dict.keys layout

    in
    svg [ viewBox -50 0 (wide + 150) height, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100,150,150,1); }
            .point line { stroke: rgba(170, 170, 170, 0.8); rgba(170, 170, 170, 0.8); }
            .point text { display: inline; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(118, 214, 78,1); }
            .point:hover text { display: inline; }
          """ ]


        , g
            [ transform [ Translate padding padding ] ]
                (
                    (  List.map (kante xScaleLocal yScaleLocal layout) tree )
                    ++
                    (List.map3 (knoten rad xScaleLocal yScaleLocal) xValues yValues nameValues )
                
                    )
        ]


                






view : Model -> Html Msg
view model =
    let
        wide =
            model.wide

        height =
            model.height

        rad =
            model.radius

        distance =
            model.distance

        converteTree : List ( String, Maybe String )
        converteTree =
            model.tree
                |> Tree.map (\x -> ( x, Nothing ))
                |> convertStud
                |> Tree.flatten

    in
            Html.div [Html.Attributes.style "padding" "20px"] 
                 [Html.p [Html.Attributes.style "fontSize" "16px"]
                 [ Html.h1 [] [ Html.text ("Navigation zu den anderen Visualisierungsmöglichkeiten:")   ]
                 
                 , Html.a[href "Main.elm"] [Html.text "Startseite"]
                 , Html.br [] []
                 , Html.a[href "ScatterplotDaten.elm"] [Html.text "Scatterplot"]
                 , Html.br [] []
                 , Html.a[href "ParallelPlot.elm"] [Html.text "Parallelplot"]
                 , Html.br [] []
                 ]
                , Html.h2 [] [ Html.text ("Baumdiagramm Studenten")   ]
                , Html.p []
                [ Html.text model.error
                , Html.text "Breite: "
                , Html.input [ Html.Events.onInput ChangeWide ] []
                , Html.text "Höhe: "
                , Html.input [ Html.Events.onInput ChangeHeight ] []
                , Html.text "Radius: "
                , Html.input [ Html.Events.onInput ChangeRadius ] []

                ,  Html.h3 []
                    [ Html.text ( "Darstellung der Lernzeiten:") ]          
                , ul []
                    [ li [] [ Html.text ("Durchschnittliche College Note: ACM")]
                    , Html.br [] []
                    , li [] [Html.text "Beste ACM Male: 77"]
                    , Html.br [] []
                    , li [] [Html.text "Beste ACM Female: 81"]
                    , Html.br [] []
                    , li [] [Html.text "Schlechteste ACM Male: 64"]
                    , Html.br [] []
                    , li [] [Html.text "Schlechteste ACM Female: 41"]

                        , treePlot wide height rad distance converteTree

                ]]

                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }