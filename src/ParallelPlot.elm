module ParallelPlot exposing (..)

import Axis
import Color
import Browser
import Html exposing (..)
import Html.Attributes exposing (href, value)
import Html.Events exposing (..)
import List.Extra
import Path
import Scale exposing (..)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, opacity, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Opacity(..), Paint(..), Transform(..))
import Http
import Csv.Decode exposing (..)
import Csv exposing (..)



type alias Student_Data =
    {   certification : String
    ,   gender : String
    ,   department : String
    ,   height : Float
    ,   weight : Float
    ,   tenthMark  : Float
    ,   twelthMark  : Float
    ,   collegeMark : Float
    ,   hobbies : String
    ,   dailyStudyingTime : String
    ,   preferStudyTime : String
    ,   salaryExpectation : Int
    ,   satisfyDegree : String --Bool
    ,   willignessDegree: String
    ,   socialMedia : String
    ,   travellingTime : String
    ,   stressLevel : String
    ,   financialStatus : String
    ,   partTimeJob : String --Bool
    }


type Msg
    = GotText (Result Http.Error String)
    | ChangeStudTime String
    | ChoosePos1 (Student_Data -> Float -> String)
    | ChoosePos2 (Student_Data -> Float -> String)
    | ChoosePos3 (Student_Data -> Float -> String)
    | ChoosePos4 (Student_Data -> Float -> String)


type Model 
    = Failure
    | Loading
    | Success 
        {data: List Student_Data
        , studTime: String
        , wert1 : Student_Data -> Float
        , wert2 : Student_Data -> Float
        , wert3 : Student_Data -> Float
        , wert4 : Student_Data -> Float
        , wertName1 : String
        , wertName2 : String
        , wertName3 : String
        , wertName4 : String
        }


type alias MultiDimPoint =
    { pointGender : String, pointHobbies: String, pointStress: String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , getCSV GotText
    )


csvString_to_data : String -> List Student_Data
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvStudentdata
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeCsvStudentdata : Csv.Decode.Decoder (Student_Data -> a ) a 
decodeCsvStudentdata =
    Csv.Decode.map Student_Data
        (Csv.Decode.field "Certification Course" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "Gender" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Department" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Height(CM)"(String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Weight(KG)" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "10th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "12th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "college mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "hobbies" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "daily studing time" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "prefer to study in" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "salary expectation" (String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Do you like your degree?" Ok) --(String.toBool >> Result.fromMaybe "error parsing string")) 
              |> Csv.Decode.andMap (Csv.Decode.field "willingness to pursue a career based on their degree  " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "social medai & video" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Travelling Time " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Stress Level " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Financial Status" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "part-time job" Ok) --(String.tool >> Result.fromMaybe "error parsing string")) 
        )


studentListe : List String -> List Student_Data
studentListe student_liste =
    List.map(\x -> csvString_to_data x) student_liste
        |> List.concat

getCSV : (Result Http.Error String -> Msg) -> Cmd Msg
getCSV msg =
    datenStudis 
    |> List.map 
      (\d ->
      Http.get
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest/main/" ++ d
    , expect = Http.expectString msg
    }
    )
    |> Cmd.batch


datenStudis: List String
datenStudis = ["Student_Behaviour.csv"]


-- vornehmen der Einstellungen


padding : Float
padding =
    50


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extension 
    , Tuple.second closeExtent + extension
    )


-- Daten filtern

filterStudents : List Student_Data -> String -> List Student_Data
filterStudents allData filterString =
    List.filter (\x -> filterString == x.dailyStudyingTime) allData



--parallelplot

scatterplot : Float -> Float -> MultiDimData -> Svg msg
scatterplot w ar model =
    let
        h : Float
        h =
            w / ar

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )

        transformListe : List (List Float)
        transformListe =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        wideExtentListe : List ( Float, Float )
        wideExtentListe =
            transformListe |> List.map wideExtent

        scaleListe =
            List.map (Scale.linear ( h, 0 )) wideExtentListe

        axisListe =
            List.map (Axis.left [ Axis.tickCount tickCount ]) scaleListe
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100
        ]
    <|
        [ TypedSvg.style []
            []
        , TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill <| Paint <| Color.white
            , stroke <| Paint <| Color.grey
            , strokeWidth <| Px 0.5
            ]
            []
        , g [ TypedSvg.Attributes.class [ "paralleleAchse" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\index axis ->
                        g
                            [ stroke <| Paint <| Color.black
                            , strokeWidth <| Px 0.1
                            , transform
                                [ Translate (Scale.convert xScale (toFloat index + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    axisListe
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\index beschreibung ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , fill <| Paint <| Color.red
                            , x <| Scale.convert xScale (toFloat index + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text beschreibung ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    punkt p gender hobbies stressLevel beschreibung=
                        let
                            graphenlinie : Path.Path
                            graphenlinie =
                                List.map3
                                    (\description s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat description
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    scaleListe
                                    p
                                    |> Shape.line Shape.linearCurve
                        in

                        g [class["parallelpoint"]][
                            Path.element graphenlinie
                            [ stroke <| Paint <| Color.black
                            , opacity (Opacity 1)
                            , strokeWidth <| Px 0.7
                            , fill PaintNone
                            , class ["parallelepoint"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (gender ++ ", " ++ hobbies ++ " (Stress level: " ++ stressLevel ++ ")" 
                               ++ (String.concat <|(List.map2(\a b -> ", " ++ a ++ ": " ++ (String.fromFloat b) ++ " ") beschreibung p)))]
                        ]
                in
                model.data
                    |> List.map
                        (\datensatz ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\x -> punkt x.value x.pointGender x.pointHobbies x.pointStress model.dimDescription) datensatz)
                        )
               )


-- Übernahme des Button aus dem Scatterplot

changeTimelul : Html Msg
changeTimelul =
    Html.select
        [onInput ChangeStudTime]
        [Html.option [value "0 - 30 minute"] [Html.text "0 - 30 minutes"]
        ,Html.option [value "30 - 60 minute"] [Html.text "30 - 60 minutes"]
        ,Html.option [value "1 - 2 Hour"] [Html.text "1 - 2 hours"]
        ,Html.option [value "2 - 3 hour"] [Html.text "2 - 3 hours"]
        ,Html.option [value "3 - 4 hour"] [Html.text "3 - 4 hours"]
        ,Html.option [value "More Than 4 hour"] [Html.text "More Than 4 hours"]
        ]




update : Msg -> Model -> Model
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| {data = studentListe [fullText], socialMedia = "0 minute", wert1 = .})





        TauschA ->
            { model | accessWerte2 = List.Extra.swapAt 0 1 model.accessWerte2, wert1 = model.wert2, wert2 = model.wert1 }

        TauschB ->
            { model | accessWerte2 = List.Extra.swapAt 1 2 model.accessWerte2, wert2 = model.wert3, wert3 = model.wert2 }

        TauschC ->
            { model | accessWerte2 = List.Extra.swapAt 2 3 model.accessWerte2, wert3 = model.wert4, wert4 = model.wert3 }