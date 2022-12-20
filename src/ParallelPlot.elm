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
    ,   salaryExpectation : Float
    ,   satisfyDegree : String 
    ,   willignessDegree: String
    ,   socialMedia : String
    ,   travellingTime : String
    ,   stressLevel : String
    ,   financialStatus : String
    ,   partTimeJob : String 
    }


type Msg
    = GotText (Result Http.Error String)
    | ChangeStudTime String
    -- | ChangeInsta String
    | ChoosePos1 (Student_Data -> Float, String)
    | ChoosePos2 (Student_Data -> Float, String)
    | ChoosePos3 (Student_Data -> Float, String)
    | ChoosePos4 (Student_Data -> Float, String)


type Model 
    = Failure
    | Loading
    | Success 
        {data: List Student_Data
        , studTime: String
        -- , socTime: String
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


init : () -> ( Model, Cmd Msg )
init _ =
    (Loading , 
    Http.get
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest2/main/Daten/Student_Behaviour.csv"
    , expect = Http.expectString GotText
    })


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
              |> Csv.Decode.andMap (Csv.Decode.field "salary expectation" (String.toFloat >> Result.fromMaybe "error parsing string"))
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

paralleplot : Float -> Float -> MultiDimData -> Svg msg
paralleplot w ar model =
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
            [   TypedSvg.Core.text """"
                .parallelpoint { stroke: Color.red;}
                .parallelpoint:hover {stroke: rgb(0, 153, 150); stroke-width: 2;} 
                .parallelpoint text { display: none; }
                .parallelpoint:hover text { display: inline; stroke: rgb(0, 0, 0); stroke-width: 0.01; font-size: small; font-family: calibri}  
                """
            ]
        , TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill <| Paint <| Color.white
            , stroke <| Paint <| Color.darkRed
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
                            , fill <| Paint <| Color.red  --maybe male female?
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
                            , opacity (Opacity 10)
                            , strokeWidth <| Px 0.7
                            , fill PaintNone
                            , class ["parallelpoint"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (gender ++ ", Hobbie: " ++ hobbies ++ " (Stress level: " ++ stressLevel ++ ")" 
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


-- Übernahme des Button aus dem paralleplot

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



-- changeSocial : Html Msg
-- changeSocial =
--     Html.select
--         [onInput ChangeStudTime]
--         [Html.option [value "0 Minute"] [Html.text "0 minutes"]
--         ,Html.option [value "1 - 30 Minute"] [Html.text "30 - 60 minutes"]
--         ,Html.option [value "30 - 60 minute"] [Html.text "30 - 60 minutes"]
--         ,Html.option [value "1 - 1.30 hour"] [Html.text "1 - 1.30 hours"]
--         ,Html.option [value "1.30 - 2 hour"] [Html.text "1.30 - 2 hours"]
--         ,Html.option [value "More Than 2 hour"] [Html.text "More Than 2 hours"]
--         ]




update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| {data = studentListe [fullText], studTime = "0 - 30 minute", wert1 = .tenthMark, wert2 = .twelthMark, wert3 = .collegeMark, wert4 = .salaryExpectation,
                    wertName1 = "Tenth Mark", wertName2 = "Twelth Mark", wertName3 = "College Mark", wertName4 = "Salary Expectation"} , Cmd.none)

                Err _ ->
                    (model, Cmd.none)

        ChangeStudTime studTimeNew ->
            case model of
                Success first ->
                    ( Success <| {data = first.data , studTime = studTimeNew, wert1 = first.wert1, wert2 = first.wert2, wert3 = first.wert3, wert4 = first.wert4,
                    wertName1 = first.wertName1, wertName2 = first.wertName2, wertName3 = first.wertName3, wertName4 = first.wertName4} , Cmd.none)


                _ ->
                    (model, Cmd.none)


        -- ChangeInsta instaNew ->
        --     case model of
        --         Success first ->
        --             ( Success <| {data = first.data , studTime = studTimeNew, wert1 = first.wert1, wert2 = first.wert2, wert3 = first.wert3, wert4 = first.wert4,
        --             wertName1 = first.wertName1, wertName2 = first.wertName2, wertName3 = first.wertName3, wertName4 = first.wertName4} , Cmd.none)


        --         _ ->
        --             (model, Cmd.none)


        ChoosePos1 (wert1New, wertName1New) ->
            case model of
                Success sec ->
                    ( Success <| {data = sec.data , studTime = sec.studTime, wert1 = wert1New, wert2 = sec.wert2, wert3 = sec.wert3, wert4 = sec.wert4,
                    wertName1 = wertName1New, wertName2 = sec.wertName2, wertName3 = sec.wertName3, wertName4 = sec.wertName4} , Cmd.none)


                _ ->
                    (model, Cmd.none)

        ChoosePos2 (wert2New, wertName2New) ->
            case model of
                Success third ->
                    ( Success <| {data = third.data , studTime = third.studTime, wert1 = third.wert1, wert2 = wert2New, wert3 = third.wert3, wert4 = third.wert4,
                    wertName1 = third.wertName1, wertName2 = wertName2New, wertName3 = third.wertName3, wertName4 = third.wertName4} , Cmd.none)


                _ ->
                    (model, Cmd.none)

        ChoosePos3 (wert3New, wertName3New) ->
            case model of
                Success fourth ->
                    ( Success <| {data = fourth.data , studTime = fourth.studTime, wert1 = fourth.wert1, wert2 = fourth.wert2, wert3 = wert3New, wert4 = fourth.wert4,
                    wertName1 = fourth.wertName1, wertName2 = fourth.wertName2, wertName3 = wertName3New, wertName4 = fourth.wertName4} , Cmd.none)


                _ ->
                    (model, Cmd.none)

        ChoosePos4 (wert4New, wertName4New) ->
            case model of
                Success fifth ->
                    ( Success <| {data = fifth.data , studTime = fifth.studTime, wert1 = fifth.wert1, wert2 = fifth.wert2, wert3 = fifth.wert3, wert4 = wert4New,
                    wertName1 = fifth.wertName1, wertName2 = fifth.wertName2, wertName3 = fifth.wertName3, wertName4 = wertName4New} , Cmd.none)

                _ ->
                    (model, Cmd.none)
            

--einfügen der Viewfunktion, anfängliche Übernahme aus dem paralleplot

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your data."

        Loading ->
            text "Loading..."

        Success fullText ->
            let

                filData :  List Student_Data
                filData =
                    filterStudents fullText.data fullText.studTime 


                multiDimensionaleDaten : List Student_Data -> (Student_Data -> Float) -> (Student_Data -> Float) -> (Student_Data -> Float) -> (Student_Data -> Float) -> (Student_Data -> String) -> (Student_Data -> String) -> (Student_Data -> String) -> String -> String -> String -> String -> MultiDimData
                multiDimensionaleDaten student_liste a b c d e f g h i j k =
                    MultiDimData [h, i, j, k]
                        [ List.map
                            (\x ->
                                [ (a x), (b x), (c x), (d x)]
                                    |> MultiDimPoint (e x) (f x) (g x)
                            )
                            student_liste


                                ]


                multiDimData =
                    multiDimensionaleDaten filData fullText.wert1 fullText.wert2 fullText.wert3 fullText.wert4 .gender .hobbies .stressLevel fullText.wertName1 fullText.wertName2 fullText.wertName3 fullText.wertName4

                numberStudents: Int
                numberStudents =
                    List.length fullText.data

                numberStudentsFiltered: Int
                numberStudentsFiltered =
                    List.length filData


            in
            Html.div [Html.Attributes.style "padding" "20px"] 
                 [Html.p [Html.Attributes.style "fontSize" "16px"] 
                 [ Html.a[href "Main.elm"] [Html.text "Startseite"]
                 , Html.br [] []
                 , Html.a[href "paralleplotDaten.elm"] [Html.text "paralleplot"]
                 , Html.br [] []
                 , Html.a[href "Baumvisualisierung.elm"] [Html.text "Baumvisualisierung"]
                 , Html.br [] []
                 ]
                , Html.h1 [] [ Html.text ("Multidimensionale Daten für " ++ fullText.studTime)   ]
                , ul []
                    [ li [] [ Html.text <| "Ausgewählte tägliche Lernzeit: " ++ fullText.studTime ]
                    , changeTimelul
                    , li []
                        [ Html.text <|
                            "Studenten insgesamt: "
                                ++ String.fromInt numberStudents
                        ]
                    , li []
                        [ Html.text <|
                            "Gefilterte Studenten insgesamt: "
                            ++ String.fromInt numberStudentsFiltered
                        ]
                    -- , ul []
                    --     [ Html.button [ onClick TauschA ] [ Html.text <| "Tausche " ++ model.wert1 ++ " und " ++ model.wert2 ]
                    --     , Html.button [ onClick TauschB ] [ Html.text <| "Tausche " ++ model.wert2 ++ " und " ++ model.wert3 ]
                    --     , Html.button [ onClick TauschC ] [ Html.text <| "Tausche " ++ model.wert3 ++ " und " ++ model.wert4 ]
                    --     ]
                    , paralleplot 600 2 multiDimData
                    ]
                ]


                