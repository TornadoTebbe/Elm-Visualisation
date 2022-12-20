module ScatterplotDaten exposing (..)

import Axis
import Browser
import Html exposing (..)
import Html.Attributes exposing (value, href)
import Html.Events exposing (onInput)
import Shape exposing (..)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import Csv
import Csv.Decode
import Http
import List exposing (filterMap)
import Html.Events exposing (onInput)
import TypedSvg.Attributes exposing (points)





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
    ,   satisfyDegree : String --Bool
    ,   willignessDegree: String
    ,   socialMedia : String
    ,   travellingTime : String
    ,   stressLevel : String
    ,   financialStatus : String
    ,   partTimeJob : String --Bool
    }

type StudentAttribute
    = TenthMark
    | TwelthMark
    | CollegeMark
    | SalaryExpectation

type Msg
    = GotText (Result Http.Error String)
    | ChangeStudTime String
    | ChooseStudent1 StudentAttribute
    | ChooseStudent2 StudentAttribute
    



type alias Point =
    { pointName : String, tenthMark : Float, twelthMark : Float, collegeMark : Float, salaryExpectation : Float}

type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

type Model
  = Failure
  | Loading
  | Success 
        {data: List Student_Data
     , dailyStudyingTime: String
    -- , salaryExpectation: Int
     , x: StudentAttribute
     , y: StudentAttribute
        }





-- work with Attributes

stringToStudent : String -> StudentAttribute
stringToStudent str =

        if str == "10th Mark" then
            TenthMark

        else if str == "12th Mark" then
            TwelthMark

        else if str == "Salary expectation" then
            SalaryExpectation

        else
            CollegeMark


studentToReverseString : StudentAttribute -> String
studentToReverseString stringo =
    case stringo of
        TenthMark ->
            "10th Mark"

        TwelthMark ->
            "12th Mark"

        CollegeMark ->
            "College Mark"

        SalaryExpectation ->
            "Salary expectation"



studentToMaybePoint : Student_Data -> Maybe Point
studentToMaybePoint student =
    map4 
        (\tenthMark twelthMark collegeMark salaryExpectation->
            Point
                (student.gender ++ " (Prefered Study Time: " ++ student.preferStudyTime ++ ")" )
                (tenthMark)
                (twelthMark)
                (salaryExpectation)
                (collegeMark) 
        )
        (Just student.tenthMark)
        (Just student.twelthMark)
        (Just student.salaryExpectation)
        (Just student.collegeMark)


-- filter and Reduce

filterStudents : List Student_Data -> String -> List Student_Data
filterStudents allData filterString =
    List.filter (\x -> filterString == x.dailyStudyingTime) allData


-- filterSalary : List Student_Data -> Int -> List Student_Data
-- filterSalary allData filterInt =
--     List.filter (\y -> y.salaryExpectation < 200000) allData

filterAndReduceStudents : List Student_Data -> XyData
filterAndReduceStudents studentreduce =
    let 
        filterStudentitos =
            List.filterMap studentToMaybePoint studentreduce

    in 
        XyData "10th Mark" "12th Mark" filterStudentitos


--map 

andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
  Maybe.map2 (|>)
  
map4 : (a -> b -> c -> d -> e)  
  -> Maybe a
  -> Maybe b
  -> Maybe c
  -> Maybe d
  -> Maybe e
 
map4 function maybe1 maybe2 maybe3 maybe4 =
  Just function
    |> andMap maybe1
    |> andMap maybe2
    |> andMap maybe3
    |> andMap maybe4



-- Scatterplot


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


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
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )




xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)



scatterplot : XyData -> List Float -> List Float -> String -> String -> Svg Msg
scatterplot model xValues yValues xBeschr yBeschr =
    let

        punkte =
            List.map2 (\x y -> (x, y)) xValues yValues

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
        svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) ( padding - 1 ) ]
            , class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            []

        , g 
            [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xValues
             , text_
                [ x 350
                 , y 30                
                ]
                [ text xBeschr ]
            ]
        , g 
            [transform [ Translate (padding - 1) padding ] ]
            [ yAxis yValues                             
            , text_
                [ x -40
                , y ( Scale.convert yScaleLocal labelPositions.y - 15)  
                ]
                [ text yBeschr ]         
            ]
        , g 
             [transform [ Translate padding padding ] ]
                (List.map2 (pointCircle xScaleLocal yScaleLocal) model.data punkte)
              
        ]


        
        

pointCircle : ContinuousScale Float -> ContinuousScale Float  -> Point -> (Float, Float) -> Svg Msg
pointCircle scaleX scaleY point xyPoint =
    g [ class [ "point" ] ]
        [ circle
            [ cx (Scale.convert scaleX (Tuple.first xyPoint))
            , cy (Scale.convert scaleY (Tuple.second xyPoint))
            , r radius
             ]
            []
            , text_
                    [ x (Scale.convert scaleX (Tuple.first xyPoint))
                    , y (Scale.convert scaleY (Tuple.second xyPoint) - (radius + 3))
                    , textAnchor AnchorMiddle
                    ]
                    [Html.text point.pointName ]
        ]


changeTimelul : Html Msg
changeTimelul =
    Html.select
        [onInput ChangeStudTime]
        [Html.option [value "0 - 30 minute"] [Html.text "0 - 30 Minuten"]
        ,Html.option [value "30 - 60 minute"] [Html.text "30 - 60 Minuten"]
        ,Html.option [value "1 - 2 Hour"] [Html.text "1 - 2 Stunden"]
        ,Html.option [value "2 - 3 hour"] [Html.text "2 - 3 Stunden"]
        ,Html.option [value "3 - 4 hour"] [Html.text "3 - 4 Stunden"]
        ,Html.option [value "More Than 4 hour"] [Html.text "Mehr als 4 Stunden"]
        ]
        


buttonX : Html Msg
buttonX =
    Html.select
        [ onInput (\x -> stringToStudent x |> ChooseStudent1) ]
         [Html.option [value "10th Mark"] [Html.text "10th Grade Mark"]
        ,Html.option [value "12th Mark"] [Html.text "12th Grade Mark"]
        ,Html.option [value "college mark"] [Html.text "College Mark"]  
        ,Html.option [value "Salary expectation"] [Html.text "Salary Expectation"]
        ]

buttonY : Html Msg
buttonY =
    Html.select
        [ onInput (\y -> stringToStudent y |> ChooseStudent2) ]
        [Html.option [value "10th Mark"] [Html.text "10th Grade Mark"]
        ,Html.option [value "12th Mark"] [Html.text "12th Grade Mark"]
        ,Html.option [value "college mark"] [Html.text "College Mark"]  
        ,Html.option [value "Salary expectation"] [Html.text "Salary Expectation"]
        ]






-- Laden der Daten


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
    (Loading , 
    Http.get
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest2/main/Daten/Student_Behaviour.csv"
    , expect = Http.expectString GotText
    })
    
-- UPDATE

--Decodierung der Daten:




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                   (Success <| { data = studentListe [fullText], dailyStudyingTime = "0 - 30 minute", x = TenthMark, y = TwelthMark }, Cmd.none)

                Err _ ->
                  (model, Cmd.none)

        ChangeStudTime newTime ->
            case model of
                Success a -> 
                    (Success <| { data = a.data, dailyStudyingTime = newTime, x = a.x, y = a.y }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        ChooseStudent1 xNew ->
            case model of
                Success b -> 
                    (Success <| { data = b.data, dailyStudyingTime = b.dailyStudyingTime, x = xNew, y = b.y }, Cmd.none)

                _ ->
                    (model, Cmd.none)


        ChooseStudent2 yNew ->
            case model of
                Success c -> 
                    (Success <| { data = c.data, dailyStudyingTime = c.dailyStudyingTime, x = c.x, y = yNew }, Cmd.none)

                _ ->
                    (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your data."

        Loading ->
            text "Loading..."

        Success fullText ->
            let
                filterAttribute : List Student_Data -> StudentAttribute -> List Float
                filterAttribute points attribut = 
                    case attribut of
                        TenthMark ->
                            List.map .tenthMark points

                        TwelthMark ->
                            List.map .twelthMark points

                        SalaryExpectation ->
                            List.map .salaryExpectation points

                        CollegeMark ->
                            List.map .collegeMark points

                xVal : List Float
                xVal =
                    filterAttribute filData fullText.x

                yVal : List Float
                yVal =
                    filterAttribute filData fullText.y

                -- filData2: List Student_Data
                -- filData2 =
                --     filterSalary fullText.data
                -- integrieren der Filterfunktion für unter 50k?

                filData :  List Student_Data
                filData =
                    filterStudents fullText.data fullText.dailyStudyingTime 

                numStud =  
                     List.length fullText.data

                
                
                numFilStud =
                    List.length filData

                filRedNumStud : XyData
                filRedNumStud =
                    filterAndReduceStudents (filterStudents fullText.data fullText.dailyStudyingTime)

            in
            Html.div [Html.Attributes.style "padding" "20px"] 
                 [Html.p [Html.Attributes.style "fontSize" "16px"] 
                 [ Html.a[href "Main.elm"] [Html.text "Startseite"]
                 , Html.br [] []
                 , Html.a[href "ParallelPlot.elm"] [Html.text "Parallele Koordinaten"]
                 , Html.br [] []
                 , Html.a[href "Baumvisualisierung.elm"] [Html.text "Baumvisualisierung"]
                 , Html.br [] []
                 ]

                , Html.h2 []
                    [Html.text ("Scatterplot")]

                , Html.p []
                     [ Html.text ("Auswahl der täglichen Lernzeit: ")
                     , Html.br [][]
                     , changeTimelul                    
                     ]

                    , Html.h3 []
                    [Html.text ("Scatterplot Lernzeit " ++ fullText.dailyStudyingTime ++ ":")]
                , Html.p []
                [ Html.text ("Anzahl Studenten gesamt: " ++ (String.fromInt (numStud)))] 

                 , Html.p []
                [ Html.text ("Anzahl Studenten mit ausgewählter Lernzeit: " ++ (String.fromInt(numFilStud)))] 
                

                , Html.h4 []
                    [ Html.text ("Auswahl X-Achse: ")
                    , buttonX
                    , Html.text ("  Auswahl Y-Achse: ")
                    , buttonY ]
                , scatterplot filRedNumStud xVal yVal (studentToReverseString fullText.x) (studentToReverseString fullText.y)
                ] 




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


            

