module ScatteplotDaten exposing (..)

import Axis
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import Csv
import Csv.Decode
import Http


type Msg
    = GotText (Result Http.Error String)
    -- | ChooseAttribute1 String
    -- | ChooseAttribute2 String
    -- | ShowText (Svg Msg)
    -- | DisableText
    -- | Error String

type alias Point =
    { pointName : String, salaryExpectation : Int, tenthMark : Float, twelthMark : Float, collegeMark : Float}


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
     , position: String
    --  , x: StudentAttributes
    --  , y: StudentAttributes}
        }



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


type alias Configuration =
    { data: List Student_Data
    , description: String
    }


-- type StudentAttributes 
--     = Points
--     | salaryExpectation
--     | tenthMark
--     | twelthMark
--     | collegeMark
 



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
        range =
            Maybe.withDefault ( 0, 0 ) (Statistics.extent values)

        minimum =
            Tuple.first range

        maximum =
            Tuple.second range

        extended =
            (maximum - minimum) / toFloat (tickCount * 3)

        upperBound =
            maximum + extended

        lowerBound =
            if minimum - extended < 0 then
                0

            else
                minimum - extended
    in
    ( lowerBound, upperBound )



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




--scatterplot

scatterplot : Model -> XyData -> Svg Msg
scatterplot model xyData =
    let
        --Testpunkt
        kreisbeschriftung : String
        kreisbeschriftung =
            Maybe.withDefault
                "Kein Punkt gefunden"
                (Maybe.map (\o -> o.pointName) (List.head model.data))

        --x-Werte/cityMPG
        xValues : List Float
        xValues =
            List.map .x model.data

        --y-Werte/retailPrice
        yValues : List Float
        yValues =
            List.map .y model.data

        --Abbildungen/Umrechnungen auf SVG
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
            .point:hover text { display: inline; fill: rgb(18, 132, 90)}
            """ ]

            
        --x-Achse
        , g
            [ transform [ Translate padding (h - padding) ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x + 25)
                , y 35
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 17.0
                , fontFamily [ "sans-serif" ]
                ]
                [ Html.text model.xDescription ]
            ]

        -- y-Achse
        , g
            [ transform [ Translate padding padding ] ]
            [ yAxis yValues
            , text_
                [ x 0
                , y (Scale.convert yScaleLocal labelPositions.y - 15)
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 17.0
                , fontFamily [ "sans-serif" ]
                ]
                [ Html.text model.yDescription ]
            ]

        --SVG der Points
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
        

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]

        --Positionierung der Punkte
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x)
                (Scale.convert scaleY xyPoint.y)
            ]
        ]
        
        [ circle [ cx 0, cy 0, r 4 ] []
        , text_
            [ x 0, y -15, TypedSvg.Attributes.textAnchor AnchorMiddle ]
            [ TypedSvg.Core.text xyPoint.pointName ]
        ]



studentToPoint : 


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




















































-- Laden der Daten


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


daten : List String
daten = [ "Student_Behaviour"]



--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
    (Loading , 
    Http.get
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest/main/Daten/Student_Behaviour.csv"
    , expect = Http.expectString GotText
    })
    
-- UPDATE

--Decodierung der Daten:

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
    



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                   (Success <| { data = csvString_to_data fullText, description = fullText }, Cmd.none)

                Err _ ->
                  {--( { model | status = Failure }, Cmd.none )--}  
                  (Failure, Cmd.none)

        {--BiggestWin ->
          ( Success <| { m | description= "Whatever stuff"}, Cmd.none ) --}

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




displaytext : Configuration -> Html Msg
displaytext conf =
    let
        var = String.fromInt (List.length conf.data)
    in
         pre [] [ text (var)
         , text (conf.description)]
        --  , Html.button [ onClick BiggestWin ] [ text "Click me" ] ]
    
        

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your data."

        Loading ->
            text "Loading..."

        Success fullText ->
            displaytext fullText