module Scatterplot exposing (..)

import Axis
import Html exposing (..)
import Html.Attributes exposing (value, href)
import Html.Events exposing (onInput)
import Scale exposing (ContinuousScale)
import Shape exposing (..)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Csv.Decode exposing (..)
import Csv exposing (Csv)
import TypedSvg.Attributes exposing (points)
import Main exposing (Student_Data)


-- type StudentAttributes 
--     = salaryExpectation
--     | tenthMark
--     | twelthMark
--     | collegeMark




type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
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

scatterplot : XyData -> Svg msg
scatterplot model =
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


stringtoMaybeStudent : String -> Maybe StudentAttribute
stringtoMaybeStudent str = 
    case str of
        "salary expectation" ->



-- studentToPoint : Student_Data -> Model -> Maybe Point
-- studentToPoint student =
--     let
        
--     in
    








-- filterReduceStudents : List Student_Data -> XyData
-- filterReduceStudents newStudents = 
--     let 
--         filterStudents
--             List.filterMap studentToPoint newStudents
--     in
--     XyData "Salary Expectation" "Tenth Mark" filterStudents


-- filterData : List Student_Data -> String -> List Student_Data
-- filterData dataDone position =
--     List.filter (\c -> c.pos == position) dataDone






