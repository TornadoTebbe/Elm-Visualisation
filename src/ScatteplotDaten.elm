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
import Main exposing (..)
import List exposing (filterMap)


type StudentAttribute
    = TenthMark
    | TwelthMark
    | CollegeMark
    -- | SalaryExpectation

type Msg
    = GotText (Result Http.Error String)
    | ChangePos String
    | ChooseStudent1 String
    | ChooseStudent2 String
    



type alias Point =
    { pointName : String, tenthMark : Float, twelthMark : Float, collegeMark : Float}

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
     , x: StudentAttribute
     , y: StudentAttribute
        }





-- work with Attributes

stringToStudent : String -> StudentAttribute
stringToStudent str =

        if str == "tenth Mark" then
             TenthMark

        else if str == "twelth Mark" then
             TwelthMark

        else
             CollegeMark


studentToReverseString : StudentAttribute -> String
studentToReverseString stringo =
    case stringo of
        TenthMark ->
            "tenth Mark"

        TwelthMark ->
            "twelth Mark"

        CollegeMark ->
            "college Mark"



studentToMaybePoint : Student_Data -> Maybe Point
studentToMaybePoint student =
    map4 
        (\tenthMark twelthMark collegeMark ->
         Point
            (student.gender ++ "(" ++ student.preferStudyTime "," ++ "," student.dailyStudyingTime ++ ")")
            (tenthMark)
            (twelthMark)
            (collegeMark) 
        )
        (Just student.tenthMark)
        (Just student.twelthMark)
        (Just student.collegeMark)


-- filter and Reduce

filterStudents : List Student_Data -> String -> List Student_Data
filterStudents allData preferStudyTime =
    List.filter (\x -> x.preferStudyTime == preferStudyTime) allData

filterAndReduceStudents : List Student_Data -> XyData
filterAndReduceStudents studentreduce =
    let 
        filterStudentitos =
            List.filterMap studentToMaybePoint studentreduce

    in 
        XyData "Tenth Grade Mark" "Twelth Grade Mark" filterStudentitos


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


textSpawnEvent : String -> Float -> Float -> Svg Msg
textSpawnEvent name xPos yPos =
    text_
        [ x xPos
        , y yPos
        , textAnchor AnchorMiddle
        ]
        [ TypedSvg.Core.text name ]



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
                [ x (Scale.convert xScaleLocal labelPositions.x)
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
                    [Html.text point.pointName]
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
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest/main/Daten/Student_Behaviour.csv"
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
                   (Success <| { data = studentListe [fullText], position = "Night", x = TenthMark, y = TwelthMark }, Cmd.none)

                Err _ ->
                  (model, Cmd.none)

        ChangePos newPos ->
            case model of
                Success a -> 
                    (Success <| { data = a.data, position = newPos, x = a.x, y = a.y }, Cmd.none)

                _ ->
                    (model, Cmd.none)

        ChooseStudent1 xNew ->
            case model of
                Success b -> 
                    (Success <| { data = b.data, position = b.position, x = b.x, y = b.y }, Cmd.none)

                _ ->
                    (model, Cmd.none)


        ChooseStudent2 yNew ->
            case model of
                Success c -> 
                    (Success <| { data = c.data, position = c.position, x = c.x, y = c.y }, Cmd.none)

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

                        CollegeMark ->
                            List.map .collegeMark points

                xVal : List Float
                xVal =
                    attribut filData fullText.x

            in
            

