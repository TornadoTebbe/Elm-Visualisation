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


type Msg
    = GotText (Result Http.Error String)
    -- | ChooseAttribute1 String
    -- | ChooseAttribute2 String
    -- | ShowText (Svg Msg)
    -- | DisableText
    -- | Error String

type alias Point =
    { pointName : String, x : Float, y : Float }

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
     , x: tenthMark
     , y: twelthMark
        }



-- type StudentMark 
--     = TenthMark
--     | TwelthMark
--     | CollegeMark


-- get Attributes

getAttributes : String -> Student_Data -> Float
getAttributes str std =
    case str of
        "tenth Mark" ->
                .tenthMark std

        "twelth Mark" ->
                .twelthMark std

        _ -> 2.7


studentFloat : List Float
studentFloat =
    List.map ( .tenthMark)


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


textSpawnEvent : String -> Float -> Float -> Svg Msg
textSpawnEvent name xPos yPos =
    text_
        [ x xPos
        , y yPos
        , textAnchor AnchorMiddle
        ]
        [ TypedSvg.Core.text name ]



scatterplot : Model -> XyData -> Svg Msg
scatterplot model xyData =
    let
        xValues : List Float
        xValues =
            List.map .x xyData.data

        yValues : List Float
        yValues =
            List.map .y xyData.data

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
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(0,0,0); }
          """ ]
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class [ "axis" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ yAxis yValues
            , text_ [ textAnchor AnchorMiddle, y (Scale.convert yScaleLocal labelPositions.y - 15), x 0 ] [ TypedSvg.Core.text xyData.yDescription ]
            ]
        , g
            [ transform [ Translate (padding - 1) (padding - 1 + Tuple.first (Scale.range yScaleLocal)) ]
            , class [ "axis" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ xAxis xValues
            , text_ [ textAnchor AnchorMiddle, y 30, x (Scale.convert xScaleLocal labelPositions.x - 10) ] [ TypedSvg.Core.text xyData.xDescription ]
            ]
        , circlePlot xyData xScaleLocal yScaleLocal
        ]
        

pointCircle : ContinuousScale Float -> ContinuousScale Float -> List (TypedSvg.Core.Attribute Msg) -> Point -> Svg Msg
pointCircle scaleX scaleY circleAttributes xyPoint =
    g [ class [ "point" ] ]
        [ circle
            ([ cx (Scale.convert scaleX xyPoint.x)
             , cy (Scale.convert scaleY xyPoint.y)
             , TypedSvg.Attributes.r (TypedSvg.Types.px 5)
             
             ]
                ++ circleAttributes
            )
            []
        ]



circlePlot : XyData -> ContinuousScale Float -> ContinuousScale Float -> Svg Msg
circlePlot data xScaleLocal yScaleLocal =
    g []
        [ g
            [ transform [ Translate padding padding ] ]
            (List.map
                (pointCircle xScaleLocal
                    yScaleLocal
                    [ TypedSvg.Attributes.fill (TypedSvg.Types.Paint (Color.rgb 255 255 255))
                    , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint (Color.rgb255 0 0 0))
                    ]
                )
                data.data
            )
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
                   (Success <| { data = csvString_to_data fullText, description = fullText }, Cmd.none)

                Err _ ->
                  {--( { model | status = Failure }, Cmd.none )--}  
                  (Failure, Cmd.none)

        {--BiggestWin ->
          ( Success <| { m | description= "Whatever stuff"}, Cmd.none ) --}

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
            fullText