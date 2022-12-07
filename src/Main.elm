module Main exposing (..)

--importieren der Module

import Browser
import Csv
import Csv.Decode
import Html exposing (div, text, input, button, pre, Html)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt, toInt)
import Debug exposing (log)
import Http
import TypedSvg.Attributes exposing (y)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Model
  = Failure
  | Loading
  | Success 
        {data: List Student_Data
     , position: String
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
type Msg
  = GotText (Result Http.Error String)
 {-- | BiggestWin --}

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
        --  , div [] [ScatterplotDaten conf.data] ]
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

            

        






