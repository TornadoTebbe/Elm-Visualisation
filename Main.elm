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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Success (List String)
    | Loading
    | Failure


daten : List String
daten = [ "Student_Behaviour"]



--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
  ( Loading 
  , daten 
      |> List.map
          (\d ->
              Http.get
              { url = "../Daten/" ++ d ++ ".csv"
              , expect = Http.expectString GotText
              }
          )
      |> Cmd.batch
  )


-- UPDATE
type Msg
  = GotText (Result Http.Error String)



--Decodierung der Daten:

csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvStudentdata
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeCsvStudentdata : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a 
decodeCsvStudentdata =
    Csv.Decode.map (\a b -> ( a, Just b ))
        (Csv.Decode.field "Certification Course" Ok
              |> Csv.Decode.andMap 
                    (Csv.Decode.field "Gender" 
                        (String.toFloat >> Result.fromMaybe "error parsing string")
                    )
              |> Csv.Decode.andMap
                    (Csv.Decode.field "Department"
                        (String.toFloat >> Result.fromMaybe "error parsing string")
                    )
        )

              {--
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
--}

transform : List ( String, Maybe Float ) -> List ( String, String )
transform textKomplett =
    List.map (\( a, b ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "Nothing" )) textKomplett


listStyle : List ( String, String ) -> Html msg
listStyle liste =
    Html.ul []
        (List.map (\( a, b ) -> Html.li [] [ text <| a ++ ", " ++ b ]) liste)
    



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        aktuelleListe =
            case model of
                Success list ->
                    list

                Failure ->
                    []

                Loading ->
                    []
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| aktuelleListe ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your stock data."

        Loading ->
            text "Loading..."

        Success list ->
            Html.div [] <|
                List.map (\fulltext -> pre [] [ listStyle <| transform <| csvString_to_data fulltext ]) list
        





