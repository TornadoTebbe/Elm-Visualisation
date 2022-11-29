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
    = Success String
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success fullText ->
            pre[] [ text fullText]
        
    









