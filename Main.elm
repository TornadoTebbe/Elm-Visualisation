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


type Status
    = Success
    | Loading
    | Failure


type alias Model =
    { status : Status
    , data : List Student_Data
    }


type Msg
    = GotText (Result Http.Error String)

--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
    let
        data =
            [ "Student_Behaviour"]
        cmds =
            List.map
                (\x ->
                    Http.get
                        { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest2/main/Daten/Student_Behaviour.csv"
                        , expect = Http.expectString GotText
                        }
                )
                data
    in
    ( { status = Loading, data = "" }
    , Cmd.batch cmds
    )







update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | status = Success, data = model.data ++ fullText }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success fullText ->
            pre[] [ text (String.fromInt (list.length(datListe [fullText])))]
        
    




main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }




