module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Baumvisualisierung exposing (Msg)


main : Html Msg
main = 
    Html.div [Html.Attributes.style "padding" "20px"]
        [Html.h1 []
            [   Html.text "Modul Information Retrieval und Visualisierung Sommersemester 2022"
        
        
            ]

        , Html.h2 []
            [ Html.text "Darstellung der Daten Student Behaviour"

            ]
         
        , Html.p [Html.Attributes.style "fontSize" "16px"] 
        [ Html.a[href "ScatterplotDaten.elm"] [Html.text "Scatterplot"]
        , Html.br [] []
        , Html.a[href "ParallelPlot.elm"] [Html.text "Parallele Koordinaten"]
        , Html.br [] []
        , Html.a[href "Baumvisualisierung.elm"] [Html.text "Baumvisualisierung"]
        , Html.br [] []
        ]

        
        ] 

        
            

        






