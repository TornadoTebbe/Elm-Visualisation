module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Baumvisualisierung exposing (Msg)


main : Html Msg
main = 
    Html.div [Html.Attributes.style "padding" "20px"]
        [Html.h1 []
            [   Html.text "Modul Information Retrieval und Visualisierung Sommersemester 2022"
            , Html.br [][]        
            ]

        , Html.h3 []
            [ Html.text "Anbei finden Sie die drei Visualisierungsmöglichkeiten zur Darstellung der kaggle-Daten Student_Behaviour"
            , Html.br [][]
            ]

        , Html.h4 []
            [ Html.text "Autor: Paul Tebbe"
            , Html.br [][]
            ]

        , nav []
            [Html.a[href "ScatterplotDaten.elm"] [Html.text "Scatterplot"]
            , Html.br [] []
            , Html.a[href "ParallelPlot.elm"] [Html.text "Parallele Koordinaten"]
            , Html.br [] []
            , Html.a[href "Baumvisualisierung.elm"] [Html.text "Baumvisualisierung"]
            , Html.br [] []
            ]

    
        ,Html.br[][]
        , Html.p [Html.Attributes.style "fontSize" "17px"]
        [Html.text "Die hier visualisierten Daten sollen dem/den Nutzer:innen eine ungefähre Vorstellung darüber verschaffen, ob Noten und die Lernzeiten von Studenten in einem Zusammenhang stehen."
        , Html.br [] []
        , Html.text "Der/die Nutzer:innen kann sich über die hrefs zu den verschiedenen Visaulisierungen bewegen."
        , Html.br[][]
        , Html.text ""
        ]

        
        ] 

        
            

        






