module Components exposing (viewCityList, viewWeatherList)

import Html exposing (Html, Attribute, div, span, text, a)
import Html.Attributes exposing (class, colspan)
import Html.Events exposing (onClick)
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Table as Table exposing (cellAttr)
import Bootstrap.Button as Button

import Constants
import Commons exposing (City, Weather, Msg)

viewCityList : List City -> Html Msg
viewCityList selectedCities =
    ListGroup.ul
        (List.map (\cityName ->
            let
                isCitySelected = List.any (\item -> item.name == cityName ) selectedCities
            in
                ListGroup.li []
                [ a [ onClick (Commons.ToggleCity cityName) ] [ text cityName ]
                , case isCitySelected of
                    True -> span [ class "float-right" ] [ text (String.fromChar (Char.fromCode 9989)) ]
                    False -> span [] []
                ]) Constants.cities
        )

viewWeatherList : List Weather -> Html Msg
viewWeatherList weatherList =
    div []
        [ Button.button [ Button.attrs ([ class "float-right mb-1" ]), Button.primary, Button.onClick Commons.Refresh ]
            [ text "Refresh" ]
        , Table.table
            { options = []
            , thead = Table.thead [] [ Table.tr []
                [ Table.th [] [ text "City" ]
                , Table.th [] [ text "ID" ]
                , Table.th [] [ text "Main" ]
                , Table.th [] [ text "Description" ]
                ]
            ]
            , tbody = case List.length weatherList of
                0 ->
                    Table.tbody [] [ Table.tr [] [ Table.td [ cellAttr(colspan 3) ] [ text "Empty"] ] ]
                _ ->
                    let
                        row : Weather -> Table.Row msg
                        row weather = Table.tr []
                            [ Table.td [] [ text weather.city ]
                            , Table.td [] [ text (String.fromInt weather.id) ]
                            , Table.td [] [ text weather.main ]
                            , Table.td [] [ text weather.description ]
                            ]
                    in
                    Table.tbody []
                        (List.map row weatherList)
            }
        ]
