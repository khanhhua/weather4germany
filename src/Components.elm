module Components exposing (viewCityList, viewWeatherList, dropdownCityList)

import Html exposing (Html, Attribute, div, text, a)
import Html.Attributes exposing (class, colspan, width)
import Html.Events exposing (onClick)
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Table as Table exposing (cellAttr)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown exposing (State, DropdownToggle, DropdownItem)

import Constants
import Commons exposing (City, Msg(..), Weather)


viewCityList : List City -> Html Msg
viewCityList selectedCities =
    ListGroup.ul
        (List.map (\city ->
            ListGroup.li []
                [ a [ onClick (Commons.ToggleCity city.name) ] [ text city.name ]]
            ) selectedCities
        )

dropdownCityList : List City -> State -> Html Msg
dropdownCityList cities state =
    let
        selectedNames : List String
        selectedNames = List.map (\item -> item.name) cities

        selectableCities = Constants.cities
            |> List.filter (\item -> not(List.member item selectedNames))
    in
    Dropdown.dropdown
        state
        { toggleMsg = ToggleDropdown
        , toggleButton = Dropdown.toggle [ Button.light, Button.small, Button.attrs([ class "mb-1" ]) ] [ text "Select City" ]
        , options = [ Dropdown.alignMenuRight ]
        , items = case List.length selectableCities of
            0 -> [ Dropdown.buttonItem [] [ text "All cities are selected" ] ]
            _ -> selectableCities
                    |> List.map (\item -> Dropdown.buttonItem [ onClick (ToggleCity item) ] [ text item])
        }

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
                , Table.th [ Table.cellAttr ( width 5 ) ] []
                ]
            ]
            , tbody = case List.length weatherList of
                0 ->
                    Table.tbody []
                        [ Table.tr []
                            [ Table.td [ cellAttr(colspan 4), cellAttr(class "text-center") ]
                                [ text "Select a city"] ] ]
                _ ->
                    let
                        row : Weather -> Table.Row Msg
                        row weather = Table.tr []
                            [ Table.td [] [ text weather.city ]
                            , Table.td [] [ text (String.fromInt weather.id) ]
                            , Table.td [] [ text weather.main ]
                            , Table.td [] [ text weather.description ]
                            , Table.td []
                                [ Button.button
                                    [ Button.outlineDanger
                                    , Button.small
                                    , Button.onClick ( Commons.ToggleCity weather.city )
                                    ]
                                    [ text "Remove" ]
                                ]
                            ]
                    in
                    Table.tbody []
                        (List.map row weatherList)
            }
        ]
