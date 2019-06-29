module Components exposing (viewCityList)

import Html exposing (Html, Attribute, div, h1, ul, li, span, text, a)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Bootstrap.ListGroup as ListGroup

import Constants
import Commons exposing (Msg)

viewCityList : List String -> Html Msg
viewCityList selectedCities =
    ListGroup.ul
        (List.map (\city ->
            let
                isCitySelected = List.member city selectedCities
            in
                ListGroup.li []
                [ a [ onClick (Commons.ToggleCity city) ] [ text city ]
                , case isCitySelected of
                    True -> span [ class "float-right" ] [ text (String.fromChar (Char.fromCode 9989)) ]
                    False -> span [] []
                ]) Constants.cities
        )
