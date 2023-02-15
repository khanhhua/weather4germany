module Main exposing (main)

import Http
import Json.Decode as D exposing (Decoder)

import Browser exposing (Document, UrlRequest)
import Url exposing (Url)
import Browser.Navigation exposing (Key)
import Html exposing (Html, h1, div, text)
import Html.Attributes exposing (class)

import Bootstrap.CDN as CDN
import Bootstrap.Grid.Row exposing (attrs)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Dropdown as Dropdown

import Commons exposing (..)
import Components exposing (dropdownCityList, viewWeatherList)

-- API KEY = 47b167289268601ac3223838e2d3de5a

type alias Flags = {}

type alias Model =
    { cities : List(City)
    , weatherList : List Weather
    , cityDropDownState: Dropdown.State
    }

weatherDecoder : Decoder WeatherResponse
weatherDecoder =
    D.map2 WeatherResponse
        (D.map2 City
            (D.field "id" D.int)
            (D.field "name" D.string)
        )
        (D.map5 Weather
            ( D.at ["weather", "0", "id"] D.int )
            ( D.at ["weather", "0", "main"] D.string )
            ( D.at ["weather", "0", "description"] D.string )
            ( D.field "name" D.string )
            ( D.at ["main", "temp"] D.float )
        )


weatherListDecoder : Decoder (List WeatherResponse)
weatherListDecoder =
    D.field "list" (D.list weatherDecoder)

init : Flags -> Url.Url -> Key -> (Model, Cmd Msg)
init flags url key =
    ( { cities = []
      , weatherList = []
      , cityDropDownState = Dropdown.initialState
      }
    , Cmd.none )

view : Model -> Document Msg
view model =
    { title = "Weather in Germany"
    , body =
        [ CDN.stylesheet
        , Grid.container [ class "mt-5" ]
            [ h1 [] [ text "Weather in Germany" ]
            , Grid.row [ attrs([ class "justify-content-center" ]) ]
                [ Grid.col [ Col.lg8, Col.md ]
                    [ (viewWeatherList model.weatherList)
                    , div [ class "text-center" ]
                        [ (dropdownCityList model.cities model.cityDropDownState)
                        ]
                    ]
                ]
            ]
        ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ( ToggleDropdown state ) ->
            ( { model | cityDropDownState = state }, Cmd.none)
        ( ToggleCity cityName ) ->
            let
                isCitySelected = List.any (\item -> item.name == cityName) model.cities
                weatherList = model.weatherList
                    |> List.filter (\item -> item.city /= cityName)
            in
            -- Remove if isCitySelected
            if isCitySelected then
                ({ model
                | cities = List.filter (\item -> item.name /= cityName) model.cities
                , weatherList = weatherList
                }, Cmd.none)
            else
                let
                    fetchCmd : Cmd Msg
                    fetchCmd = Http.get
                        { url = "https://api.openweathermap.org/data/2.5/weather?q=" ++ cityName ++ ",de&units=metric&APPID=47b167289268601ac3223838e2d3de5a"
                        , expect = Http.expectJson OnCitySelected weatherDecoder
                        }
                    city = { id = 0, name =cityName }
                in
                ({ model | cities = model.cities ++ [city] }, fetchCmd)
        ( OnCitySelected result ) ->
            case result of
                Ok data ->
                    let
                        currentCity = data.city
                        cities = List.map(\item ->
                            if currentCity.name == item.name then
                                currentCity
                            else
                                item
                            ) model.cities
                        cityIds = cities |> List.map (\item -> (String.fromInt item.id))

                        fetchCmd : Cmd Msg
                        fetchCmd = Http.get
                            { url = "https://api.openweathermap.org/data/2.5/group?id=" ++ (String.join "," cityIds) ++ "&units=metric&APPID=47b167289268601ac3223838e2d3de5a"
                            , expect = Http.expectJson OnWeathersRefreshed weatherListDecoder
                            }
                    in
                    ({ model | cities = cities }, fetchCmd)
                Err _ ->
                    (model, Cmd.none)
        ( OnWeathersRefreshed result ) ->
            case result of
                Ok data ->
                    let
                        weatherList = data
                            |> List.map (\item ->
                                    let
                                        weather = item.weather
                                    in
                                    { weather | city = item.city.name }
                                )
                    in
                    ({ model | weatherList = weatherList }, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        ( Refresh ) ->
            let
                cityIds = model.cities |> List.map (\item -> (String.fromInt item.id))
                fetchCmd : Cmd Msg
                fetchCmd = Http.get
                    { url = "https://api.openweathermap.org/data/2.5/group?id=" ++ (String.join "," cityIds) ++ "&APPID=47b167289268601ac3223838e2d3de5a"
                    , expect = Http.expectJson OnWeathersRefreshed weatherListDecoder
                    }
            in
            ( model, fetchCmd )
        _ -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.cityDropDownState ToggleDropdown ]

main : Program Flags Model Msg
main = Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = UrlChange
    }
