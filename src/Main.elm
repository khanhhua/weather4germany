module Main exposing (main)

import Debug
import Http
import Json.Decode as D exposing (Decoder)
import Maybe

import Browser exposing (Document, UrlRequest)
import Url exposing (Url)
import Browser.Navigation exposing (Key)
import Html exposing (Html, div, h1, ul, li, span, text, a)
import Html.Attributes exposing (colspan, href, class)
import Html.Events exposing (onClick)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table exposing (cellAttr)

import Commons exposing (..)
import Components exposing (viewCityList)

-- API KEY = 47b167289268601ac3223838e2d3de5a

type alias Flags = {}

type alias Model =
    { cities : List(City)
    , data : List Weather
    }

weatherDecoder : Decoder WeatherResponse
weatherDecoder =
    D.map2 WeatherResponse
        (D.map2 City
            (D.field "id" D.int)
            (D.field "name" D.string)
        )
        (D.at ["weather", "0"]
            (D.map3 Weather
                (D.field "id" D.int )
                (D.field "main" D.string )
                (D.field "description" D.string )
            )
        )


weatherListDecoder : Decoder (List WeatherResponse)
weatherListDecoder =
    D.field "list" (D.list weatherDecoder)

init : Flags -> Url.Url -> Key -> (Model, Cmd Msg)
init flags url key =
    ( { cities = []
      , data = []
      }
    , Cmd.none )

view : Model -> Document Msg
view model =
    { title = "Weather in Germany"
    , body =
        [ CDN.stylesheet
        , Grid.container []
            [ h1 [] [ text "Weather in Germany" ]
            , Grid.row []
                [ Grid.col [ Col.sm4 ]
                    [(viewCityList model.cities )]
                , Grid.col [ Col.sm8 ]
                    [ Table.table
                        { options = []
                        , thead = Table.thead [] [ Table.tr []
                            [ Table.th [] [ text "ID" ]
                            , Table.th [] [ text "Main" ]
                            , Table.th [] [ text "Description" ]
                            ]
                        ]
                        , tbody = case List.length model.data of
                            0 ->
                                Table.tbody [] [ Table.tr [] [ Table.td [ cellAttr(colspan 3) ] [ text "Empty"] ] ]
                            _ ->
                                let
                                    row : Weather -> Table.Row msg
                                    row weather = Table.tr []
                                        [ Table.td [] [ text (String.fromInt weather.id) ]
                                        , Table.td [] [ text weather.main ]
                                        , Table.td [] [ text weather.description ]
                                        ]
                                in
                                Table.tbody []
                                    (List.map row model.data)
                        }
                    ]
                ]
            ]
        ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ( ToggleCity cityName ) ->
            let
                isCitySelected = List.any (\item -> item.name == cityName) model.cities
            in
            if isCitySelected then
                ({ model | cities = List.filter (\item -> item.name /= cityName) model.cities }, Cmd.none)
            else
                let
                    fetchCmd : Cmd Msg
                    fetchCmd = Http.get
                        { url = "https://api.openweathermap.org/data/2.5/weather?q=" ++ cityName ++ ",de&APPID=47b167289268601ac3223838e2d3de5a"
                        , expect = Http.expectJson GetCurrentWeather weatherDecoder
                        }
                    city = { id = 0, name =cityName }
                in
                ({ model | cities = model.cities ++ [city] }, fetchCmd)
        ( GetCurrentWeather result ) ->
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
                            { url = "http://api.openweathermap.org/data/2.5/group?id=" ++ (String.join "," cityIds) ++ "&APPID=47b167289268601ac3223838e2d3de5a"
                            , expect = Http.expectJson RefreshWeathers weatherListDecoder
                            }
                    in
                    ({ model | cities = cities }, fetchCmd)
                Err err ->
                    Debug.log "Scheisse passiert allen"
                    (model, Cmd.none)
        ( RefreshWeathers result ) ->
            case result of
                Ok data ->
                    let
                        weatherList = data |> List.map (\item -> item.weather)
                    in
                    ({ model | data = weatherList }, Cmd.none)
                Err err ->
                    Debug.log "Scheisse passiert allen"
                    (model, Cmd.none)
        _ -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main : Program Flags Model Msg
main = Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = UrlChange
    }
