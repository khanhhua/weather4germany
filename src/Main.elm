module Main exposing (main)

import Debug
import Http
import Json.Decode as D exposing (Decoder)
import Maybe

import Browser exposing (Document, UrlRequest)
import Maybe exposing (Maybe)
import Url exposing (Url)
import Browser.Navigation exposing (Key)
import Html exposing (Html, div, h1, ul, li, span, text, a)
import Html.Attributes exposing (colspan, href, class)
import Html.Events exposing (onClick)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table exposing (cellAttr)

import Constants
import Commons exposing (..)
import Components exposing (viewCityList)

-- API KEY = 47b167289268601ac3223838e2d3de5a

type alias Flags = {}

type alias Model =
    { cities : List(String)
    , data : Maybe(List Weather)
    }

weatherDecoder : Decoder Weather
weatherDecoder =
    D.map3 Weather
        (D.field "id" D.int)
        (D.field "main" D.string)
        (D.field "description" D.string)

weatherListDecoder : Decoder (List Weather)
weatherListDecoder =
    D.field "weather" (D.list weatherDecoder)

init : Flags -> Url.Url -> Key -> (Model, Cmd Msg)
init flags url key =
    ( { cities = []
      , data = Nothing
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
                        , tbody = case model.data of
                            Just data ->
                                let
                                    row : Weather -> Table.Row msg
                                    row weather = Table.tr []
                                        [ Table.td [] [ text (String.fromInt weather.id) ]
                                        , Table.td [] [ text weather.main ]
                                        , Table.td [] [ text weather.description ]
                                        ]
                                in
                                Table.tbody []
                                    (List.map row data)
                            Nothing ->
                                Table.tbody [] [ Table.tr [] [ Table.td [ cellAttr(colspan 3) ] [ text "Empty"] ] ]
                        }
                    ]
                ]
            ]
        ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ( ToggleCity city ) ->
            let
                isCitySelected = List.member city model.cities
            in
            if isCitySelected then
                ({ model | cities = List.filter (\item -> item /= city) model.cities }, Cmd.none)
            else
                let
                    fetchCmd : Cmd Msg
                    fetchCmd = Http.get
                        { url = "https://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ ",de&APPID=47b167289268601ac3223838e2d3de5a"
                        , expect = Http.expectJson GetCurrentWeather weatherListDecoder
                        }
                in
                ({ model | cities = model.cities ++ [city] }, fetchCmd)
        ( GetCurrentWeather result ) ->
            case result of
                Ok data ->
                    ({ model | data = Maybe.Just(data) }, Cmd.none)
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
