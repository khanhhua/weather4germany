module Commons exposing (..)

import Browser exposing (UrlRequest)
import Http
import Url exposing (Url)

type Msg
    = ToggleCity String
    | GetCurrentWeather (Result Http.Error (WeatherResponse))
    | RefreshWeathers (Result Http.Error (List WeatherResponse))
    | ClickedLink UrlRequest
    | UrlChange Url

type alias Weather =
    { id : Int
    , main : String
    , description : String
    }

type alias WeatherList = List(Weather)

type alias City =
    { id : Int
    , name : String
    }

type alias WeatherResponse =
    { city : City
    , weather: Weather
    }
