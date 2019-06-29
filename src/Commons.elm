module Commons exposing (..)

import Browser exposing (UrlRequest)
import Http
import Url exposing (Url)

type Msg
    = ToggleCity String
    | GetCurrentWeather (Result Http.Error (List Weather))
    | ClickedLink UrlRequest
    | UrlChange Url

type alias Weather =
    { id : Int
    , main : String
    , description : String
    }

type alias WeatherList = List(Weather)
