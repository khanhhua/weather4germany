module Commons exposing (..)

import Browser exposing (UrlRequest)
import Http
import Url exposing (Url)
import Bootstrap.Dropdown as Dropdown

type Msg
    = ToggleCity String
    | Refresh
    | OnCitySelected (Result Http.Error (WeatherResponse))
    | OnWeathersRefreshed (Result Http.Error (List WeatherResponse))
    | ClickedLink UrlRequest
    | UrlChange Url
    | ToggleDropdown Dropdown.State

type alias Weather =
    { id : Int
    , main : String
    , description : String
    , city : String
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
