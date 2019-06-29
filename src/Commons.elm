module Commons exposing (..)

import Browser exposing (UrlRequest)
import Url exposing (Url)

type Msg
    = ToggleCity String
    | GetCurrentWeather (List String)
    | ClickedLink UrlRequest
    | UrlChange Url
