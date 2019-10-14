module Components.Login exposing (..)

import Element exposing (column, layout, row, text)
import Element.Input as Input
import Html exposing (Html)


type Msg
    = LogIn { userid : String, password : String }
    | UserIDUpdate String
    | PasswordUpdate String


type alias Model =
    { userid : String, password : String }
