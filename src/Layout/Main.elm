module Layout.Main exposing (..)

import Element exposing (Element, row, text)
import Element.Background as Background
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Element msg -> Element msg -> Element msg
view aside main =
    row [ Element.width Element.fill, Element.height Element.fill, Element.padding 20, Element.spacing 20 ]
        [ aside, main ]
