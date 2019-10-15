module Layout.Main exposing (..)

import Element exposing (Element, row, text)
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Element msg -> Element msg -> Element msg
view aside main =
    row [ Element.width Element.fill, Element.height Element.fill ]
        [ aside, main ]
