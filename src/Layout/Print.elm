module Layout.Print exposing (..)

import Element exposing (Element, row, text)
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Element msg -> Element msg
view main =
    row [ Element.width Element.fill, Element.height Element.fill, Element.padding 20, Element.spacing 20 ]
        [ main ]
