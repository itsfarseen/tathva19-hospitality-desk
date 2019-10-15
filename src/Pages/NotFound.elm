module Pages.NotFound exposing (title, view)

import Backend
import Element exposing (column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import Html
import Theme


title =
    "Error 404"


view : Html.Html msg
view =
    layout [] <|
        column [ Element.width (Element.px 400), Element.paddingXY 20 0, Element.centerX, Element.centerY, Font.size 15, Element.spacing 20 ]
            [ Element.el (Theme.pageTitle ++ [ Element.moveUp 20.0 ]) (text "Page Not Found")
            ]
