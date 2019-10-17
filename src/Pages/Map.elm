module Pages.Map exposing (view)

import Element exposing (column, html, image)
import Html exposing (div)
import Html.Attributes exposing (style)
import Theme


view =
    column
        [ Element.paddingXY 20 20
        , Element.spacing 20
        ]
        [ image
            [ Element.width Element.fill, Element.height Element.fill ]
            { src = "/TathvaMap.png", description = "Map showing hospitality locations." }
        ]
