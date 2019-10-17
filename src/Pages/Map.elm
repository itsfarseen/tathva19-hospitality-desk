module Pages.Map exposing (view)

import Element exposing (column, html, image)
import Html exposing (div)
import Html.Attributes exposing (style)
import Theme


view =
    column
        [ Element.padding 20
        , Element.spacing 20
        ]
        [ image
            [ Element.width Element.fill
            , Element.htmlAttribute (style "page-break-after" "always")
            ]
            { src = "/TathvaMap.png", description = "Map showing hospitality locations." }
        ]
