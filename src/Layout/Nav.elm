module Layout.Nav exposing (..)

import Element exposing (Element, column, text)
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Page -> (Page -> msg) -> Element msg
view activePage redirectFn =
    column [ Element.alignTop ]
        (List.map
            (\page -> navElement page (page == activePage) redirectFn)
            Pages.listForNav
        )


navElement : Page -> Bool -> (Page -> msg) -> Element msg
navElement targetPage isActive redirectFn =
    Element.el [ onMouseDown (redirectFn targetPage) ] (text (Pages.getTitle targetPage))
