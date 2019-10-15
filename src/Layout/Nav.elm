module Layout.Nav exposing (..)

import Element exposing (Element, column, text)
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Page -> (Page -> msg) -> (Page -> String) -> Element msg
view activePage redirectFn titleFn =
    column [ Element.alignTop ]
        (List.map
            (\page -> navElement page (page == activePage) redirectFn titleFn)
            Pages.listForNav
        )


navElement : Page -> Bool -> (Page -> msg) -> (Page -> String) -> Element msg
navElement targetPage isActive redirectFn titleFn =
    Input.button
        (if isActive then
            [ Font.underline ]

         else
            []
        )
        { label = text (titleFn targetPage), onPress = Just (redirectFn targetPage) }
