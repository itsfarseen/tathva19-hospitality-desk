module Layout.Nav exposing (..)

import AppState
import Element exposing (Element, column, text)
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Pages exposing (Page)
import Theme


view : Page -> AppState.Auth -> (Page -> msg) -> (Page -> String) -> Element msg
view activePage authState redirectFn titleFn =
    column [ Element.alignTop ]
        (List.filter
            (\page -> List.member page (Pages.allowedPages authState))
            Pages.listForNav
            |> List.map
                (\page -> navElement page (page == activePage) redirectFn titleFn)
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
