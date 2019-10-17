module Theme exposing (..)

import Element exposing (column, layout, rgb255, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


primary =
    -- "#5a2ca0ff"
    rgb255 0x5A 0x2C 0xA0


primaryLight1 =
    -- "#7137c8ff"
    rgb255 0x71 0x37 0xC8


primaryLight2 =
    -- "#8d5fd3ff"
    rgb255 0x8D 0x5F 0xFD


primaryDark1 =
    -- "#442178ff"
    rgb255 0x44 0x21 0x78


primaryDark2 =
    -- "#2d1650ff"
    rgb255 0x2D 0x16 0x50


neutral0 =
    -- "#ffffff"
    rgb255 0xFF 0xFF 0xFF


neutral1 =
    -- "#f2f2f2"
    rgb255 0xF2 0xF2 0xF2


neutral2 =
    -- "#e6e6e6"
    rgb255 0xE6 0xE6 0xE6


neutral3 =
    -- "#cccccc"
    rgb255 0xCC 0xCC 0xCC


fg0 =
    -- "#1f1c24ff"
    rgb255 0x1F 0x1C 0x24


fg1 =
    -- "#666666"
    rgb255 0x66 0x66 0x66


success =
    -- "#00aa88ff"
    rgb255 0x00 0xAA 0x88


error =
    -- "#d35f5fff"
    rgb255 0xD3 0x5F 0x5F


fontFamily =
    Font.family [ Font.typeface "Roboto", Font.sansSerif ]


root content =
    Element.el [ fontFamily, Font.color fg1, Element.width Element.fill ] content


title t =
    Element.el [ Font.size 40, Font.color fg1, Font.bold ] (text (String.toUpper t))


title1 t =
    Element.el [ Font.size 20, Font.color fg1, Font.bold ] (text (String.toUpper t))


title2 t =
    Element.paragraph [ Font.size 12, Font.color fg1, Font.bold ] [ text (String.toUpper t) ]


baseControl =
    [ Border.color neutral3
    , Element.width (Element.fillPortion 2)
    , Element.focused [ Border.color primaryLight2 ]
    , Element.mouseOver [ Border.color primaryLight2 ]
    , Border.width 1
    , Element.height (Element.px 30)
    , Border.rounded 0
    , Element.padding 5
    ]


labelLeft str =
    Input.labelLeft
        [ Element.centerY
        , Element.width (Element.fillPortion 1)
        , Font.bold
        ]
        (title2 (String.toUpper str))


labelAbove str =
    Input.labelAbove [] (text (String.toUpper str))


inputText label value onChange =
    Input.text
        baseControl
        { label = label
        , text = value
        , onChange = onChange
        , placeholder = Nothing
        }


inputPassword label value onChange =
    Input.newPassword
        baseControl
        { label = label
        , text = value
        , onChange = onChange
        , placeholder = Nothing
        , show = False
        }


buttonBase enabled =
    [ Border.rounded 0
    , Element.alignRight
    , Element.padding 10
    , if enabled then
        Background.color primary

      else
        Background.color primaryLight2
    , Font.color neutral0
    ]
        ++ (if enabled then
                [ Element.mouseDown <|
                    [ Background.color primaryDark1
                    ]
                , Element.mouseOver <|
                    [ Background.color primaryLight1
                    ]
                ]

            else
                []
           )


button label action enabled =
    Input.button
        (buttonBase enabled)
        { label =
            Element.el
                [ Font.bold
                , Font.size 12
                , Font.color neutral1
                , Element.centerX
                ]
                (text (String.toUpper label))
        , onPress =
            if enabled then
                Just action

            else
                Nothing
        }


pageCardAttrs =
    [ Background.color neutral2
    , Border.color neutral3
    , Border.width 1
    , Element.width Element.fill
    ]


pageCardTitleBar title_ rightItem =
    Element.row
        [ Element.padding 20, Element.width Element.fill ]
        [ Element.el [ Element.width (Element.fillPortion 1) ] (title1 title_)
        , Element.el [ Element.width (Element.fillPortion 1), Element.alignRight ] rightItem
        ]


pageSubCardAttrs =
    [ Background.color neutral1
    , Element.padding 20
    , Element.spacing 20
    , Element.width Element.fill
    , Border.width 1
    , Border.color neutral3
    ]
