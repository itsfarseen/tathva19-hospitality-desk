module Components.Theme exposing (..)

import Element exposing (column, layout, rgb255, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


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
    -- "#dcdcdcff"
    rgb255 0xDC 0xDC 0xDC


neutral1 =
    -- "#c5c5c5ff"
    rgb255 0xC5 0xC5 0xC5


neutral2 =
    -- "#6b6b6bff"
    rgb255 0x6B 0x6B 0x6B


neutral3 =
    -- "#1f1c24ff"
    rgb255 0x1F 0x1C 0x24


success =
    -- "#00aa88ff"
    rgb255 0x00 0xAA 0x88


error =
    -- "#d35f5fff"
    rgb255 0xD3 0x5F 0x5F


pageTitle =
    [ Font.size 30 ]


baseControl =
    [ Border.color primaryLight2, Border.width 1 ]


input =
    baseControl
        ++ [ Border.rounded 0 ]


button =
    [ Border.rounded 0
    , Element.padding 10
    , Element.width <| Element.minimum 1 (Element.px 80)
    , Background.color primary
    , Font.color neutral0
    , Element.mouseDown <|
        [ Background.color primaryDark1
        ]
    , Element.mouseOver <|
        [ Background.color primaryLight1
        ]
    ]
