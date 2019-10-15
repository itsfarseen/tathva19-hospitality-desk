module Pages exposing (Page(..), allowedPages, fromUrl, listForNav, toUrl)

import AppState
import Browser.Navigation as Nav
import Pages.Dashboard as Dashboard
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Page
    = Login
    | Dashboard
    | NotFound


listForNav : List Page
listForNav =
    [ Login, Dashboard ]


fromUrl : String -> Page
fromUrl string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            let
                parser =
                    oneOf
                        [ map Login (s "login")
                        , map Dashboard top
                        ]
            in
            parse parser url
                |> Maybe.withDefault NotFound


toUrl : Page -> String
toUrl page =
    let
        pieces =
            case page of
                Login ->
                    [ "login" ]

                Dashboard ->
                    []

                NotFound ->
                    [ "404" ]
    in
    "/" ++ String.join "/" pieces


allowedPages : AppState.Auth -> List Page
allowedPages authState =
    case authState of
        AppState.LoggedIn _ ->
            [ Dashboard, NotFound ]

        AppState.LoggedOut ->
            [ Login, NotFound ]
