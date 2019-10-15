module Pages exposing (Page(..), allowedPages, fromUrl, getTitle, toUrl)

import AppState
import Browser.Navigation as Nav
import Pages.Login as Login
import Pages.NotFound as NotFound
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s)


type Page
    = Login
    | NotFound


fromUrl : String -> Page
fromUrl string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            let
                parser =
                    oneOf
                        [ map Login (s "login") ]
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

                NotFound ->
                    [ "404" ]
    in
    "#/" ++ String.join "/" pieces


getTitle : Page -> String
getTitle page =
    case page of
        Login ->
            Login.title

        NotFound ->
            NotFound.title


allowedPages : AppState.Auth -> List Page
allowedPages authState =
    case authState of
        AppState.LoggedIn _ ->
            [ NotFound ]

        AppState.LoggedOut ->
            [ Login, NotFound ]
