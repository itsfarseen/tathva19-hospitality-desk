module Pages exposing (Page(..), allowedAuthStates, fromUrl, toUrl)

import AppState
import Browser.Navigation as Nav
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


allowedAuthStates : Page -> Maybe AppState.Auth
allowedAuthStates page =
    -- Nothing means allowed everywhere
    case page of
        Login ->
            Just AppState.LoggedOut

        NotFound ->
            Nothing
