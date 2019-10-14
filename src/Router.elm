module Router exposing (toRoute)

import Browser.Navigation as Nav
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s)


type Route
    = Login
    | NotFound


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            parse parser url
                |> Maybe.withDefault NotFound


redirectTo : Nav.Key -> Route -> Cmd msg
redirectTo key route =
    Nav.replaceUrl key (routeToString route)



-- PRIVATE


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Login (s "login") ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Login ->
                    [ "login" ]

                NotFound ->
                    [ "404" ]
    in
    "#/" ++ String.join "/" pieces
