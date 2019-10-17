module Pages exposing (Page(..), fromUrl, listForNav, toUrl)

import AppState
import Browser.Navigation as Nav
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Page
    = Login
    | Dashboard
    | Logout
    | NotFound
    | ViewBill { billNo : String }
    | PrintBill { billNo : String }


listForNav : AppState.Auth -> List Page
listForNav auth =
    case auth of
        AppState.LoggedIn _ ->
            [ Dashboard, Logout ]

        AppState.LoggedOut ->
            [ Login ]


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
                        , map Logout (s "logout")
                        , map
                            (\billNo -> ViewBill { billNo = billNo })
                            (s "bill" </> Url.Parser.string)
                        , map
                            (\billNo -> PrintBill { billNo = billNo })
                            (s "bill" </> Url.Parser.string </> s "print")
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

                Logout ->
                    [ "logout" ]

                Dashboard ->
                    []

                NotFound ->
                    [ "404" ]

                ViewBill { billNo } ->
                    [ "bill", billNo ]

                PrintBill { billNo } ->
                    [ "bill", billNo, "print" ]
    in
    "/" ++ String.join "/" pieces
