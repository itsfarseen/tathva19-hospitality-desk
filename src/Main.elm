module Main exposing (Model, Msg(..), init, main, update, view)

import AppState exposing (AppState)
import Backend exposing (Participant, getParticipants)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, column, layout, row, text)
import Element.Input as Input
import GlobalMsg exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Layout.Main as MainLayout
import Layout.Nav as NavLayout
import Layout.Print as PrintLayout
import List
import Pages
import Pages.Dashboard as Dashboard
import Pages.Login as Login
import Pages.Logout as Logout
import Pages.NotFound as NotFound
import Pages.ViewBill as ViewBill
import Theme
import Url


main : Program (Maybe String) MainModel Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type Display
    = Screen
    | Print


type alias MainModel =
    { model : Model, appState : AppState, display : Display }


type Model
    = Login Login.Model
    | PageNotFound
    | Logout
    | Dashboard Dashboard.Model
    | ViewBill ViewBill.Model


init : Maybe String -> Url.Url -> Nav.Key -> ( MainModel, Cmd Msg )
init flags url navkey =
    let
        appState =
            AppState.init flags navkey

        requestedPage =
            Pages.fromUrl <| Url.toString url

        initialDisplay =
            Screen
    in
    loadPage requestedPage initialDisplay appState
        |> handleGlobalMsg



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | ViewBillMsg ViewBill.Msg
    | GlobalMsg GlobalMsg


subscriptions : MainModel -> Sub Msg
subscriptions { model } =
    case model of
        Dashboard subModel ->
            Sub.map DashboardMsg (Dashboard.subscriptions subModel)

        _ ->
            Sub.none


handleGlobalMsg : ( MainModel, Cmd Msg, Maybe GlobalMsg ) -> ( MainModel, Cmd Msg )
handleGlobalMsg ( model, cmd, maybeGlobalMsg ) =
    case maybeGlobalMsg of
        Just globalMsg ->
            update (GlobalMsg globalMsg) model
                |> Tuple.mapSecond (\cmd1 -> Cmd.batch [ cmd, cmd1 ])

        Nothing ->
            ( model, cmd )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg model =
    case ( model.model, msg ) of
        ( _, UrlChanged url ) ->
            loadPage (Pages.fromUrl <| Url.toString url) model.display model.appState
                |> handleGlobalMsg

        ( _, UrlRequested _ ) ->
            -- TODO
            ( model, Cmd.none )

        ( _, GlobalMsg (Batch list) ) ->
            List.foldl
                (\msg1 ( model1, cmds ) ->
                    update (GlobalMsg msg1) model1 |> Tuple.mapSecond (\cmd1 -> cmds ++ [ cmd1 ])
                )
                ( model, [] )
                list
                |> Tuple.mapSecond Cmd.batch

        ( _, GlobalMsg EnterPrintMode ) ->
            ( { model | display = Print }, Cmd.none )

        ( _, GlobalMsg ExitPrintMode ) ->
            ( { model | display = Screen }, Cmd.none )

        ( _, GlobalMsg (LogIn { token }) ) ->
            let
                ( appState_, cmd ) =
                    AppState.setAuth model.appState (AppState.LoggedIn token)
            in
            ( { model | display = Screen, appState = appState_ }, cmd )

        ( _, GlobalMsg LogOut ) ->
            let
                ( appState_, cmd ) =
                    AppState.setAuth model.appState AppState.LoggedOut
            in
            ( { model | display = Screen, appState = appState_ }, cmd )

        ( _, GlobalMsg (RedirectToPage page) ) ->
            let
                -- Load page first, then changeUrl to avoid flickering
                -- when reloading from init functions
                ( newModel1, cmd1 ) =
                    loadPage page model.display model.appState
                        |> handleGlobalMsg

                ( newModel2, cmd2 ) =
                    changeUrlTo page newModel1
            in
            ( { newModel2 | display = Screen }, Cmd.batch [ cmd1, cmd2 ] )

        ( Login subModel, LoginMsg subMsg ) ->
            convertMsg (Login.update subModel subMsg) Login LoginMsg
                |> updateMainModel model
                |> handleGlobalMsg

        ( Dashboard subModel, DashboardMsg subMsg ) ->
            convertMsg (Dashboard.update subModel subMsg) Dashboard DashboardMsg
                |> updateMainModel model
                |> handleGlobalMsg

        ( _, _ ) ->
            ( model, Cmd.none )


getTitle : Pages.Page -> String
getTitle page =
    case page of
        Pages.Login ->
            Login.title

        Pages.NotFound ->
            NotFound.title

        Pages.Dashboard ->
            Dashboard.title

        Pages.Logout ->
            Logout.title

        Pages.ViewBill { billNo } ->
            ViewBill.title billNo


changeUrlTo : Pages.Page -> MainModel -> ( MainModel, Cmd Msg )
changeUrlTo page model =
    let
        navKey =
            AppState.getNavKey model.appState
    in
    ( model, Nav.pushUrl navKey (Pages.toUrl page) )


convertMsg : ( model, Cmd msg, a ) -> (model -> Model) -> (msg -> Msg) -> ( Model, Cmd Msg, a )
convertMsg ( model, cmd, a ) toModel toMsg =
    ( toModel model, Cmd.map toMsg cmd, a )


updateMainModel : MainModel -> ( Model, Cmd Msg, a ) -> ( MainModel, Cmd Msg, a )
updateMainModel mainModel ( model, cmd, a ) =
    ( { mainModel | model = model }, cmd, a )


loadPage : Pages.Page -> Display -> AppState -> ( MainModel, Cmd Msg, Maybe GlobalMsg )
loadPage page display appState =
    let
        ( model, cmd, maybeGlobalMsg ) =
            case AppState.getAuth appState of
                AppState.LoggedIn token ->
                    case page of
                        Pages.NotFound ->
                            ( PageNotFound, Cmd.none, Nothing )

                        Pages.Logout ->
                            ( Logout, Cmd.none, Just GlobalMsg.LogOut )

                        Pages.ViewBill { billNo } ->
                            convertMsg (ViewBill.init token billNo) ViewBill ViewBillMsg

                        _ ->
                            -- Everything else goes to dashboard
                            convertMsg (Dashboard.init token) Dashboard DashboardMsg

                AppState.LoggedOut ->
                    case page of
                        Pages.NotFound ->
                            ( PageNotFound, Cmd.none, Nothing )

                        _ ->
                            -- Everything else goes to dashboard
                            convertMsg Login.init Login LoginMsg
    in
    ( { model = model, display = display, appState = appState }, cmd, maybeGlobalMsg )


getPage : Model -> Pages.Page
getPage model =
    case model of
        Login _ ->
            Pages.Login

        Dashboard _ ->
            Pages.Dashboard

        PageNotFound ->
            Pages.NotFound

        Logout ->
            Pages.Logout

        ViewBill subModel ->
            Pages.ViewBill { billNo = ViewBill.getBillNo subModel }



-- VIEW


getPageView : MainModel -> Element Msg
getPageView { model, display } =
    case model of
        Login subModel ->
            Login.view subModel
                |> Element.map LoginMsg

        Dashboard subModel ->
            Dashboard.view subModel
                |> Element.map DashboardMsg

        PageNotFound ->
            NotFound.view

        Logout ->
            Logout.view

        ViewBill subModel ->
            ViewBill.view subModel (display == Print)
                |> Element.map ViewBillMsg


view : MainModel -> Browser.Document Msg
view { model, appState, display } =
    { title = "Hospitality | " ++ (getPage model |> getTitle)
    , body =
        [ layout [] <|
            Theme.root <|
                case display of
                    Screen ->
                        MainLayout.view
                            (NavLayout.view
                                (getPage model)
                                (AppState.getAuth appState)
                                (GlobalMsg << RedirectToPage)
                                getTitle
                            )
                            (getPageView { model = model, appState = appState, display = display })

                    Print ->
                        PrintLayout.view
                            (getPageView { model = model, appState = appState, display = display })
        ]
    }
