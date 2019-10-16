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
import Url


main : Program (Maybe String) MainModel Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type Display
    = Screen
    | Print


type alias MainModel =
    ( Model, Display )


type Model
    = Login Login.Model
    | PageNotFound AppState
    | Logout AppState
    | Dashboard Dashboard.Model


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



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | ParticipantsDataResult (Result (Backend.Error ()) (List Participant))
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | GlobalMsg GlobalMsg


type alias PageUpdate subModel subMsg =
    ( subModel, Cmd subMsg, Maybe GlobalMsg )


updateModel : PageUpdate subModel subMsg -> Display -> (subModel -> Model) -> (subMsg -> Msg) -> ( MainModel, Cmd Msg )
updateModel ( subModel, subCmd, maybeGlobalMsg ) display toModel toMsg =
    -- FIXME: This function is poorly named
    let
        ( model, cmd ) =
            case maybeGlobalMsg of
                Just globalMsg ->
                    update (GlobalMsg globalMsg) ( toModel subModel, display )

                Nothing ->
                    ( ( toModel subModel, display ), Cmd.none )
    in
    ( model, Cmd.batch [ Cmd.map toMsg subCmd, cmd ] )


toMainModel : Display -> Model -> MainModel
toMainModel display model =
    ( model, display )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg ( model, display ) =
    case ( model, msg ) of
        ( _, UrlChanged url ) ->
            loadPage (Pages.fromUrl <| Url.toString url) display (getAppState model)

        ( _, UrlRequested _ ) ->
            ( ( model, Screen ), Cmd.none )

        ( _, GlobalMsg (RedirectToPage Pages.Logout) ) ->
            let
                ( newAppState, cmd1 ) =
                    AppState.setAuth (getAppState model) AppState.LoggedOut

                ( newModel, cmd2 ) =
                    update (GlobalMsg (RedirectToPage Pages.Login)) ( setAppState model newAppState, Screen )
            in
            ( newModel, Cmd.batch [ cmd1, cmd2 ] )

        ( _, GlobalMsg EnterPrintMode ) ->
            ( ( model, Print ), Cmd.none )

        ( _, GlobalMsg ExitPrintMode ) ->
            ( ( model, Screen ), Cmd.none )

        ( _, GlobalMsg (RedirectToPage page) ) ->
            let
                -- Load page first, then changeUrl to avoid flickering
                -- when reloading from init functions
                ( newModel1, cmd1 ) =
                    loadPage page display (getAppState model)

                ( newModel2, cmd2 ) =
                    changeUrlTo page (Tuple.first newModel1)
            in
            ( ( newModel2, Screen ), Cmd.batch [ cmd1, cmd2 ] )

        ( _, ParticipantsDataResult (Ok participants) ) ->
            let
                ( appState, cmd ) =
                    AppState.setParticipantsList (getAppState model) participants
            in
            ( ( setAppState model appState, display ), cmd )

        ( Login subModel, LoginMsg subMsg ) ->
            updateModel (Login.update subModel subMsg) display Login LoginMsg

        ( Login _, _ ) ->
            ( ( model, display ), Cmd.none )

        ( Logout _, _ ) ->
            ( ( model, display ), Cmd.none )

        ( Dashboard subModel, DashboardMsg subMsg ) ->
            let
                ( subModel_, subCmd ) =
                    Dashboard.update subModel subMsg
            in
            ( ( Dashboard subModel_, display ), Cmd.map DashboardMsg subCmd )

        ( Dashboard _, _ ) ->
            ( ( model, display ), Cmd.none )

        ( PageNotFound _, _ ) ->
            ( ( model, display ), Cmd.none )


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


changeUrlTo : Pages.Page -> Model -> ( Model, Cmd Msg )
changeUrlTo page model =
    let
        navKey =
            AppState.getNavKey (getAppState model)
    in
    ( model, Nav.pushUrl navKey (Pages.toUrl page) )


loadPage : Pages.Page -> Display -> AppState -> ( MainModel, Cmd Msg )
loadPage page display appState =
    case page of
        Pages.Login ->
            updateModel (Login.init appState) display Login LoginMsg

        Pages.NotFound ->
            ( ( PageNotFound appState, display ), Cmd.none )

        Pages.Dashboard ->
            updateModel (Dashboard.init appState) display Dashboard DashboardMsg

        Pages.Logout ->
            ( ( Logout appState, display ), Cmd.none )


getAppState : Model -> AppState
getAppState model =
    case model of
        Login subModel ->
            Login.getAppState subModel

        PageNotFound appState ->
            appState

        Dashboard subModel ->
            Dashboard.getAppState subModel

        Logout appState ->
            appState


setAppState : Model -> AppState -> Model
setAppState model newAppState =
    case model of
        Login subModel ->
            Login (Login.setAppState subModel newAppState)

        Dashboard subModel ->
            Dashboard (Dashboard.setAppState subModel newAppState)

        PageNotFound appState ->
            PageNotFound newAppState

        Logout appState ->
            Logout newAppState


getPage : Model -> Pages.Page
getPage model =
    case model of
        Login _ ->
            Pages.Login

        Dashboard _ ->
            Pages.Dashboard

        PageNotFound _ ->
            Pages.NotFound

        Logout _ ->
            Pages.Logout



-- VIEW


getPageView : Model -> Element Msg
getPageView model =
    case model of
        Login subModel ->
            Login.view subModel
                |> Element.map LoginMsg

        Dashboard subModel ->
            Dashboard.view subModel
                |> Element.map DashboardMsg

        PageNotFound _ ->
            NotFound.view

        Logout _ ->
            Logout.view


view : MainModel -> Browser.Document Msg
view ( model, screen ) =
    { title = "Hospitality | " ++ (getPage model |> getTitle)
    , body =
        [ layout [] <|
            case screen of
                Screen ->
                    MainLayout.view
                        (NavLayout.view
                            (getPage model)
                            (AppState.getAuth (getAppState model))
                            (GlobalMsg << RedirectToPage)
                            getTitle
                        )
                        (getPageView model)

                Print ->
                    PrintLayout.view
                        (getPageView model)
        ]
    }
