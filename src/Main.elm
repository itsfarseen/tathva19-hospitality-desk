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
import List
import Pages
import Pages.Dashboard as Dashboard
import Pages.Login as Login
import Pages.Logout as Logout
import Pages.NotFound as NotFound
import Url


main : Program (Maybe String) Model Msg
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


type Model
    = Login Login.Model
    | PageNotFound AppState
    | Logout AppState
    | Dashboard Dashboard.Model


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navkey =
    let
        appState =
            AppState.init flags navkey

        requestedPage =
            Pages.fromUrl <| Url.toString url
    in
    loadPage requestedPage appState



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


updateModel : PageUpdate subModel subMsg -> (subModel -> Model) -> (subMsg -> Msg) -> ( Model, Cmd Msg )
updateModel ( subModel, subCmd, maybeGlobalMsg ) toModel toMsg =
    -- FIXME: This function is poorly named
    let
        ( model, cmd ) =
            case maybeGlobalMsg of
                Just globalMsg ->
                    update (GlobalMsg globalMsg) (toModel subModel)

                Nothing ->
                    ( toModel subModel, Cmd.none )
    in
    ( model, Cmd.batch [ Cmd.map toMsg subCmd, cmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, UrlChanged url ) ->
            loadPage (Pages.fromUrl <| Url.toString url) (getAppState model)

        ( _, UrlRequested _ ) ->
            ( model, Cmd.none )

        ( _, GlobalMsg (RedirectToPage Pages.Logout) ) ->
            let
                ( newAppState, cmd1 ) =
                    AppState.setAuth (getAppState model) AppState.LoggedOut

                ( newModel, cmd2 ) =
                    update (GlobalMsg (RedirectToPage Pages.Login)) (setAppState model newAppState)
            in
            ( newModel, Cmd.batch [ cmd1, cmd2 ] )

        ( _, GlobalMsg (RedirectToPage page) ) ->
            let
                -- Load page first, then changeUrl to avoid flickering
                -- when reloading from init functions
                ( newModel1, cmd1 ) =
                    loadPage page (getAppState model)

                ( newModel2, cmd2 ) =
                    changeUrlTo page newModel1
            in
            ( newModel2, Cmd.batch [ cmd1, cmd2 ] )

        ( _, ParticipantsDataResult (Ok participants) ) ->
            let
                ( appState, cmd ) =
                    AppState.setParticipantsList (getAppState model) participants
            in
            ( setAppState model appState, cmd )

        ( Login subModel, LoginMsg subMsg ) ->
            updateModel (Login.update subModel subMsg) Login LoginMsg

        ( Login _, _ ) ->
            ( model, Cmd.none )

        ( Logout _, _ ) ->
            ( model, Cmd.none )

        ( Dashboard subModel, DashboardMsg subMsg ) ->
            let
                ( subModel_, subCmd ) =
                    Dashboard.update subModel subMsg
            in
            ( Dashboard subModel_, Cmd.map DashboardMsg subCmd )

        ( Dashboard _, _ ) ->
            ( model, Cmd.none )

        ( PageNotFound _, _ ) ->
            ( model, Cmd.none )


updateGlobalMsg : GlobalMsg -> Model -> ( Model, Cmd Msg )
updateGlobalMsg globalMsg model =
    case globalMsg of
        GlobalMsg.RedirectToPage page ->
            changeUrlTo page model


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


loadPage : Pages.Page -> AppState -> ( Model, Cmd Msg )
loadPage page appState =
    case page of
        Pages.Login ->
            updateModel (Login.init appState) Login LoginMsg

        Pages.NotFound ->
            ( PageNotFound appState, Cmd.none )

        Pages.Dashboard ->
            updateModel (Dashboard.init appState) Dashboard DashboardMsg

        Pages.Logout ->
            ( Logout appState, Cmd.none )


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


view : Model -> Browser.Document Msg
view model =
    { title = "Hospitality | " ++ (getPage model |> getTitle)
    , body =
        [ layout [] <|
            MainLayout.view
                (NavLayout.view
                    (getPage model)
                    (AppState.getAuth (getAppState model))
                    (GlobalMsg << RedirectToPage)
                    getTitle
                )
                (getPageView model)
        ]
    }
