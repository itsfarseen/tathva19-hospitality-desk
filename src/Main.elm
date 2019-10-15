module Main exposing (Model, Msg(..), init, main, update, view)

import AppState exposing (AppState)
import Backend exposing (Participant, getParticipants)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, column, layout, row, text)
import Element.Input as Input
import GlobalMsg exposing (GlobalMsg)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Layout.Main as MainLayout
import Layout.Nav as NavLayout
import List
import Pages
import Pages.Dashboard as Dashboard
import Pages.Login as Login
import Pages.NotFound as NotFound
import Url


main : Program () Model Msg
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
    | Dashboard Dashboard.Model


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navkey =
    let
        appState =
            AppState.init navkey

        requestedPage =
            Pages.fromUrl <| Url.toString url

        initialModel =
            case AppState.getAuth appState of
                AppState.LoggedIn _ ->
                    PageNotFound appState

                AppState.LoggedOut ->
                    Login (Login.init appState)
    in
    loadPage requestedPage initialModel



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | LoginMsg Login.Msg
    | DashboardMsg Dashboard.Msg
    | RedirectToPage Pages.Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, UrlChanged url ) ->
            loadPage (Pages.fromUrl <| Url.toString url) model

        ( _, UrlRequested _ ) ->
            ( model, Cmd.none )

        ( _, RedirectToPage page ) ->
            changeUrlTo page model

        ( Login subModel, LoginMsg subMsg ) ->
            let
                ( updatedSubModel, maybeGlobalMsg, subCmd ) =
                    Login.update subModel subMsg

                ( updatedModel, cmd ) =
                    case maybeGlobalMsg of
                        Just globalMsg ->
                            handleGlobalMsg globalMsg (Login updatedSubModel)

                        Nothing ->
                            ( Login updatedSubModel, Cmd.none )
            in
            ( updatedModel, Cmd.batch [ Cmd.map LoginMsg subCmd, cmd ] )

        ( Login _, _ ) ->
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


handleGlobalMsg : GlobalMsg -> Model -> ( Model, Cmd Msg )
handleGlobalMsg globalMsg model =
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


changeUrlTo : Pages.Page -> Model -> ( Model, Cmd Msg )
changeUrlTo page model =
    let
        navKey =
            AppState.getNavKey (getAppState model)
    in
    ( model, Nav.pushUrl navKey (Pages.toUrl page) )


loadPage : Pages.Page -> Model -> ( Model, Cmd Msg )
loadPage page model =
    let
        -- Auth Guard: Show not found if page is not allowed in current auth state
        pageFiltered =
            if List.member page (Pages.allowedPages (AppState.getAuth (getAppState model))) then
                page

            else
                Pages.NotFound
    in
    case pageFiltered of
        Pages.Login ->
            ( Login (Login.init <| getAppState model), Cmd.none )

        Pages.NotFound ->
            ( PageNotFound <| getAppState model, Cmd.none )

        Pages.Dashboard ->
            ( Dashboard (Dashboard.init <| getAppState model), Cmd.none )


getAppState : Model -> AppState
getAppState model =
    case model of
        Login subModel ->
            Login.getAppState subModel

        PageNotFound appState ->
            appState

        Dashboard subModel ->
            Dashboard.getAppState subModel


setAppState : Model -> AppState -> Model
setAppState model newAppState =
    case model of
        Login subModel ->
            Login (Login.setAppState subModel newAppState)

        Dashboard subModel ->
            Dashboard (Dashboard.setAppState subModel newAppState)

        PageNotFound appState ->
            PageNotFound newAppState


getPage : Model -> Pages.Page
getPage model =
    case model of
        Login _ ->
            Pages.Login

        Dashboard _ ->
            Pages.Dashboard

        PageNotFound _ ->
            Pages.NotFound



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


view : Model -> Browser.Document Msg
view model =
    { title = "Hospitality | " ++ (getPage model |> getTitle)
    , body =
        [ layout [] <| MainLayout.view (NavLayout.view (getPage model) RedirectToPage getTitle) (getPageView model) ]
    }
