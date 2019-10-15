module Main exposing (Model, Msg(..), init, main, update, view)

import AppState exposing (AppState)
import Backend exposing (Participant, getParticipants)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, column, layout, row, text)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Layout.Main as MainLayout
import Layout.Nav as NavLayout
import List
import Pages
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
                ( subModel_, subCmd ) =
                    Login.update subModel subMsg
            in
            ( Login subModel_, Cmd.map LoginMsg subCmd )

        ( PageNotFound _, _ ) ->
            ( model, Cmd.none )


changeUrlTo : Pages.Page -> Model -> ( Model, Cmd Msg )
changeUrlTo page model =
    let
        navKey =
            AppState.getNavKey (getAppState model)
    in
    ( model, Nav.pushUrl navKey (Pages.toUrl page) )


loadPage : Pages.Page -> Model -> ( Model, Cmd Msg )
loadPage page model =
    case page of
        Pages.Login ->
            ( Login (Login.init <| getAppState model), Cmd.none )

        Pages.NotFound ->
            ( PageNotFound <| getAppState model, Cmd.none )


getAppState : Model -> AppState
getAppState model =
    case model of
        Login subModel ->
            Login.getAppState subModel

        PageNotFound appState ->
            appState


setAppState : Model -> AppState -> Model
setAppState model newAppState =
    case model of
        Login subModel ->
            Login (Login.setAppState subModel newAppState)

        PageNotFound appState ->
            PageNotFound newAppState


getPage : Model -> Pages.Page
getPage model =
    case model of
        Login _ ->
            Pages.Login

        PageNotFound _ ->
            Pages.NotFound



-- VIEW


getPageView : Model -> Element Msg
getPageView model =
    case model of
        Login subModel ->
            Login.view subModel
                |> Element.map LoginMsg

        PageNotFound _ ->
            NotFound.view


view : Model -> Browser.Document Msg
view model =
    { title = "Hospitality | " ++ (getPage model |> Pages.getTitle)
    , body =
        [ layout [] <| MainLayout.view (NavLayout.view (getPage model) RedirectToPage) (getPageView model) ]
    }
