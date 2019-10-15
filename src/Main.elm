module Main exposing (Model, Msg(..), init, main, update, view)

import AppState exposing (AppState)
import Backend exposing (Participant, getParticipants)
import Browser
import Browser.Navigation as Nav
import Components.Main as Main_
import Dict exposing (Dict)
import Element exposing (column, layout, row, text)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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

        requestedRoute =
            Route.toRoute <| Url.toString url

        initialModel =
            case AppState.getAuth appState of
                AppState.LoggedIn _ ->
                    PageNotFound appState

                AppState.LoggedOut ->
                    Login (Login.init appState)
    in
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | LoginMsg Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            loadRoute (Route.toRoute <| Url.toString url) model

        ( UrlRequested _, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( subModel_, subCmd ) =
                    Login.update subModel subMsg
            in
            ( Login subModel_, Cmd.map LoginMsg subCmd )

        ( _, PageNotFound _ ) ->
            ( model, Cmd.none )


changeRouteTo : Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        navKey =
            AppState.getNavKey (getAppState model)
    in
    ( model, Route.redirectTo navKey route )


loadRoute : Route.Route -> Model -> ( Model, Cmd Msg )
loadRoute route model =
    case route of
        Route.Login ->
            ( Login (Login.init <| getAppState model), Cmd.none )

        Route.NotFound ->
            ( PageNotFound <| getAppState model, Cmd.none )


getAppState : Model -> AppState
getAppState model =
    case model of
        Login subModel ->
            Login.getAppState subModel

        PageNotFound appState ->
            appState



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hospitality Admin"
    , body =
        [ case model of
            Login subModel ->
                Login.view subModel
                    |> Html.map LoginMsg

            PageNotFound _ ->
                NotFound.view
        ]
    }
