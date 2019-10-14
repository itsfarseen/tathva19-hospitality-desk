module Main exposing (Model, Msg(..), init, main, update, view)

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
import Pages.Login as Login
import Pages.NotFound as NotFound
import Route
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
    = LoginModel Login.Model
    | PageNotFound


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navkey =
    changeRouteTo (Route.toRoute <| Url.toString url) PageNotFound



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | LoginMsg Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.toRoute <| Url.toString url) model

        ( UrlRequested _, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg subMsg, LoginModel subModel ) ->
            let
                ( subModel_, subCmd ) =
                    Login.update subModel subMsg
            in
            ( LoginModel subModel_, Cmd.map LoginMsg subCmd )

        ( _, PageNotFound ) ->
            ( model, Cmd.none )


changeRouteTo : Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Route.Login ->
            ( LoginModel Login.init, Cmd.none )

        Route.NotFound ->
            ( PageNotFound, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ case model of
            LoginModel subModel ->
                Html.map LoginMsg (Login.view subModel)

            PageNotFound ->
                NotFound.view
        ]
    }
