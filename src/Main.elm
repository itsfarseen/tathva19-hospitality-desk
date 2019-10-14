module Main exposing (Model, Msg(..), init, main, update, view)

import Backend exposing (Participant, getParticipants)
import Browser
import Components.Main as Main_
import Dict exposing (Dict)
import Element exposing (column, layout, row, text)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List
import Pages.Login as Login


main : Program () Model Msg
main =
    Browser.application
        { init = \f url navkey -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \u -> UrlChanged
        , onUrlRequest = \l -> LinkClicked
        }



-- MODEL


type Model
    = LoginModel Login.Model


init : Model
init =
    LoginModel Login.init



-- UPDATE


type Msg
    = UrlChanged
    | LinkClicked
    | LoginMsg Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked, _ ) ->
            ( model, Cmd.none )

        ( LoginMsg subMsg, LoginModel subModel ) ->
            let
                ( subModel_, subCmd ) =
                    Login.update subModel subMsg
            in
            ( LoginModel subModel_, Cmd.map LoginMsg subCmd )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ case model of
            LoginModel subModel ->
                Html.map LoginMsg (Login.view subModel)
        ]
    }
