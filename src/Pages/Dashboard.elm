module Pages.Dashboard exposing (Model, Msg, getAppState, init, setAppState, title, update, view)

import AppState exposing (AppState)
import Backend
import Element exposing (Element, column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import Html
import Theme


title =
    "Dashboard"


type Model
    = Model ModelRecord -- As an opaque type


type alias ModelRecord =
    { state : State, form : Form, appState : AppState }


type alias Form =
    { userid : String
    , password : String
    }


type State
    = Default
    | LoggingIn
    | LogInFailed


toInner : Model -> ModelRecord
toInner model =
    case model of
        Model record ->
            record


getAppState : Model -> AppState
getAppState model =
    .appState <| toInner model


setAppState : Model -> AppState -> Model
setAppState model newAppState =
    let
        record =
            toInner model
    in
    Model { record | appState = newAppState }


init : AppState -> Model
init appState =
    Model { state = Default, form = Form "" "", appState = appState }


type Msg
    = UserIDChanged String
    | PasswordChanged String
    | LoginClicked
    | LoginSuccess Backend.Token
    | LoginFailed


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        UserIDChanged userid ->
            ( updateForm model (\form -> { form | userid = userid }), Cmd.none )

        PasswordChanged password ->
            ( updateForm model (\form -> { form | password = password }), Cmd.none )

        LoginClicked ->
            ( model, Backend.login (getForm model) loginHandler )

        LoginSuccess token ->
            AppState.setAuth (getAppState model) (AppState.LoggedIn token)
                |> Tuple.mapFirst (setAppState model)

        LoginFailed ->
            ( setState model LogInFailed, Cmd.none )


updateForm : Model -> (Form -> Form) -> Model
updateForm model updater =
    let
        record =
            toInner model
    in
    Model { record | form = updater record.form }


setState : Model -> State -> Model
setState model newState =
    let
        record =
            toInner model
    in
    Model { record | state = newState }


getForm : Model -> Form
getForm model =
    .form <| toInner model


loginHandler : Result Backend.Error (Maybe Backend.Token) -> Msg
loginHandler result =
    case result of
        Ok (Just token) ->
            LoginSuccess token

        _ ->
            LoginFailed


view : Model -> Element Msg
view model =
    column [ Element.width (Element.px 400), Element.paddingXY 20 0, Element.centerX, Element.centerY, Font.size 15, Element.spacing 20 ]
        [ Element.el (Theme.pageTitle ++ [ Element.moveUp 20.0 ]) (text "Dashboard")
        ]
