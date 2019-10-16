module Pages.Login exposing (Model, Msg, getAppState, init, setAppState, title, update, view)

import AppState exposing (AppState)
import Backend exposing (Error, LoginCreds, LoginError, Token)
import Element exposing (Element, column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import GlobalMsg exposing (GlobalMsg)
import Html
import Pages
import Theme


title =
    "Login"


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


init : AppState -> ( Model, Cmd Msg, Maybe GlobalMsg )
init appState =
    ( Model { state = Default, form = Form "" "", appState = appState }
    , Cmd.none
    , case AppState.getAuth appState of
        AppState.LoggedIn _ ->
            Just (GlobalMsg.RedirectToPage Pages.Dashboard)

        _ ->
            Nothing
    )


type Msg
    = UserIDChanged String
    | PasswordChanged String
    | LoginClicked
    | LoginSuccess Backend.Token
    | LoginFailed (Error LoginError)


update : Model -> Msg -> ( Model, Cmd Msg, Maybe GlobalMsg )
update model msg =
    case msg of
        UserIDChanged userid ->
            ( updateForm model (\form -> { form | userid = userid }), Cmd.none, Nothing )

        PasswordChanged password ->
            ( updateForm model (\form -> { form | password = password }), Cmd.none, Nothing )

        LoginClicked ->
            ( setState model LoggingIn, Backend.login (getForm model) loginHandler, Nothing )

        LoginSuccess token ->
            let
                ( newAppState, appStateCmd ) =
                    AppState.setAuth (getAppState model) (AppState.LoggedIn token)

                newModel =
                    setAppState model newAppState
            in
            ( newModel, Cmd.batch [ appStateCmd ], Just (GlobalMsg.RedirectToPage Pages.Dashboard) )

        LoginFailed _ ->
            ( setState model LogInFailed, Cmd.none, Nothing )


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


loginHandler : Result (Error LoginError) Token -> Msg
loginHandler result =
    case result of
        Ok token ->
            LoginSuccess token

        Err error ->
            LoginFailed error


view : Model -> Element Msg
view model =
    column
        [ Element.width (Element.px 400), Element.paddingXY 20 0, Element.centerX, Element.centerY, Font.size 15, Element.spacing 20 ]
        [ Theme.title "Login"
        , Theme.inputText (Theme.labelLeft "User ID") (getForm model).userid UserIDChanged
        , Theme.inputPassword
            (Theme.labelLeft "Password")
            (getForm model).password
            PasswordChanged
        , let
            disabled =
                case (toInner model).state of
                    LoggingIn ->
                        True

                    _ ->
                        False
          in
          Theme.button "Login" LoginClicked (not disabled)
        ]
