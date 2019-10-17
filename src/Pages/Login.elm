module Pages.Login exposing (Model, Msg, init, title, update, view)

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


type alias Model =
    { state : State, form : Form }


type alias Form =
    { userid : String
    , password : String
    }


type State
    = Default
    | LoggingIn
    | LogInFailed


init : ( Model, Cmd Msg, Maybe GlobalMsg )
init =
    ( { state = Default, form = { userid = "", password = "" } }
    , Cmd.none
    , Nothing
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
            ( setState model LoggingIn, Backend.login model.form loginHandler, Nothing )

        LoginSuccess token ->
            ( model, Cmd.none, Just (GlobalMsg.LogIn { token = token }) )

        LoginFailed _ ->
            ( setState model LogInFailed, Cmd.none, Nothing )


updateForm : Model -> (Form -> Form) -> Model
updateForm model updater =
    { model | form = updater model.form }


setState : Model -> State -> Model
setState model newState =
    { model | state = newState }


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
        , Theme.inputText (Theme.labelLeft "User ID") model.form.userid UserIDChanged
        , Theme.inputPassword
            (Theme.labelLeft "Password")
            model.form.password
            PasswordChanged
        , let
            disabled =
                case model.state of
                    LoggingIn ->
                        True

                    _ ->
                        False
          in
          Theme.button "Login" LoginClicked (not disabled)
        ]
