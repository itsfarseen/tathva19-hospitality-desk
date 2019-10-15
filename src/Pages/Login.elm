module Pages.Login exposing (Model, Msg, getAppState, init, update, view)

import AppState exposing (AppState)
import Backend
import Element exposing (column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import Html
import Theme


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


type Msg
    = UserIDChanged String
    | PasswordChanged String
    | LoginClicked
    | LoginSuccess Backend.Token
    | LoginFailed


init : AppState -> Model
init appState =
    Model { state = Default, form = Form "" "", appState = appState }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        UserIDChanged userid ->
            ( updateForm model (\form -> { form | userid = userid }), Cmd.none )

        PasswordChanged password ->
            ( updateForm model (\form -> { form | password = password }), Cmd.none )

        LoginClicked ->
            ( model, Backend.login (getForm model) loginHandler )

        LoginSuccess ->
            ( model, Cmd.none )

        LoginFailed ->
            ( updateState model LogInFailed, Cmd.none )


updateForm : Model -> (Form -> Form) -> Model
updateForm model updater =
    let
        record =
            toInner model
    in
    Model { record | form = updater record.form }


updateState : Model -> State -> Model
updateState model newState =
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
            LoginSuccess

        _ ->
            LoginFailed


view : Model -> Html.Html Msg
view model =
    layout [] <|
        column [ Element.width (Element.px 400), Element.paddingXY 20 0, Element.centerX, Element.centerY, Font.size 15, Element.spacing 20 ]
            [ Element.el (Theme.pageTitle ++ [ Element.moveUp 20.0 ]) (text "Hospitality Login")
            , Input.text Theme.input
                { label = Input.labelAbove [] (text "User ID")
                , onChange = UserIDChanged
                , placeholder = Just (Input.placeholder [] (text "UserID"))
                , text = (getForm model).userid
                }
            , Input.text Theme.input
                { label = Input.labelAbove [] (text "Password")
                , onChange = PasswordChanged
                , placeholder = Just (Input.placeholder [] (text "Password"))
                , text = (getForm model).password
                }
            , Input.button
                (Theme.button
                    ++ [ Element.alignRight ]
                )
                { label = Element.el [ Element.centerX ] (text "Login")
                , onPress = Just LoginClicked
                }
            ]
