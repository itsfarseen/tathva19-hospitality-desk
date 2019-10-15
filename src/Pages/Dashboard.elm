module Pages.Dashboard exposing (Model, Msg, getAppState, init, setAppState, title, update, view)

import AppState exposing (AppState)
import Backend exposing (Participant)
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
    { billNo : String, addParticipant : String, participants : List ( Participant, String ), appState : AppState }


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
    Model { billNo = "", addParticipant = "", participants = [], appState = appState }


type Msg
    = BillNoChanged String
    | AddParticipantChanged String
    | CreateBill


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        BillNoChanged newBillNo ->
            ( updateModel model (\form -> { form | billNo = newBillNo }), Cmd.none )

        AddParticipantChanged participant ->
            ( updateModel model (\form -> { form | addParticipant = participant }), Cmd.none )

        CreateBill ->
            ( model, Cmd.none )


updateModel : Model -> (ModelRecord -> ModelRecord) -> Model
updateModel model updater =
    let
        record =
            toInner model
    in
    Model (updater record)


view : Model -> Element Msg
view model =
    column [ Element.paddingXY 20 0, Font.size 15, Element.spacing 20, Element.alignTop ]
        [ Element.el Theme.pageTitle (text "Dashboard")
        ]
