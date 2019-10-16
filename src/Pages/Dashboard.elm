module Pages.Dashboard exposing (Model, Msg, getAppState, init, setAppState, subscriptions, title, update, view)

import AppState exposing (AppState)
import Backend exposing (BedAssignment, NewBill, Participant, RemoteData(..))
import Element exposing (Element, column, layout, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GlobalMsg exposing (GlobalMsg)
import Html
import Http
import Pages
import Task
import Theme
import Time


title =
    "Dashboard"


type alias Model =
    { form : NewBill
    , shortId : String
    , participant : Maybe Participant
    , bedNo : String
    , hostel : String
    , viewBillNo : String
    , participantsList : Maybe (List Participant)
    , participantsListUpdated : RemoteData ( Time.Posix, Time.Zone ) (Backend.Error ())
    , participantsListProgress : Maybe Http.Progress
    , appState : AppState
    , createBillError : Maybe String
    }


getAppState : Model -> AppState
getAppState model =
    .appState model


setAppState : Model -> AppState -> Model
setAppState model newAppState =
    { model | appState = newAppState }


init : AppState -> ( Model, Cmd Msg, Maybe GlobalMsg )
init appState =
    ( { form = { billNo = "", bedAssignments = [] }
      , shortId = ""
      , participant = Nothing
      , bedNo = ""
      , viewBillNo = ""
      , hostel = ""
      , participantsList = Nothing
      , participantsListUpdated = NotRequested
      , participantsListProgress = Nothing
      , createBillError = Nothing
      , appState = appState
      }
    , Cmd.none
    , case AppState.getAuth appState of
        AppState.LoggedOut ->
            Just (GlobalMsg.RedirectToPage Pages.Login)

        _ ->
            Nothing
    )


type Msg
    = BillNoChanged String
    | ParticipantShortIdChanged String
    | BedNoChanged String
    | HostelChanged String
    | ViewBillNoChanged String
    | ViewBill
    | RefreshParticipantsList
    | AddParticipant
    | CreateBill
    | BillCreated (Result (Backend.Error Backend.SaveError) ())
    | ParticipantLoad (Result (Backend.Error Backend.QueryError) ParticipantEx)
    | ParticipantsListLoad (Result (Backend.Error ()) (List Participant))
    | ParticipantsListUpdateTime ( Time.Posix, Time.Zone )
    | ParticipantsListProgress Http.Progress
    | DeleteBedAssignment String


loadParticipantsList : Model -> ( Model, Cmd Msg, Maybe GlobalMsg )
loadParticipantsList model =
    case AppState.getAuth model.appState of
        AppState.LoggedIn token ->
            ( { model | participantsListUpdated = Loading }
            , Backend.getParticipants
                token
                ParticipantsListLoad
                (Just "participants")
            , Nothing
            )

        AppState.LoggedOut ->
            ( model, Cmd.none, Nothing )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.participantsListUpdated of
        Backend.Loading ->
            Http.track "participants" ParticipantsListProgress

        _ ->
            Sub.none


commonErrorMessages : Backend.Error err -> String
commonErrorMessages error =
    case error of
        Backend.HttpError (Backend.CodeError str) ->
            "Internal Error: " ++ str

        Backend.HttpError (Backend.BadStatus { statusCode, body }) ->
            "Unexpected Response (" ++ statusCode ++ "): " ++ body

        Backend.HttpError Backend.NetworkError ->
            "Network Error"

        Backend.Error _ ->
            "Unknown Error"

        Backend.TokenExpired ->
            "Login Expired"


handleCommonErrors : Backend.Error err -> Model -> (Model -> String -> Model) -> ( Model, Cmd Msg, Maybe GlobalMsg )
handleCommonErrors error model displayError =
    ( displayError model (commonErrorMessages error)
    , Cmd.none
    , if error == Backend.TokenExpired then
        Just (GlobalMsg.RedirectToPage Pages.Logout)

      else
        Nothing
    )


findParticipantByShortId : String -> List Participant -> Maybe Participant
findParticipantByShortId shortid participants =
    List.filter
        (\p ->
            (let
                shortid_ =
                    String.toUpper shortid
             in
             if String.startsWith "T19-" shortid_ then
                String.dropLeft 4 shortid_

             else
                shortid_
            )
                |> (==) (String.toUpper p.shortid)
        )
        participants
        |> List.head


update : Model -> Msg -> ( Model, Cmd Msg, Maybe GlobalMsg )
update model msg =
    case msg of
        BillNoChanged newBillNo ->
            ( updateForm model (\form -> { form | billNo = newBillNo }), Cmd.none, Nothing )

        ParticipantShortIdChanged shortId ->
            let
                participant =
                    model.participantsList
                        |> Maybe.andThen (findParticipantByShortId shortId)
            in
            ( { model
                | shortId =
                    participant
                        |> Maybe.map (\p -> "T19-" ++ p.shortid)
                        |> Maybe.withDefault shortId
                , participant = participant
              }
            , Cmd.none
            , Nothing
            )

        BedNoChanged bedNo ->
            ( { model | bedNo = String.toUpper bedNo }, Cmd.none, Nothing )

        HostelChanged hostel ->
            ( { model | hostel = String.toUpper hostel }, Cmd.none, Nothing )

        ViewBillNoChanged billNo ->
            ( { model | viewBillNo = billNo }, Cmd.none, Nothing )

        RefreshParticipantsList ->
            loadParticipantsList model

        ViewBill ->
            ( model, Cmd.none, Nothing )

        CreateBill ->
            case AppState.getAuth model.appState of
                AppState.LoggedIn token ->
                    ( model, Backend.saveBill token model.form BillCreated, Nothing )

                AppState.LoggedOut ->
                    ( model, Cmd.none, Nothing )

        BillCreated result ->
            let
                displayError =
                    \mdl errmsg -> { mdl | createBillError = Just ("Could not create bill. " ++ errmsg) }
            in
            case result of
                Ok _ ->
                    ( model, Cmd.none, Nothing )

                Err (Backend.Error Backend.IDConflict) ->
                    ( displayError model "Duplicate bill no", Cmd.none, Nothing )

                Err error ->
                    handleCommonErrors
                        error
                        model
                        displayError

        AddParticipant ->
            case model.participant of
                Just participant ->
                    ( updateForm model
                        (\form ->
                            { form
                                | bedAssignments = List.append form.bedAssignments [ ( participant, model.hostel ++ " - " ++ model.bedNo ) ]
                            }
                        )
                    , Cmd.none
                    , Nothing
                    )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        ParticipantsListLoad (Ok list) ->
            ( { model | participantsList = Just list, participantsListProgress = Nothing }
            , Time.now
                |> Task.andThen
                    (\time ->
                        Time.here
                            |> Task.map
                                (\zone -> ( time, zone ))
                    )
                |> Task.perform ParticipantsListUpdateTime
            , Nothing
            )

        ParticipantsListUpdateTime ( time, zone ) ->
            ( { model | participantsListUpdated = Loaded ( time, zone ) }, Cmd.none, Nothing )

        ParticipantsListLoad (Err Backend.TokenExpired) ->
            ( { model | participantsListUpdated = Backend.LoadFailed Backend.TokenExpired }, Cmd.none, Just (GlobalMsg.RedirectToPage Pages.Logout) )

        ParticipantsListLoad (Err err) ->
            ( { model | participantsListUpdated = Backend.LoadFailed err }, Cmd.none, Nothing )

        ParticipantsListProgress progress ->
            ( { model | participantsListProgress = Just progress }, Cmd.none, Nothing )

        DeleteBedAssignment shortid ->
            ( updateForm model
                (\form ->
                    { form
                        | bedAssignments =
                            List.filter
                                (\( p, b ) -> p.shortid /= shortid)
                                form.bedAssignments
                    }
                )
            , Cmd.none
            , Nothing
            )


updateForm : Model -> (NewBill -> NewBill) -> Model
updateForm model updater =
    { model | form = updater model.form }


view : Model -> Element Msg
view model =
    column [ Element.paddingXY 20 20, Font.size 15, Element.spacing 40, Element.alignTop, Element.width Element.fill ]
        [ Theme.title "Dashboard"
        , Element.wrappedRow
            [ Element.width Element.fill, Element.spacing 40 ]
            [ viewNewBill model
            , column
                [ Element.width Element.fill, Element.spacing 40, Element.alignTop ]
                [ viewViewBillCard model, viewParticipantsListCard model ]
            ]
        ]


viewNewBill : Model -> Element Msg
viewNewBill model =
    Element.el [ Element.width (Element.fill |> Element.maximum 600), Element.alignTop ] <|
        column Theme.pageCardAttrs
            [ Theme.pageCardTitleBar "New Bill"
                (Theme.inputText (Theme.labelLeft "Bill No")
                    model.form.billNo
                    BillNoChanged
                )
            , viewAddParticipant model
            , column [ Element.padding 20, Element.spacing 20, Element.width Element.fill ]
                [ Theme.title2 "PARTICIPANTS ADDED"
                , column [ Element.spacing 10, Element.width Element.fill ] (List.map viewNewBillItem model.form.bedAssignments)
                ]
            , row
                (Theme.pageSubCardAttrs
                    ++ [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                       , Element.padding 20
                       ]
                )
                [ Theme.button "Create Bill" CreateBill (not (List.isEmpty model.form.bedAssignments))
                ]
            ]


viewNewBillItem : BedAssignment -> Element Msg
viewNewBillItem ( participant, bed ) =
    row
        (Theme.pageSubCardAttrs
            ++ [ Element.width Element.fill, Element.spaceEvenly ]
        )
        [ column [ Element.spacing 5, Element.width Element.fill ]
            [ Theme.title2 participant.shortid
            , Theme.title2 participant.name
            , Theme.title2 participant.college
            ]
        , Element.el [ Element.width Element.fill ] (Theme.title2 participant.mobile)
        , Element.el [ Element.width Element.fill ] (Theme.title2 bed)
        , Theme.button "Delete" (DeleteBedAssignment participant.shortid) True
        ]


viewAddParticipant : Model -> Element Msg
viewAddParticipant model =
    column (Theme.pageSubCardAttrs ++ [ Border.widthXY 0 1, Element.spacing 20 ])
        [ Theme.title2 "Add Participant"
        , wrappedRow
            [ Element.spacing 20, Element.width Element.fill ]
            [ column [ Element.width (Element.fill |> Element.minimum 200), Element.spacing 10 ]
                [ Theme.inputText
                    (Theme.labelLeft "Tathva ID")
                    model.shortId
                    ParticipantShortIdChanged
                , Theme.inputText
                    (Theme.labelLeft "Hostel")
                    model.hostel
                    HostelChanged
                , Theme.inputText
                    (Theme.labelLeft "Bed")
                    model.bedNo
                    BedNoChanged
                , Theme.button "Add"
                    AddParticipant
                    (String.length model.bedNo > 0 && String.length model.hostel > 0 && model.participant /= Nothing)
                ]
            , Element.el [ Element.width Element.fill, Element.alignTop ] (viewSelectedParticipant model)
            ]
        ]


viewSelectedParticipant : Model -> Element Msg
viewSelectedParticipant model =
    case ( model.participant, model.shortId ) of
        ( Just participant, _ ) ->
            column [ Element.width Element.fill ]
                [ Theme.title2 participant.shortid
                , Theme.title2 participant.name
                , Theme.title2 participant.college
                , Theme.title2 participant.mobile
                , Theme.title2 participant.email
                ]

        ( Nothing, "" ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Please enter a tathva id"
                ]

        ( Nothing, _ ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Tathva ID not found"
                ]


viewViewBillCard : Model -> Element Msg
viewViewBillCard model =
    Element.el [ Element.width (Element.fill |> Element.maximum 400) ] <|
        column Theme.pageCardAttrs
            [ Theme.pageCardTitleBar
                "View Bill"
                (Theme.title2 "For checking/reprint/dereg")
            , wrappedRow
                (Theme.pageSubCardAttrs ++ [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }, Element.spacing 20 ])
                [ Theme.inputText
                    (Theme.labelLeft "Bill No")
                    model.viewBillNo
                    ViewBillNoChanged
                , Theme.button "Search" ViewBill True
                ]
            ]


viewParticipantsListCard : Model -> Element Msg
viewParticipantsListCard model =
    Element.el [ Element.width (Element.fill |> Element.maximum 400) ] <|
        column Theme.pageCardAttrs
            [ Theme.pageCardTitleBar
                "Participants List"
                Element.none
            , wrappedRow
                (Theme.pageSubCardAttrs ++ [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }, Element.spacing 20 ])
                [ column []
                    ([ Theme.title2
                        ("Last Updated: " ++ displayUpdateTime model.participantsListUpdated)
                     ]
                        ++ (case model.participantsListProgress of
                                Just progress ->
                                    [ Theme.title2 ("Progress: " ++ Backend.progressToString progress) ]

                                Nothing ->
                                    []
                           )
                    )
                , Theme.button "Refresh" RefreshParticipantsList True
                ]
            ]


displayUpdateTime : RemoteData ( Time.Posix, Time.Zone ) (Backend.Error ()) -> String
displayUpdateTime updateTime =
    case updateTime of
        Backend.Loaded ( time, zone ) ->
            String.fromInt (Time.toHour zone time)
                ++ ":"
                ++ String.fromInt (Time.toMinute zone time)
                ++ ":"
                ++ String.fromInt (Time.toSecond zone time)

        Backend.Loading ->
            "Refreshing"

        Backend.NotRequested ->
            "Never"

        Backend.LoadFailed err ->
            "Refreshing Failed - " ++ commonErrorMessages err
