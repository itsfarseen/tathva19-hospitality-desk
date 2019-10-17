module Pages.Dashboard exposing (Model, Msg, init, subscriptions, title, update, view)

import AppState exposing (AppState)
import Backend exposing (BedAssignment, NewBill, Participant, ParticipantEx, RemoteData(..))
import Components.Bill as BillComp
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
    , participant : RemoteData ParticipantEx (Backend.Error Backend.QueryError)
    , addParticipantError : String
    , bedNo : String
    , hostel : String
    , viewBillNo : String
    , createBillError : String
    , token : String
    }


init : String -> ( Model, Cmd Msg, Maybe GlobalMsg )
init token =
    ( { form = { billNo = "", bedAssignments = [] }
      , shortId = ""
      , participant = NotRequested
      , addParticipantError = ""
      , bedNo = ""
      , viewBillNo = ""
      , hostel = ""
      , createBillError = ""
      , token = token
      }
    , Cmd.none
    , Nothing
    )


type Msg
    = BillNoChanged String
    | ParticipantShortIdChanged String
    | BedNoChanged String
    | HostelChanged String
    | ViewBillNoChanged String
    | ViewBill
    | AddParticipant
    | CreateBill
    | BillCreated (Result (Backend.Error Backend.SaveError) ())
    | ParticipantLoad (Result (Backend.Error Backend.QueryError) ParticipantEx)
    | DeleteBedAssignment { shortId : String }


subscriptions : Model -> Sub Msg
subscriptions model =
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


viewBill : String -> Model -> ( Model, Cmd Msg, Maybe GlobalMsg )
viewBill billNo model =
    ( model, Cmd.none, Just <| GlobalMsg.RedirectToPage <| Pages.ViewBill { billNo = billNo } )


update : Model -> Msg -> ( Model, Cmd Msg, Maybe GlobalMsg )
update model msg =
    case msg of
        BillNoChanged newBillNo ->
            ( updateForm model (\form -> { form | billNo = newBillNo }), Cmd.none, Nothing )

        ParticipantShortIdChanged shortId ->
            let
                shortIdNormalized =
                    if String.startsWith "T19-" (String.toUpper shortId) then
                        String.dropLeft 4 shortId
                            |> String.toUpper

                    else
                        String.toUpper shortId

                shortIdDisplay =
                    if
                        String.length shortIdNormalized
                            < 4
                            && String.startsWith shortIdNormalized "T19-"
                    then
                        shortIdNormalized

                    else
                        "T19-" ++ shortIdNormalized

                triggerLoad =
                    String.length shortIdNormalized == 6

                model_ =
                    { model
                        | shortId = shortIdDisplay
                        , participant =
                            if triggerLoad then
                                Loading

                            else
                                NotRequested
                    }
            in
            ( model_
            , if triggerLoad then
                Backend.getParticipant model.token
                    ParticipantLoad
                    shortIdNormalized

              else
                Cmd.none
            , Nothing
            )

        ParticipantLoad result ->
            let
                ( participant, globalmsg ) =
                    case result of
                        Ok participant_ ->
                            ( Loaded participant_, Nothing )

                        Err Backend.TokenExpired ->
                            ( LoadFailed Backend.TokenExpired, Just (GlobalMsg.RedirectToPage Pages.Logout) )

                        Err err ->
                            ( LoadFailed err, Nothing )
            in
            ( { model | participant = participant }, Cmd.none, globalmsg )

        BedNoChanged bedNo ->
            ( { model | bedNo = String.toUpper bedNo }, Cmd.none, Nothing )

        HostelChanged hostel ->
            ( { model | hostel = String.toUpper hostel }, Cmd.none, Nothing )

        ViewBillNoChanged billNo ->
            ( { model | viewBillNo = billNo }, Cmd.none, Nothing )

        ViewBill ->
            ( model, Cmd.none, Just (GlobalMsg.RedirectToPage <| Pages.ViewBill { billNo = model.viewBillNo }) )

        CreateBill ->
            let
                formRes =
                    if String.length model.form.billNo == 0 then
                        Err "Please provide bill number"

                    else if List.length model.form.bedAssignments == 0 then
                        Err "Bill is empty"

                    else
                        Ok model.form
            in
            case formRes of
                Ok form ->
                    ( { model | createBillError = "" }, Backend.saveBill model.token form BillCreated, Nothing )

                Err err ->
                    ( { model | createBillError = err }, Cmd.none, Nothing )

        BillCreated result ->
            let
                displayError =
                    \mdl errmsg -> { mdl | createBillError = "Could not create bill. " ++ errmsg }
            in
            case result of
                Ok _ ->
                    ( { model
                        | form =
                            { billNo =
                                String.toInt model.form.billNo
                                    |> Maybe.map (\x -> x + 1)
                                    |> Maybe.map String.fromInt
                                    |> Maybe.withDefault ""
                            , bedAssignments = []
                            }
                        , createBillError = ""
                      }
                    , Cmd.none
                    , Just <| GlobalMsg.RedirectToPage <| Pages.ViewBill { billNo = model.form.billNo }
                    )

                Err (Backend.Error Backend.IDConflict) ->
                    ( displayError model "Bill no already used / Participant already added to another bill", Cmd.none, Nothing )

                Err error ->
                    handleCommonErrors
                        error
                        model
                        displayError

        AddParticipant ->
            let
                modelRes =
                    case model.participant of
                        Loaded participant ->
                            if
                                not participant.tathva_pass
                                    && not (Backend.hasWorkshop participant)
                            then
                                Err "Participant doesn't have Tathva Pass or Workshop Pass"

                            else if String.length model.hostel == 0 then
                                Err "Please provide hostel"

                            else if String.length model.bedNo == 0 then
                                Err "Please provide bed"

                            else
                                Ok participant

                        NotRequested ->
                            Err "No participant selected"

                        _ ->
                            Err "Participant not loaded"
            in
            case modelRes of
                Ok participantEx ->
                    ( updateForm model
                        (\form ->
                            { form
                                | bedAssignments =
                                    List.append form.bedAssignments
                                        [ { participant = Backend.toParticipant participantEx
                                          , bedno = model.hostel ++ " - " ++ model.bedNo
                                          }
                                        ]
                            }
                        )
                        |> (\modelUpdated ->
                                { modelUpdated
                                    | shortId = ""
                                    , participant = NotRequested
                                    , bedNo =
                                        String.toInt model.bedNo
                                            |> Maybe.map (\x -> x + 1)
                                            |> Maybe.map String.fromInt
                                            |> Maybe.withDefault ""
                                }
                           )
                    , Cmd.none
                    , Nothing
                    )

                Err err ->
                    ( { model | addParticipantError = err }, Cmd.none, Nothing )

        DeleteBedAssignment { shortId } ->
            ( updateForm model
                (\form ->
                    { form
                        | bedAssignments =
                            List.filter
                                (\{ participant, bedno } -> participant.shortId /= shortId)
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
                [ viewViewBillCard model ]
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
            , BillComp.newBill model.form DeleteBedAssignment
            , row
                (Theme.pageSubCardAttrs
                    ++ [ Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                       , Element.padding 20
                       ]
                )
                [ Theme.title2 model.createBillError
                , Theme.button
                    "Create Bill"
                    CreateBill
                    True
                ]
            ]


viewAddParticipant : Model -> Element Msg
viewAddParticipant model =
    column (Theme.pageSubCardAttrs ++ [ Border.widthXY 0 1, Element.spacing 20 ])
        [ Theme.title2 "Add Participant"
        , wrappedRow
            [ Element.spacing 40, Element.width Element.fill ]
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
                , wrappedRow [ Element.width Element.fill ]
                    [ Theme.title2 model.addParticipantError
                    , Element.el [ Element.alignRight ]
                        (Theme.button
                            "Add"
                            AddParticipant
                            True
                        )
                    ]
                ]
            , Element.el [ Element.width Element.fill, Element.alignTop ] (viewSelectedParticipant model)
            ]
        ]


viewSelectedParticipant : Model -> Element Msg
viewSelectedParticipant model =
    case ( model.participant, model.shortId ) of
        ( Loaded participant, _ ) ->
            column [ Element.width Element.fill, Element.spacing 10 ]
                [ Theme.title1 ("T19-" ++ participant.shortId)
                , Theme.title2 ("Name: " ++ participant.name)
                , Theme.title2 ("College: " ++ participant.college)
                , Theme.title2 ("Mobile: " ++ participant.mobile)
                , Theme.title2 ("Email: " ++ participant.email)
                , Theme.title2
                    ("Tathva Pass: "
                        ++ (if participant.tathva_pass then
                                "YES"

                            else
                                "NO"
                           )
                    )
                , Theme.title2
                    ("Workshops: "
                        ++ (if Backend.hasTwoDayWorkshop participant then
                                "Two day"

                            else if Backend.hasWorkshop participant then
                                "One day"

                            else
                                "No"
                           )
                    )
                ]

        ( _, "" ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Please enter a tathva id"
                ]

        ( NotRequested, _ ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Please enter a tathva id"
                ]

        ( Loading, _ ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Loading"
                ]

        ( LoadFailed (Backend.Error Backend.NotFound), _ ) ->
            column [ Element.centerX ]
                [ Theme.title2 "Tathva ID not found"
                ]

        ( LoadFailed error, _ ) ->
            column []
                [ Theme.title2 (commonErrorMessages error)
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
