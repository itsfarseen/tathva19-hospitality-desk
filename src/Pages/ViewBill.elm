module Pages.ViewBill exposing (Model, Msg, getBillNo, init, title, update, view)

import AppState exposing (AppState, Auth(..))
import Backend exposing (Bill, Error, QueryError, RemoteData(..), Token, getBill)
import Components.Bill as BillComp
import Element exposing (Element, column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import GlobalMsg exposing (GlobalMsg)
import Html
import Pages
import Theme


title billNo =
    "Bill #" ++ billNo


getBillNo : Model -> String
getBillNo model =
    model.billNo


type alias Model =
    { billNo : String, token : String, bill : RemoteData Bill (Error QueryError) }


init : Token -> String -> ( Model, Cmd Msg, Maybe GlobalMsg )
init token billNo =
    ( { billNo = billNo, token = token, bill = Loading }
    , Backend.getBill token billNo GotBillResponse
    , Nothing
    )


type Msg
    = GotBillResponse (Result (Error QueryError) Bill)
    | DeregResponse (Result (Error QueryError) ())
    | DeregClicked
    | PrintClicked
    | MailClicked


getFirstMailId : Bill -> Maybe String
getFirstMailId bill =
    bill.bedAssignments
        |> List.filterMap
            (\{ participant } ->
                if participant.email /= "" then
                    Just participant.email

                else
                    Nothing
            )
        |> List.head


update : Model -> Msg -> ( Model, Cmd Msg, Maybe GlobalMsg )
update model msg =
    case ( model.bill, msg ) of
        ( _, GotBillResponse resp ) ->
            ( { model
                | bill =
                    case resp of
                        Err e ->
                            LoadFailed e

                        Ok bill ->
                            Loaded bill
              }
            , Cmd.none
            , Nothing
            )

        ( _, DeregResponse resp ) ->
            ( model, Backend.getBill model.token model.billNo GotBillResponse, Nothing )

        ( _, PrintClicked ) ->
            ( model, Cmd.none, Nothing )

        ( Loaded bill, DeregClicked ) ->
            ( model
            , Backend.deregBill model.token
                model.billNo
                (not bill.dereg)
                DeregResponse
            , Nothing
            )

        ( _, DeregClicked ) ->
            ( model, Cmd.none, Nothing )

        ( _, MailClicked ) ->
            ( model, Cmd.none, Nothing )


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


view : Model -> Element Msg
view model =
    column [ Element.paddingXY 20 20, Font.size 15, Element.spacing 40, Element.alignTop, Element.width (Element.fill |> Element.maximum 600) ]
        (case model.bill of
            Loaded bill ->
                [ row [ Element.spacing 20 ]
                    [ Theme.button
                        (if bill.dereg then
                            "UNDO DE-REG"

                         else
                            "DE-REG"
                        )
                        DeregClicked
                        True
                    , Element.newTabLink []
                        { url = "/bill/" ++ model.billNo ++ "/print"
                        , label =
                            Theme.button
                                "Print"
                                PrintClicked
                                True
                        }
                    , let
                        mailId_ =
                            getFirstMailId bill
                      in
                      case mailId_ of
                        Just mailId ->
                            Element.newTabLink []
                                { url = "mailto:" ++ mailId ++ "?subject=\"Tathva'19 Hospitality Bill\""
                                , label =
                                    Theme.button
                                        ("Mail: "
                                            ++ mailId
                                        )
                                        MailClicked
                                        True
                                }

                        Nothing ->
                            Theme.button
                                "No Mail id"
                                MailClicked
                                False
                    ]
                , BillComp.savedBill
                    bill
                ]

            LoadFailed err ->
                [ Theme.title (commonErrorMessages err) ]

            Loading ->
                [ Theme.title "Loading" ]

            NotRequested ->
                [ Theme.title "Not Loaded" ]
        )
