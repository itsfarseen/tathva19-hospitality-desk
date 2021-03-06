module Pages.PrintBill exposing (Model, Msg, getBillNo, init, title, update, view)

import AppState exposing (AppState, Auth(..))
import Backend exposing (Bill, Error, QueryError, RemoteData(..), Token, getBill)
import Components.Bill as BillComp
import Element exposing (Element, column, layout, row, text)
import Element.Background
import Element.Font as Font
import Element.Input as Input
import GlobalMsg exposing (GlobalMsg)
import Html.Attributes
import Pages
import Pages.Map as Map
import Pages.Rules as Rules
import Theme


title billNo =
    "Bill #" ++ billNo


getBillNo : Model -> String
getBillNo model =
    model.billNo


type alias Model =
    { billNo : String, bill : RemoteData Bill (Error QueryError) }


init : Token -> String -> ( Model, Cmd Msg, Maybe GlobalMsg )
init token billNo =
    ( { billNo = billNo, bill = Loading }
    , Backend.getBill token billNo GotBillResponse
    , Nothing
    )


type Msg
    = GotBillResponse (Result (Error QueryError) Bill)
    | PrintModeClicked


update : Model -> Msg -> ( Model, Cmd Msg, Maybe GlobalMsg )
update model msg =
    case msg of
        GotBillResponse resp ->
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

        PrintModeClicked ->
            ( model, Cmd.none, Just GlobalMsg.EnterPrintMode )


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
    column [ Element.paddingXY 20 20, Font.size 15, Element.spacing 10, Element.alignTop, Element.width Element.fill ]
        (case model.bill of
            Loaded bill ->
                [ Element.column [ Element.width Element.fill, Element.spacing 20 ]
                    [ BillComp.savedBill bill
                    , Theme.title2 "Map and Rules & Regulations at attached below"
                    ]
                , fullHeight Rules.view
                , fullHeight Map.view
                ]

            LoadFailed err ->
                [ Theme.title (commonErrorMessages err) ]

            Loading ->
                [ Theme.title "Loading" ]

            NotRequested ->
                [ Theme.title "Not Loaded" ]
        )


fullHeight el =
    Element.el [ Element.htmlAttribute (Html.Attributes.style "min-height" "97%"), Element.height Element.fill ] el
