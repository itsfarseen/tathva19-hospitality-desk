module Backend exposing
    ( Error(..)
    , LoginCreds
    , LoginError(..)
    , Participant
    , ParticipantID
    , QueryError(..)
    , SaveError(..)
    , Token
    , getBill
    , getParticipants
    , login
    , saveBill
    )

import Http
import Json.Decode as D
import Json.Encode as E
import List
import Process
import Task


serverUrl =
    "https://staging.tathva.org/"


type alias Token =
    String


type HttpError
    = NetworkError
    | ClientError { statusCode : String, body : String }
    | ServerError { statusCode : String, body : String }
    | CodeError String


type Error error
    = HttpError HttpError
    | Error error
    | TokenExpired


type QueryError
    = NotFound


type SaveError
    = IDConflict


type LoginError
    = InvalidCreds


type ParticipantID
    = ParticipantID String


type alias BedNum =
    String


type alias BedAssignment =
    ( Participant, BedNum )


type alias NewBill =
    { billNo : String
    , bedAssignments : List BedAssignment
    }


type alias Bill =
    { billNo : String
    , bedAssignments : List BedAssignment
    , dereg : Bool
    }


type alias Participant =
    { name : String
    , college : String
    , mobile : String
    , shortid : String
    }


type alias LoginCreds =
    { userid : String, password : String }



-- PUBLIC FUNCTIONS


getParticipants : Token -> (Result (Error ()) (List Participant) -> msg) -> Cmd msg
getParticipants token toMsg =
    Http.request
        { method = "GET"
        , url = serverUrl ++ "participants/nano"
        , expect =
            expectJson
                toMsg
                (\code -> Nothing)
                (D.list participantDecoder)
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


saveBill : Token -> NewBill -> (Result (Error SaveError) () -> msg) -> Cmd msg
saveBill token newBill toMsg =
    Http.request
        { method = "POST"
        , url = serverUrl ++ "hospitality/"
        , body = Http.jsonBody (encodeNewBill newBill)
        , headers = [ authHeader token ]
        , expect =
            expectWhatever
                (toMsg << Debug.log "saveBill")
                (\code ->
                    case code of
                        403 ->
                            Just IDConflict

                        _ ->
                            Nothing
                )
        , timeout = Nothing
        , tracker = Nothing
        }


getBill : Token -> String -> (Result (Error QueryError) Bill -> msg) -> Cmd msg
getBill token billNo toMsg =
    Http.request
        { method = "GET"
        , url = serverUrl ++ "hospitality/" ++ billNo
        , body = Http.emptyBody
        , headers = [ authHeader token ]
        , expect =
            expectJson
                (toMsg << Debug.log "getBill")
                (\code ->
                    case code of
                        404 ->
                            Just NotFound

                        _ ->
                            Nothing
                )
                billDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


login : LoginCreds -> (Result (Error LoginError) Token -> msg) -> Cmd msg
login creds toMsg =
    Http.request
        { method = "POST"
        , url = serverUrl ++ "auth/admin/get-token"
        , headers = []
        , body = Http.jsonBody (encodeLogin creds)
        , expect =
            expectJson
                (toMsg << Debug.log "login")
                (\code ->
                    case code of
                        401 ->
                            Just InvalidCreds

                        _ ->
                            Nothing
                )
                loginTokenDecorder
        , timeout = Nothing
        , tracker = Nothing
        }



-- DECODERS


loginTokenDecorder : D.Decoder Token
loginTokenDecorder =
    D.map (\x -> x)
        (D.field "token" D.string)


participantDecoder : D.Decoder Participant
participantDecoder =
    D.map4
        Participant
        (D.field "name" D.string)
        (D.field "college" D.string)
        (D.field "mobile" D.string)
        (D.field "short_id" D.string)


bedAssignmentDecoder : D.Decoder BedAssignment
bedAssignmentDecoder =
    D.map2 Tuple.pair
        (D.field "participant" participantDecoder)
        (D.field "bedid" D.string)


billDecoder : D.Decoder Bill
billDecoder =
    D.map3
        Bill
        (D.field "bill_no" D.string)
        (D.field "BedAssignments" (D.list bedAssignmentDecoder))
        (D.field "dereg" D.bool)



--- ENCODERS


encodeLogin : LoginCreds -> E.Value
encodeLogin creds =
    E.object
        [ ( "username", E.string creds.userid )
        , ( "password", E.string creds.password )
        ]


encodeNewBill : NewBill -> E.Value
encodeNewBill bill =
    E.object
        [ ( "bill_no", E.string bill.billNo )
        , ( "bookings", E.list encodeBedAssignment bill.bedAssignments )
        ]


encodeBedAssignment : BedAssignment -> E.Value
encodeBedAssignment ( participant, bedNum ) =
    E.object
        [ ( "short_id", E.string participant.shortid )
        , ( "bed_id", E.string bedNum )
        ]



-- PRIVATE


authHeader : Token -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


expectWhatever toMsg toErr =
    expectJson toMsg toErr (D.succeed ())


expectJson :
    (Result (Error error) contentObj -> msg)
    -> (Int -> Maybe error)
    -> D.Decoder contentObj
    -> Http.Expect msg
expectJson toMsg toErr decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| HttpError (CodeError ("Bad URL: " ++ url))

                Http.Timeout_ ->
                    Err (HttpError NetworkError)

                Http.NetworkError_ ->
                    Err (HttpError NetworkError)

                Http.BadStatus_ metadata body ->
                    case toErr metadata.statusCode of
                        Just err ->
                            Err <| Error err

                        _ ->
                            if metadata.statusCode // 100 == 5 then
                                Err <| HttpError (ServerError { statusCode = String.fromInt metadata.statusCode, body = body })

                            else
                                Err <| HttpError (ClientError { statusCode = String.fromInt metadata.statusCode, body = body })

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Err e ->
                            Err <| HttpError (ServerError { statusCode = "599", body = "Failed to decode JSON response: " ++ D.errorToString e })

                        Ok obj ->
                            Ok obj
