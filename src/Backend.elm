module Backend exposing
    ( BedAssignment
    , Bill
    , Error(..)
    , HttpError(..)
    , LoginCreds
    , LoginError(..)
    , NewBill
    , Participant
    , ParticipantEx
    , QueryError(..)
    , RemoteData(..)
    , SaveError(..)
    , Token
    , deregBill
    , getBill
    , getParticipant
    , getParticipants
    , hasTwoDayWorkshop
    , hasWorkshop
    , login
    , progressToString
    , saveBill
    , toParticipant
    )

import Http exposing (Progress)
import Json.Decode as D
import Json.Encode as E
import List
import Process
import Round
import Task


serverUrl =
    -- "http://localhost:5000/"
    -- "https://staging.tathva.org/"
    "https://api.tathva.org/"


type RemoteData data error
    = NotRequested
    | Loading
    | Loaded data
    | LoadFailed error


type alias Token =
    String


type HttpError
    = NetworkError
    | BadStatus { statusCode : String, body : String }
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


type alias BedAssignment =
    { participant : Participant, bedno : String }


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
    , shortId : String
    , email : String
    , tathva_pass : Bool
    }


type alias ParticipantEx =
    { name : String
    , college : String
    , mobile : String
    , shortId : String
    , email : String
    , tathva_pass : Bool
    , workshops : List Workshop
    }


type alias Workshop =
    { title : String, two_days : Bool }


type alias LoginCreds =
    { userid : String, password : String }



-- PUBLIC FUNCTIONS


hasWorkshop : ParticipantEx -> Bool
hasWorkshop participant =
    List.length participant.workshops > 0


hasTwoDayWorkshop : ParticipantEx -> Bool
hasTwoDayWorkshop participant =
    (List.filter (\w -> w.two_days) participant.workshops |> List.length) > 0


toParticipant : ParticipantEx -> Participant
toParticipant participant =
    { name = participant.name
    , college = participant.college
    , mobile = participant.mobile
    , shortId = participant.shortId
    , email = participant.email
    , tathva_pass = participant.tathva_pass
    }


getParticipants : Token -> (Result (Error ()) (List Participant) -> msg) -> Maybe String -> Cmd msg
getParticipants token toMsg progressTracker =
    Http.request
        { method = "GET"
        , url = serverUrl ++ "participants/nano"
        , expect =
            expectJson
                toMsg
                (\code -> Nothing)
                (D.at [ "participants" ]
                    (D.list participantDecoder)
                )
        , headers = [ authHeader token ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = progressTracker
        }


getParticipant : Token -> (Result (Error QueryError) ParticipantEx -> msg) -> String -> Cmd msg
getParticipant token toMsg shortid =
    Http.request
        { method = "GET"
        , url = serverUrl ++ "participants/" ++ shortid
        , expect =
            expectJson
                toMsg
                (\code ->
                    if code == 404 then
                        Just NotFound

                    else
                        Nothing
                )
                (D.at [ "participant" ]
                    participantExDecoder
                )
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
                (D.at [ "bill" ] billDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


deregBill : Token -> String -> Bool -> (Result (Error QueryError) () -> msg) -> Cmd msg
deregBill token billNo dereg toMsg =
    Http.request
        { method = "GET"
        , url =
            serverUrl
                ++ "hospitality/"
                ++ billNo
                ++ "/dereg?status="
                ++ (if dereg then
                        "true"

                    else
                        "false"
                   )
        , body = Http.emptyBody
        , headers = [ authHeader token ]
        , expect =
            expectWhatever
                (toMsg << Debug.log "deregBill")
                (\code ->
                    case code of
                        404 ->
                            Just NotFound

                        _ ->
                            Nothing
                )
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


progressToString : Progress -> String
progressToString progress =
    case progress of
        Http.Sending { sent, size } ->
            bytesToString sent
                ++ "/"
                ++ bytesToString size
                ++ "("
                ++ Round.round 0 (Http.fractionSent { sent = sent, size = size } * 100)
                ++ "% )"

        Http.Receiving { received, size } ->
            bytesToString received
                ++ "/"
                ++ bytesToString (Maybe.withDefault 0 size)
                ++ "("
                ++ Round.round 0 (Http.fractionReceived { received = received, size = size } * 100)
                ++ "%)"


bytesToString bytes =
    if bytes < 1024 then
        String.fromInt bytes ++ " B"

    else if bytes < 1024 * 1024 then
        Round.round 2 (toFloat bytes / 1024) ++ " KB"

    else if bytes < 1024 * 1024 * 1024 then
        Round.round 2 (toFloat bytes / (1024 * 1024)) ++ " MB"

    else
        Round.round 2 (toFloat bytes / (1024 * 1024 * 1024)) ++ " GB"



-- DECODERS


loginTokenDecorder : D.Decoder Token
loginTokenDecorder =
    D.map (\x -> x)
        (D.field "token" D.string)


workshopDecoder : D.Decoder Workshop
workshopDecoder =
    D.map2 Workshop
        (D.field "name" D.string)
        (D.maybe (D.at [ "meta", "two_day" ] D.string)
            |> D.map
                (\v ->
                    case v of
                        Just "yes" ->
                            True

                        _ ->
                            False
                )
        )


participantDecoder : D.Decoder Participant
participantDecoder =
    D.map6
        Participant
        (D.field "name" D.string)
        (D.field "college" D.string)
        (D.maybe (D.field "mobile" D.string)
            |> D.map
                (\maybeMobile ->
                    case maybeMobile of
                        Just mobile ->
                            mobile

                        Nothing ->
                            "N/A"
                )
        )
        (D.field "short_id" D.string)
        (D.field "email" D.string)
        (D.field "tathva_pass" D.bool)


participantExDecoder : D.Decoder ParticipantEx
participantExDecoder =
    D.map7
        ParticipantEx
        (D.field "name" D.string)
        (D.field "college" D.string)
        (D.maybe (D.field "mobile" D.string)
            |> D.map
                (\maybeMobile ->
                    case maybeMobile of
                        Just mobile ->
                            mobile

                        Nothing ->
                            "N/A"
                )
        )
        (D.field "short_id" D.string)
        (D.field "email" D.string)
        (D.field "tathva_pass" D.bool)
        (D.field "events"
            (D.list (D.maybe workshopDecoder)
                |> D.map (\list -> List.filterMap (\x -> x) list)
            )
        )


bedAssignmentDecoder : D.Decoder BedAssignment
bedAssignmentDecoder =
    D.map2 BedAssignment
        (D.field "participant" participantDecoder)
        (D.field "bedid" D.string)


billDecoder : D.Decoder Bill
billDecoder =
    D.map3
        Bill
        (D.field "bill_no" D.string)
        (D.field "bookings" (D.list bedAssignmentDecoder))
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
encodeBedAssignment { participant, bedno } =
    E.object
        [ ( "short_id", E.string participant.shortId )
        , ( "bed_id", E.string bedno )
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
                            if metadata.statusCode == 401 then
                                Err <| TokenExpired

                            else
                                Err <| HttpError (BadStatus { statusCode = String.fromInt metadata.statusCode, body = body })

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Err e ->
                            Err <| HttpError (CodeError ("Failed to decode JSON response: " ++ D.errorToString e))

                        Ok obj ->
                            Ok obj
