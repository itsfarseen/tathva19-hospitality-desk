module Backend exposing (Error, Participant, ParticipantID, Token, getParticipants, login)

import Http
import Json.Decode as D
import Json.Encode as E
import List
import Process
import Task


serverUrl =
    "http://127.0.0.1:5000/"


type Token
    = Token String


type Error
    = NetworkError
    | ClientError { statusCode : String, body : String }
    | ServerError { statusCode : String, body : String }
    | CodeError String


type ParticipantID
    = ParticipantID String


type alias Participant =
    { id : ParticipantID
    , name : String
    , college : String
    , mobile : String
    , shortid : String
    }


getParticipants : (Result Error (List Participant) -> msg) -> Cmd msg
getParticipants toMsg =
    Http.get
        { url = serverUrl ++ "participants"
        , expect = expectJson toMsg participantsDecoder
        }


login : { userid : String, password : String } -> (Result Error (Maybe Token) -> msg) -> Cmd msg
login creds toMsg =
    if creds.userid == "admin" && creds.password == "admin" then
        delay 2000 <|
            toMsg <|
                Ok (Just (Token "LoginSuccessToken"))

    else
        delay 2000 (toMsg (Ok Nothing))


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


participantsDecoder : D.Decoder (List Participant)
participantsDecoder =
    D.list <|
        D.map5
            Participant
            (D.field "_id" (D.map ParticipantID D.string))
            (D.field "name" D.string)
            (D.field "college" D.string)
            (D.field "mobile" D.string)
            (D.field "shortid" D.string)


expectJson : (Result Error contentObj -> msg) -> D.Decoder contentObj -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| CodeError ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err NetworkError

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode // 100 == 5 then
                        Err <| ServerError { statusCode = String.fromInt metadata.statusCode, body = body }

                    else
                        Err <| ClientError { statusCode = String.fromInt metadata.statusCode, body = body }

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Err e ->
                            Err <| ServerError { statusCode = "599", body = "Failed to decode JSON response: " ++ D.errorToString e }

                        Ok obj ->
                            Ok obj
