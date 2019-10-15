module AppState exposing (AppState, Auth, getAuth, getParticipants, init, setAuth, setParticipantsList)

import Backend exposing (Participant)


type Token
    = String


type Auth
    = LoggedIn Token
    | LoggedOut


type AppState
    = AppState AppStateRecord


type alias AppStateRecord =
    { auth : Auth
    , participants : List Participant
    }


init : AppState
init =
    AppState (AppStateRecord LoggedOut [])


toInner : AppState -> AppStateRecord
toInner appState =
    case appState of
        AppState record ->
            record


getAuth : AppState -> Auth
getAuth appState =
    .auth <| toInner appState


getParticipants : AppState -> List Participant
getParticipants appState =
    .participants <| toInner appState


setAuth : AppState -> Auth -> ( AppState, Cmd msg )
setAuth appState auth =
    let
        record =
            toInner appState
    in
    ( AppState { record | auth = auth }, Cmd.none )


setParticipantsList : AppState -> List Participant -> ( AppState, Cmd msg )
setParticipantsList appState participants =
    let
        record =
            toInner appState
    in
    ( AppState { record | participants = participants }, Cmd.none )
