module AppState exposing (AppState, Auth(..), getAuth, getNavKey, getParticipants, init, setAuth, setParticipantsList)

import Backend exposing (Participant, Token)
import Browser.Navigation as Nav


type Auth
    = LoggedIn Token
    | LoggedOut


type AppState
    = AppState AppStateRecord


type alias AppStateRecord =
    { auth : Auth
    , participants : List Participant
    , navKey : Nav.Key
    }


init : Nav.Key -> AppState
init navKey =
    AppState (AppStateRecord LoggedOut [] navKey)


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


getNavKey : AppState -> Nav.Key
getNavKey appState =
    .navKey <| toInner appState
