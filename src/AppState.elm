module AppState exposing (AppState, Auth(..), getAuth, getNavKey, init, setAuth)

import Backend exposing (Token)
import Browser.Navigation as Nav
import Json.Encode as E
import Ports


type Auth
    = LoggedIn Token
    | LoggedOut


type AppState
    = AppState AppStateRecord


type alias AppStateRecord =
    { auth : Auth
    , navKey : Nav.Key
    }


init : Maybe String -> Nav.Key -> AppState
init flags navKey =
    let
        flags_ =
            Maybe.withDefault "" flags
    in
    if flags_ /= "" then
        AppState (AppStateRecord (LoggedIn flags_) navKey)

    else
        AppState (AppStateRecord LoggedOut navKey)


toInner : AppState -> AppStateRecord
toInner appState =
    case appState of
        AppState record ->
            record


getAuth : AppState -> Auth
getAuth appState =
    .auth <| toInner appState


setAuth : AppState -> Auth -> ( AppState, Cmd msg )
setAuth appState auth =
    let
        record =
            toInner appState
    in
    updateCache (AppState { record | auth = auth })


getNavKey : AppState -> Nav.Key
getNavKey appState =
    .navKey <| toInner appState


toString : AppState -> String
toString appState =
    let
        record =
            toInner appState
    in
    case record.auth of
        LoggedIn token ->
            token

        LoggedOut ->
            ""


updateCache : AppState -> ( AppState, Cmd msg )
updateCache appState =
    ( appState, Ports.cache (toString appState) )
