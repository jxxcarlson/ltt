module Backend exposing (Model, app)

import Lamdera.Backend
import Lamdera.Types exposing (..)
import Log exposing (Log)
import Msg exposing (..)
import Set exposing (Set)
import TestData exposing (log1, log2, user1)
import User exposing (User, validateUser)


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }



--
-- MODEL
--


type alias Model =
    { logs : List Log, users : List User, clients : Set ClientId }


init : ( Model, Cmd BackendMsg )
init =
    ( { logs = [ log1, log2 ], users = [ user1 ], clients = Set.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        -- Our sendToFrontend Cmd has completed
        SentToFrontendResult clientId result ->
            case result of
                Ok () ->
                    -- Message was delivered successfully! We don't do anything
                    -- with this info, but other apps might.
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        SendSignInInfo username password ->
            let
                _ =
                    Debug.log "VAL U" <| User.validateUser model.users username password
            in
            case User.validateUser model.users username password of
                True ->
                    ( model, sendToFrontend clientId <| SendValidatedUser (validUser username model.users) )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        RequestLogs ->
            ( model, sendToFrontend clientId (SendLogsToFrontend model.logs) )

        SendLogsToBackend logList ->
            ( { model | logs = logList }, Cmd.none )

        SendLogToBackend log ->
            ( { model | logs = Log.replaceLog log model.logs }, Cmd.none )

        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg


validUser : String -> List User -> Maybe User
validUser username userList =
    case List.filter (\user -> user.userName == username) userList of
        [ user ] ->
            Just { user | encryptedPassword = "" }

        _ ->
            Nothing



-- sendToFrontend :
--     Milliseconds
--     -> ClientId
--     -> (Result WsError () -> backendMsg)
--     -> toFrontend
--     -> Cmd backendMsg
