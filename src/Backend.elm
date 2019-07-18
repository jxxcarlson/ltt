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



-- sendToFrontend :
--     Milliseconds
--     -> ClientId
--     -> (Result WsError () -> backendMsg)
--     -> toFrontend
--     -> Cmd backendMsg
