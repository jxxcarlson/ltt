module Backend exposing (Model, app)

import Dict exposing (Dict)
import Frontend
import Lamdera.Backend
import Lamdera.Types exposing (..)
import Log exposing (Log)
import Msg exposing (..)
import Set exposing (Set)
import TestData exposing (log1, log2, user1)
import User exposing (UserDict, UserInfo, Username, addNewUser, validateUser)
import UserLog


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
    { userDict : UserDict Log
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { userDict = TestData.userDict, clients = Set.empty }, Cmd.none )


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
            case User.validateUser model.userDict username password of
                True ->
                    ( model, sendToFrontend clientId <| SendValidatedUser (Just username) )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        SendSignUpInfo username password email ->
            case addNewUser username password email model.userDict of
                Just newUserDict ->
                    ( { model | userDict = newUserDict }, sendToFrontend clientId <| SendValidatedUser (Just username) )

                Nothing ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        RequestLogs maybeUser ->
            case maybeUser of
                Nothing ->
                    ( model, sendToFrontend clientId (SendLogsToFrontend []) )

                Just username ->
                    let
                        usersLogs =
                            Dict.get username model.userDict
                                |> Maybe.map .data
                                |> Maybe.withDefault []
                    in
                    ( model, sendToFrontend clientId (SendLogsToFrontend usersLogs) )

        SendLogToBackend maybeUserName log ->
            case maybeUserName of
                Nothing ->
                    ( model, Cmd.none )

                Just username ->
                    ( { model | userDict = UserLog.update username log model.userDict }, Cmd.none )

        CreateLog maybeUsername log ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just username ->
                    ( { model | userDict = UserLog.create username log model.userDict }, Cmd.none )

        SendChangeLogName maybeUsername newLogName log ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just username ->
                    let
                        changedLog =
                            { log | name = newLogName }
                    in
                    ( { model | userDict = UserLog.update username changedLog model.userDict }, Cmd.none )

        BEDeleteEvent maybeUsername log eventId ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just username ->
                    ( { model | userDict = UserLog.deleteEvent username log model.userDict eventId }, Cmd.none )

        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--
