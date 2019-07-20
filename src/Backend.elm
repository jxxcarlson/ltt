module Backend exposing (Model, app)

import Dict exposing (Dict)
import Frontend
import Lamdera.Backend
import Lamdera.Types exposing (..)
import Log exposing (Log)
import Msg exposing (..)
import Set exposing (Set)
import TestData exposing (passwordDict, userDict)
import User exposing (PasswordDict, UserDict, UserInfo, Username)
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
    { passwordDict : PasswordDict
    , userDict : UserDict Log
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { passwordDict = TestData.passwordDict
      , userDict = TestData.userDict
      , clients = Set.empty
      }
    , Cmd.none
    )


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
            case User.validateUser model.passwordDict username password of
                True ->
                    ( model, sendToFrontend clientId <| SendValidatedUser (User.fromDict username model.userDict) )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        SendSignUpInfo username password email ->
            case User.add username password email ( model.passwordDict, model.userDict ) of
                Ok ( newPasswordDict, newUserDict ) ->
                    ( { model | userDict = newUserDict, passwordDict = newPasswordDict }
                    , sendToFrontend clientId <| SendValidatedUser (User.fromDict username model.userDict)
                    )

                Err str ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        RequestLogs maybeUser ->
            case maybeUser of
                Nothing ->
                    ( model, sendToFrontend clientId (SendLogsToFrontend []) )

                Just user ->
                    let
                        usersLogs =
                            Dict.get user.username model.userDict
                                |> Maybe.map .data
                                |> Maybe.withDefault []
                    in
                    ( model, sendToFrontend clientId (SendLogsToFrontend usersLogs) )

        SendLogToBackend maybeUserName log ->
            case maybeUserName of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserLog.update user.username log model.userDict }, Cmd.none )

        CreateLog maybeUsername log ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserLog.create user.username log model.userDict }, Cmd.none )

        SendChangeLogName maybeUsername newLogName log ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        changedLog =
                            { log | name = newLogName }
                    in
                    ( { model | userDict = UserLog.update user.username changedLog model.userDict }, Cmd.none )

        BEDeleteEvent maybeUsername log eventId ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserLog.deleteEvent user.username log model.userDict eventId }, Cmd.none )

        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--
