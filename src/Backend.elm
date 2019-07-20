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

        SendSignUpInfo username password ->
            case addNewUser username password model.userDict of
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
                    case Dict.get username model.userDict of
                        Nothing ->
                            ( model, Cmd.none )

                        Just userInfo ->
                            let
                                changedLog =
                                    { log | name = newLogName }

                                updater : Maybe (UserInfo Log) -> Maybe (UserInfo Log)
                                updater =
                                    Maybe.map (\uInfo -> { uInfo | data = Log.replaceLog changedLog uInfo.data })
                            in
                            ( { model | userDict = Dict.update username updater model.userDict }, Cmd.none )

        BEDeleteEvent maybeUsername logId eventId ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just username ->
                    ( model, Cmd.none )

        -- case Dict.get username model.userDict of
        --     Nothing ->
        --         ( model, Cmd.none )
        --
        --     Just userInfo ->
        --         let
        --             maybeTargetLog : Maybe Log
        --             maybeTargetLog =
        --                 List.filter (\log -> log.id == logId) userInfo.data
        --                     |> List.head
        --
        --             maybeChangedData =
        --                 Maybe.map (List.filter (\event -> event.id /= eventId)) (Maybe.map .data maybeTargetLog)
        --
        --             maybeChangedLog =
        --
        --             updater : Log -> Maybe (UserInfo Log) -> Maybe (UserInfo Log)
        --             updater changedLog_ =
        --                 Maybe.map (\uInfo -> { uInfo | data = Log.replaceLog changedLog_ uInfo.data })
        --         in
        --         -- case maybeChangedLog of
        --             Nothing ->
        --                 ( model, Cmd.none )
        --
        --             Just changedLog ->
        --                 ( { model | userDict = Dict.update username (updater changedLog) model.userDict }, Cmd.none )
        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--
