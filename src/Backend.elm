module Backend exposing (Model, app, userList)

import Dict exposing (Dict)
import Frontend
import Lamdera.Backend
import Lamdera.Types exposing (..)
import Log exposing (Log)
import Maybe.Extra
import Msg exposing (..)
import Set exposing (Set)
import TestData exposing (passwordDict, userDict)
import User exposing (PasswordDict, User, UserDict, UserInfo, Username)
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
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestUsers ->
            ( model, sendToFrontend clientId (SendUserList (userList model.userDict)) )

        GetUserStats ->
            ( model, sendToFrontend clientId (SendUserStats (UserLog.compile model.userDict)) )

        SendSignInInfo username password ->
            case User.validateUser model.passwordDict username password of
                True ->
                    ( model, sendToFrontend clientId <| SendValidatedUser (User.fromDict model.userDict username) )

                False ->
                    ( model, sendToFrontend clientId <| SendValidatedUser Nothing )

        SendChangePasswordInfo username password newPassword ->
            case User.validateUser model.passwordDict username password of
                True ->
                    let
                        passwordUpdater =
                            Maybe.map (\ep -> User.encrypt newPassword)

                        newPasswordDict =
                            Dict.update username passwordUpdater model.passwordDict
                    in
                    ( { model | passwordDict = newPasswordDict }, sendToFrontend clientId <| SendMessage "Password changed" )

                False ->
                    ( model, sendToFrontend clientId <| SendMessage "Could not change password" )

        SendSignUpInfo username password email ->
            case User.add username password email ( model.passwordDict, model.userDict ) of
                Ok ( newPasswordDict, newUserDict ) ->
                    ( { model | userDict = newUserDict, passwordDict = newPasswordDict }
                    , sendToFrontend clientId <| SendValidatedUser (User.fromDict newUserDict username)
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
                    let
                        newUserDict =
                            UserLog.create user.username log model.userDict

                        usersLogs =
                            Dict.get user.username newUserDict
                                |> Maybe.map .data
                                |> Maybe.withDefault []
                    in
                    ( { model | userDict = newUserDict }
                    , sendToFrontend clientId (SendLogsToFrontend usersLogs)
                    )

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

        CleanData ->
            let
                ( p, u ) =
                    User.deleteUser "" ( model.passwordDict, model.userDict )

                uu =
                    UserLog.clean "jxxcarlson" u
            in
            ( { model | passwordDict = p, userDict = uu }, Cmd.none )

        ClientJoin ->
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId (\_ -> NoOpBackendMsg) msg



--
-- HELPERS
--


userList : UserDict Log -> List User
userList userDict =
    List.map (User.fromDict userDict) (Dict.keys userDict)
        |> Maybe.Extra.values
