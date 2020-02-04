module Backend exposing (Model, app, userList)

-- import Frontend
-- import Lamdera exposing (ClientId, sendToFrontend)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Log exposing (Log)
import Maybe.Extra
import Set exposing (Set)
import TestData exposing (passwordDict, userDict)
import Types exposing (..)
import User exposing (PasswordDict, User, UserDict, UserInfo, Username)
import UserLog


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }



--
-- MODEL
--


type alias Model =
    Types.BackendModel


init : ( Model, Cmd BackendMsg )
init =
    ( { passwordDict = Dict.empty
      , userDict = Dict.empty
      , clients = Set.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )



-- updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, sendToFrontend clientId (SendUserList (userList model.userDict)) )

        -- ( model, Cmd.none )
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

        BEUpdateLog maybeUserName log ->
            case maybeUserName of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | userDict = UserLog.update user.username log model.userDict }
                    , sendToFrontend clientId (SendLogToFrontend log)
                    )

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

        DeleteLog maybeUsername log ->
            case maybeUsername of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        newUserDict =
                            UserLog.delete user.username log model.userDict

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
                -- ( p, u ) =
                --     User.deleteUser "jxxcarlson" ( model.passwordDict, model.userDict )
                --
                -- uu =
                --     UserLog.clean "jxxcarlson" u
                result =
                    User.addAdmin "jxxcarlson" "lobo4795" "jxxcarlson@gmail.com" ( model.passwordDict, model.userDict )
            in
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok ( p, u ) ->
                    ( { model | passwordDict = p, userDict = u }, Cmd.none )

        ClientJoin ->
            ( model, Cmd.none )

        SendUserDict ->
            -- let
            --     _ =
            --         Debug.log "Dict" model.passwordDict
            -- in
            ( model, Cmd.none )


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.sendToFrontend clientId msg



--
-- HELPERS
--


userList : UserDict Log -> List User
userList userDict =
    List.map (User.fromDict userDict) (Dict.keys userDict)
        |> Maybe.Extra.values
