module Frontend exposing (Model, app)

--
-- import Main exposing (Logging)
-- import Svg.Attributes exposing (k1)
-- exposing (..)
-- import Date exposing (Date)

import Array exposing (map)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import DateTime
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Option(..))
import Html exposing (Html, time)
import Lamdera.Frontend as Frontend
import Lamdera.Types exposing (..)
import Log exposing (DateFilter(..), Event, EventGrouping(..), Log)
import Msg exposing (AppMode(..), BackendMsg(..), DeleteEventSafety(..), DeleteLogSafety(..), FrontendMsg(..), TimerCommand(..), ToBackend(..), ToFrontend(..), ValidationState(..))
import Style
import Task
import TestData exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User, Username)
import UserLog exposing (UserStats)
import Utility


app =
    Frontend.application
        { init = \_ _ -> init
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Lamdera Alpha"
                , body = [ view model ]
                }
        }



--
-- TYPES
--


type Visibility
    = Visible
    | Hidden


toggleVisibility : Visibility -> Visibility
toggleVisibility vis =
    case vis of
        Visible ->
            Hidden

        Hidden ->
            Visible


type TimerState
    = TSInitial
    | TSRunning
    | TSPaused



--
-- MODEL
--


type alias Model =
    { input : String
    , appMode : AppMode
    , message : String
    , visibilityOfLogList : Visibility

    -- ADMIN
    , userStats : UserStats

    -- USER
    , currentUser : Maybe User
    , username : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String
    , email : String
    , userList : List User

    -- EVENTS
    , changedEventDurationString : String
    , deleteEventSafety : DeleteEventSafety
    , eventDurationString : String
    , eventCameBeforeString : String
    , eventCameAfterString : String
    , maybeCurrentEvent : Maybe Event
    , filterState : EventGrouping

    --, dateFilters : List DateFilter
    -- LOGS
    , logs : List Log
    , newLogName : String
    , changedLogName : String
    , maybeCurrentLog : Maybe Log
    , logFilterString : String
    , deleteLogSafety : DeleteLogSafety

    -- TIMER
    , beginTime : Maybe Posix
    , currentTime : Posix
    , elapsedTime : TypedTime
    , accumulatedTime : TypedTime
    , doUpdateElapsedTime : Bool
    , timerState : TimerState

    --
    , timeZoneOffset : Int
    , outputUnit : Unit
    }



--
-- INIT
--


initialModel =
    { input = "App started"
    , message = "Please sign in"
    , appMode = UserValidation SignInState

    -- ADMIN
    , userStats = Dict.empty

    -- USER
    , currentUser = Nothing
    , username = ""
    , password = ""
    , newPassword1 = ""
    , newPassword2 = ""
    , email = ""
    , userList = []

    -- EVENT
    , changedEventDurationString = ""
    , eventDurationString = ""
    , deleteEventSafety = DeleteEventSafetyOn
    , eventCameBeforeString = ""
    , eventCameAfterString = ""

    -- LOGS
    , logs = []
    , newLogName = ""
    , changedLogName = ""
    , maybeCurrentLog = Nothing
    , maybeCurrentEvent = Nothing
    , logFilterString = ""
    , visibilityOfLogList = Visible
    , deleteLogSafety = DeleteLogSafetyOn

    -- TIME
    , currentTime = Time.millisToPosix 0
    , beginTime = Nothing
    , doUpdateElapsedTime = False
    , elapsedTime = TypedTime Seconds 0
    , accumulatedTime = TypedTime Seconds 0

    --, dateFilter = []
    , timeZoneOffset = 5
    , timerState = TSInitial
    , filterState = NoGrouping
    , outputUnit = Hours
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( initialModel, sendToBackend timeoutInMs SentToBackendResult ClientJoin )


timeoutInMs =
    5 * 1000


sendToBackend : Milliseconds -> (Result WsError () -> FrontendMsg) -> ToBackend -> Cmd FrontendMsg
sendToBackend =
    Frontend.sendToBackend


subscriptions model =
    Time.every 1000 TimeChange



--
-- UPDATE
--


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        SendUserStats userStats ->
            ( { model | userStats = userStats }, Cmd.none )

        SendMessage str ->
            ( { model | message = str }, Cmd.none )

        SendLogsToFrontend newLogList ->
            ( { model | logs = newLogList }, Cmd.none )

        SendUserList userList ->
            ( { model | userList = userList }, Cmd.none )

        SendValidatedUser currentUser ->
            case currentUser of
                Nothing ->
                    ( { model | currentUser = Nothing, message = "Incorrect password/username" }, Cmd.none )

                Just user ->
                    ( { model | currentUser = Just user, appMode = Logging }
                    , sendToBackend timeoutInMs SentToBackendResult (RequestLogs (Just user))
                    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        -- ADMIN
        AdminCleanData ->
            ( model, sendToBackend timeoutInMs SentToBackendResult CleanData )

        SendUsers ->
            case currentUserIsAdmin model of
                False ->
                    ( model, Cmd.none )

                True ->
                    ( model
                    , sendToBackend timeoutInMs SentToBackendResult RequestUsers
                    )

        -- BACKEND
        SendUserLogs userId ->
            ( model, Cmd.none )

        SentToBackendResult result ->
            ( model, Cmd.none )

        -- URL (NOT USED)
        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Cmd.none )

        -- UI
        SetAppMode mode ->
            let
                cmd =
                    case mode of
                        Admin ->
                            Cmd.batch
                                [ sendToBackend timeoutInMs SentToBackendResult RequestUsers
                                , sendToBackend timeoutInMs SentToBackendResult GetUserStats
                                ]

                        _ ->
                            Cmd.none

                filterState =
                    case mode of
                        Editing ->
                            NoGrouping

                        _ ->
                            model.filterState

                message =
                    case mode of
                        UserValidation SignInState ->
                            "Please sign in"

                        UserValidation SignUpState ->
                            "Please sign up"

                        _ ->
                            ""

                visibilityOfLogList =
                    case mode of
                        Editing ->
                            Visible

                        _ ->
                            model.visibilityOfLogList
            in
            ( { model
                | appMode = mode
                , visibilityOfLogList = visibilityOfLogList
                , filterState = filterState
                , message = message
              }
            , cmd
            )

        SetDeleteLogSafety deleteLogSafetyState ->
            ( { model | deleteLogSafety = deleteLogSafetyState }, Cmd.none )

        SetDeleteEventSafety deleteEventSafetyState ->
            ( { model | deleteEventSafety = deleteEventSafetyState }, Cmd.none )

        ToggleLogs ->
            ( { model | visibilityOfLogList = toggleVisibility model.visibilityOfLogList }, Cmd.none )

        -- USER
        GotUserName str ->
            ( { model | username = str }, Cmd.none )

        GotPassword str ->
            ( { model | password = str }, Cmd.none )

        GotNewPassword1 str ->
            ( { model | newPassword1 = str }, Cmd.none )

        GotNewPassword2 str ->
            ( { model | newPassword2 = str }, Cmd.none )

        ChangePassword ->
            case User.validateChangePassword model.newPassword1 model.newPassword2 of
                [] ->
                    case model.currentUser of
                        Nothing ->
                            ( { model | message = "No user signed in" }, Cmd.none )

                        Just user ->
                            ( { model | message = "OK" }
                            , sendToBackend timeoutInMs SentToBackendResult (SendChangePasswordInfo user.username model.password model.newPassword1)
                            )

                errorList ->
                    ( { model | message = String.join ", " errorList }, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            ( initialModel, sendToBackend timeoutInMs SentToBackendResult (SendSignInInfo model.username model.password) )

        SignUp ->
            let
                signUpErrors =
                    User.validateSignUpInfo model.username model.password model.email
            in
            case List.length signUpErrors > 0 of
                True ->
                    ( { model | message = String.join ", " signUpErrors }, Cmd.none )

                False ->
                    ( initialModel, sendToBackend timeoutInMs SentToBackendResult (SendSignUpInfo model.username model.password model.email) )

        SignOut ->
            ( initialModel, Cmd.none )

        -- EVENT
        GotChangedEventDuration str ->
            ( { model | changedEventDurationString = str }, Cmd.none )

        ChangeDuration log event ->
            let
                r =
                    changeEventUsingString model.currentUser "" model.changedEventDurationString event log model.logs
            in
            ( { model | logs = r.logList, maybeCurrentLog = Just r.currentLog }, r.cmd )

        GotLogFilter str ->
            ( { model | logFilterString = str }, Cmd.none )

        GotEventDateAfterFilter str ->
            ( { model | eventCameAfterString = str }, Cmd.none )

        GotEventDateBeforeFilter str ->
            ( { model | eventCameBeforeString = str }, Cmd.none )

        GotValueString str ->
            ( { model | eventDurationString = str }, Cmd.none )

        GetEvents logId ->
            let
                maybeLog =
                    List.filter (\log -> log.id == logId) model.logs
                        |> List.head

                maybeLogName =
                    case maybeLog of
                        Nothing ->
                            ""

                        Just log ->
                            log.name
            in
            ( { model
                | maybeCurrentEvent = Nothing
                , maybeCurrentLog = maybeLog
                , changedLogName = maybeLogName
              }
            , Cmd.none
            )

        SetCurrentEvent event_ ->
            ( { model | maybeCurrentEvent = Just event_ }, Cmd.none )

        SetGroupFilter filterState ->
            ( { model | filterState = filterState }, Cmd.none )

        SetUnits unit ->
            ( { model | outputUnit = unit }, Cmd.none )

        MakeEvent ->
            case model.maybeCurrentLog of
                Nothing ->
                    ( { model | message = "No log available to make event" }, Cmd.none )

                Just log ->
                    let
                        r =
                            addEventUsingString model.currentUser model.eventDurationString model.currentTime log model.logs
                    in
                    ( { model | logs = r.logList, maybeCurrentLog = Just r.currentLog }, r.cmd )

        DeleteEvent logId eventId ->
            case model.maybeCurrentLog of
                Nothing ->
                    ( model, Cmd.none )

                Just log ->
                    let
                        changedLog =
                            Log.deleteEvent log eventId
                    in
                    ( { model
                        | logs = Log.replaceLog changedLog model.logs
                        , maybeCurrentLog = Just changedLog
                      }
                    , sendToBackend timeoutInMs SentToBackendResult (SendLogToBackend model.currentUser changedLog)
                    )

        MakeNewLog ->
            case newLog model of
                Nothing ->
                    ( model, Cmd.none )

                Just newLog_ ->
                    ( { model | maybeCurrentLog = Just newLog_, logs = newLog_ :: model.logs }
                    , sendToBackend timeoutInMs SentToBackendResult (CreateLog model.currentUser newLog_)
                    )

        DeleteCurrentLog ->
            case model.maybeCurrentLog of
                Nothing ->
                    ( { model | deleteLogSafety = DeleteLogSafetyOn }, Cmd.none )

                Just log ->
                    ( { model | maybeCurrentLog = Nothing, deleteLogSafety = DeleteLogSafetyOn }
                    , sendToBackend timeoutInMs SentToBackendResult (DeleteLog model.currentUser log)
                    )

        ChangeLogName ->
            case model.maybeCurrentLog of
                Nothing ->
                    ( model, Cmd.none )

                Just log ->
                    let
                        changedLog =
                            { log | name = model.changedLogName }
                    in
                    ( { model | logs = Log.replaceLog changedLog model.logs }, sendToBackend timeoutInMs SentToBackendResult (SendChangeLogName model.currentUser model.changedLogName log) )

        GotNewLogName str ->
            ( { model | newLogName = str }, Cmd.none )

        GotChangedLogName str ->
            ( { model | changedLogName = str }, Cmd.none )

        -- TIMER
        TimeChange time ->
            ( { model
                | currentTime = time
                , elapsedTime =
                    if model.doUpdateElapsedTime then
                        elapsedTypedTime model

                    else
                        model.elapsedTime
              }
            , Cmd.none
            )

        UpdateElapsedTime et ->
            ( { model | elapsedTime = TypedTime Seconds et }, Cmd.none )

        TC timerCommand ->
            case timerCommand of
                TCStart ->
                    ( { model
                        | beginTime = Just model.currentTime
                        , elapsedTime = TypedTime Seconds 0
                        , accumulatedTime = TypedTime Seconds 0
                        , doUpdateElapsedTime = True
                        , timerState = TSRunning
                      }
                    , Cmd.none
                    )

                TCPause ->
                    ( { model
                        | beginTime = Just model.currentTime
                        , accumulatedTime = TypedTime.sum [ model.accumulatedTime, model.elapsedTime ]
                        , elapsedTime = TypedTime Seconds 0
                        , doUpdateElapsedTime = False
                        , timerState = TSPaused
                      }
                    , Cmd.none
                    )

                TCContinue ->
                    ( { model
                        | timerState = TSRunning
                        , beginTime = Just model.currentTime
                        , doUpdateElapsedTime = True
                      }
                    , Cmd.none
                    )

                TCLog ->
                    case model.maybeCurrentLog of
                        Nothing ->
                            ( model, Cmd.none )

                        Just log ->
                            let
                                duration =
                                    TypedTime.sum [ model.accumulatedTime, model.elapsedTime ]

                                r =
                                    addEvent model.currentUser duration model.currentTime log model.logs
                            in
                            ( { model
                                | logs = r.logList
                                , timerState = TSInitial
                                , elapsedTime = TypedTime Seconds 0
                                , accumulatedTime = TypedTime Seconds 0
                                , doUpdateElapsedTime = False
                                , maybeCurrentLog = Just r.currentLog
                              }
                            , r.cmd
                            )

                TCReset ->
                    ( { model
                        | beginTime = Just model.currentTime
                        , doUpdateElapsedTime = False
                        , elapsedTime = TypedTime Seconds 0
                        , accumulatedTime = TypedTime Seconds 0
                        , timerState = TSInitial
                      }
                    , Cmd.none
                    )



--
-- VIEW
--


view : Model -> Html FrontendMsg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element FrontendMsg
mainView model =
    column []
        [ header model
        , case model.appMode of
            UserValidation _ ->
                userValidationView model

            Logging ->
                masterLogView model

            Editing ->
                editingView model

            Admin ->
                adminView model
        ]



--
-- USER VIEW
--


userValidationView : Model -> Element FrontendMsg
userValidationView model =
    case model.currentUser of
        Nothing ->
            noUserView model

        Just user ->
            signedInUserView model user


noUserView : Model -> Element FrontendMsg
noUserView model =
    column Style.mainColumnX
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (text "Welcome!")
        , inputUserName model
        , inputPassword model
        , showIf (model.appMode == UserValidation SignUpState) (inputEmail model)
        , showIf (model.appMode == UserValidation SignUpState) (el [ Font.size 12 ] (text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserValidation SignInState) (signInButton model)
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserValidation SignUpState) (cancelSignUpButton model)
                ]
            ]
        , el [ Font.size 12 ] (text model.message)
        ]


signedInUserView : Model -> User -> Element FrontendMsg
signedInUserView model user =
    column Style.mainColumnX
        [ el [] (text <| "Signed in as " ++ user.username)
        , signOutButton model
        , showIf (model.appMode == UserValidation ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ changePasswordButton model
            , showIf (model.appMode == UserValidation ChangePasswordState) (cancelChangePasswordButton model)
            ]
        , adminStatus model
        ]


passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 12 ] (text model.message)
        ]


inputCurrentPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Old password: ")
        }


inputNewPassword1 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword1
        , show = False
        , text = model.newPassword1
        , placeholder = Nothing

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "New password: ")
        }


inputNewPassword2 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword2
        , text = model.newPassword2
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (text "Password again: ")
        }


changePasswordButton : Model -> Element FrontendMsg
changePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just ChangePassword

                _ ->
                    Just <| SetAppMode (UserValidation ChangePasswordState)
        , label = Element.text "Change password"
        }


adminStatus : Model -> Element FrontendMsg
adminStatus model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    el [ Font.size 12 ] (text "Admin")


inputUserName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotUserName
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Username")
        }


inputEmail model =
    Input.text (Style.inputStyle 200)
        { onChange = GotEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Email")
        }


inputPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (text "Password")
        }


signInButton : Model -> Element FrontendMsg
signInButton model =
    Input.button Style.headerButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUpButton : Model -> Element FrontendMsg
signUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just SignUp

                _ ->
                    Just (SetAppMode (UserValidation SignUpState))
        , label = Element.text "Sign Up"
        }


cancelSignUpButton : Model -> Element FrontendMsg
cancelSignUpButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation SignUpState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just NoOpFrontendMsg
        , label = Element.text "Cancel"
        }


cancelChangePasswordButton : Model -> Element FrontendMsg
cancelChangePasswordButton model =
    Input.button Style.headerButton
        { onPress =
            case model.appMode of
                UserValidation ChangePasswordState ->
                    Just (SetAppMode (UserValidation SignInState))

                _ ->
                    Just NoOpFrontendMsg
        , label = Element.text "Cancel"
        }


signOutButton : Model -> Element FrontendMsg
signOutButton model =
    Input.button Style.headerButton
        { onPress = Just SignOut
        , label = Element.text "Sign out"
        }



--
-- EDITOR VIEW
--


changeDurationButton model =
    case ( model.maybeCurrentLog, model.maybeCurrentEvent ) of
        ( Just log, Just event ) ->
            Input.button Style.button
                { onPress = Just (ChangeDuration log event)
                , label = Element.text "Change duration"
                }

        _ ->
            Element.none


inputChangeEventDuration model =
    Input.text (Style.inputStyle 60)
        { onChange = GotChangedEventDuration
        , text = model.changedEventDurationString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "Duration: ")
        }


deleteEventButton model =
    case ( model.maybeCurrentLog, model.maybeCurrentEvent ) of
        ( Just lg, Just evt ) ->
            case model.deleteEventSafety of
                DeleteEventSafetyOff ->
                    Input.button Style.dangerousButton
                        { onPress = Just (DeleteEvent lg.id evt.id)
                        , label = Element.text <| "Remove forever?"
                        }

                DeleteEventSafetyOn ->
                    Input.button Style.button
                        { onPress = Just (SetDeleteEventSafety DeleteEventSafetyOff)
                        , label = Element.text <| "Remove event"
                        }

        _ ->
            Element.none


cancelDeleteEventButton =
    Input.button Style.button
        { onPress = Just (SetDeleteEventSafety DeleteEventSafetyOn)
        , label = Element.text <| "Cancel"
        }


changeLogNameButton : Element FrontendMsg
changeLogNameButton =
    Input.button Style.button
        { onPress = Just ChangeLogName
        , label = Element.text "Change log name"
        }


inputChangeLogName model =
    Input.text (Style.inputStyle 180)
        { onChange = GotChangedLogName
        , text = model.changedLogName
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
-- HEADER
--


header : Model -> Element FrontendMsg
header model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        , spacing 12
        ]
        [ showIf (currentUserIsAdmin model) (adminModeButton model)
        , userValidationModeButton model
        , showIf (model.currentUser /= Nothing) (loggingModeButton model)
        , showIf (model.currentUser /= Nothing) (editingModeButton model)
        , showIf (model.currentUser /= Nothing && model.appMode == Logging) (toggleLogsButton model)
        , el [ centerX, Font.size 18, Font.color Style.white ] (text <| "Time Log" ++ currentUserName model)
        ]


currentUserIsAdmin : Model -> Bool
currentUserIsAdmin model =
    case model.currentUser of
        Nothing ->
            False

        Just user ->
            user.admin


currentUserName : Model -> String
currentUserName model =
    case model.currentUser of
        Nothing ->
            ""

        Just user ->
            " for " ++ user.username


toggleLogsButton : Model -> Element FrontendMsg
toggleLogsButton model =
    let
        message =
            showOne (model.visibilityOfLogList == Visible) "Hide log list" "Show log list"
    in
    Input.button Style.headerButton
        { onPress = Just ToggleLogs
        , label = Element.text message
        }


userValidationModeButton : Model -> Element FrontendMsg
userValidationModeButton model =
    Input.button ((Style.select <| model.appMode == UserValidation SignInState) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode (UserValidation SignInState))
        , label = Element.text "User"
        }


adminModeButton : Model -> Element FrontendMsg
adminModeButton model =
    Input.button ((Style.select <| model.appMode == Admin) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Admin)
        , label = Element.text "Admin"
        }


loggingModeButton : Model -> Element FrontendMsg
loggingModeButton model =
    Input.button ((Style.select <| model.appMode == Logging) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Logging)
        , label = Element.text "Logs"
        }


editingModeButton : Model -> Element FrontendMsg
editingModeButton model =
    Input.button ((Style.select <| model.appMode == Editing) Style.selectedHeaderButton Style.headerButton)
        { onPress = Just (SetAppMode Editing)
        , label = Element.text "Edit"
        }



--
-- LOG ROW
--


masterLogView : Model -> Element FrontendMsg
masterLogView model =
    column Style.mainColumnX
        [ showIf (model.visibilityOfLogList == Visible) (filterPanel model)
        , row []
            [ showIf (model.visibilityOfLogList == Visible) (logListPanel model)
            , eventListDisplay model
            , eventPanel model
            ]
        , column [ spacing 12 ]
            [ row [ spacing 12 ]
                [ showIf (model.maybeCurrentLog /= Nothing) changeLogNameButton
                , showIf (model.maybeCurrentLog /= Nothing) (inputChangeLogName model)
                ]
            , showIf (model.maybeCurrentLog /= Nothing)
                (row [ spacing 12 ]
                    [ deleteLogButton model
                    , showIf (model.deleteLogSafety == DeleteLogSafetyOff) cancelDeleteLogButton
                    ]
                )
            ]
        ]


editingView : Model -> Element FrontendMsg
editingView model =
    column Style.mainColumnX
        [ showIf (model.visibilityOfLogList == Visible) (filterPanel model)
        , row [ spacing 12 ]
            [ logListPanel model
            , eventListDisplay model
            , logEventPanel model
            ]
        , column [ spacing 12 ]
            [ row [ spacing 12 ]
                [ showIf (model.maybeCurrentLog /= Nothing) changeLogNameButton
                , showIf (model.maybeCurrentLog /= Nothing) (inputChangeLogName model)
                ]
            , deleteLogButton model
            ]
        ]


newLogButton : Element FrontendMsg
newLogButton =
    Input.button Style.button
        { onPress = Just MakeNewLog
        , label = Element.text "New log"
        }


cancelDeleteLogButton : Element FrontendMsg
cancelDeleteLogButton =
    Input.button Style.button
        { onPress = Just <| SetDeleteLogSafety DeleteLogSafetyOn
        , label = Element.text "Cancel"
        }


deleteLogButton : Model -> Element FrontendMsg
deleteLogButton model =
    case model.deleteLogSafety of
        DeleteLogSafetyOn ->
            Input.button Style.button
                { onPress = Just <| SetDeleteLogSafety DeleteLogSafetyOff
                , label = Element.text "Delete log"
                }

        DeleteLogSafetyOff ->
            Input.button Style.dangerousButton
                { onPress = Just DeleteCurrentLog
                , label = Element.text "Delete forever?"
                }


inputNewLogName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotNewLogName
        , text = model.newLogName
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }



--
--
-- VIEWLOGS
--


viewLog : Model -> Element FrontendMsg
viewLog model =
    case model.maybeCurrentLog of
        Nothing ->
            column [ spacing 12, padding 20, height (px 500) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just currentLog ->
            let
                today =
                    model.currentTime

                events2 =
                    Log.bigDateFilter today model.eventCameBeforeString model.eventCameAfterString currentLog.data

                -- events : List Event
                -- events =
                --     Log.groupingFilter model.filterState events2
                events =
                    case model.filterState of
                        NoGrouping ->
                            events2

                        GroupByDay ->
                            Log.eventsByDay events2

                eventSum_ =
                    Log.eventSum events

                nEvents =
                    List.length events |> toFloat

                average =
                    TypedTime.multiply (1.0 / nEvents) eventSum_
            in
            column [ spacing 12, padding 20, height (px 430) ]
                [ el [ Font.size 16, Font.bold ] (text (Maybe.map .name model.maybeCurrentLog |> Maybe.withDefault "XXX"))
                , indexedTable [ spacing 4, Font.size 12, height (px 400), scrollbarY ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text <| idLabel model)
                          , width = px (indexWidth model.appMode)
                          , view = indexButton model
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80

                          --, view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.humanDateStringFromPosix <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Time")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.naiveTimeStringFromPosix <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Duration")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| TypedTime.timeAsStringWithUnit Minutes event.duration)
                          }
                        ]
                    }
                , row [ spacing 24, alignBottom, alignRight ]
                    [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Count: " ++ String.fromInt (List.length events))
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Average: " ++ TypedTime.timeAsStringWithUnit Minutes average)
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Total: " ++ TypedTime.timeAsStringWithUnit Minutes eventSum_)
                    ]
                ]


idLabel : Model -> String
idLabel model =
    case model.appMode of
        Editing ->
            "id"

        _ ->
            "idx"



--
-- VIEWEVENTS
--


eventPanel : Model -> Element FrontendMsg
eventPanel model =
    case model.maybeCurrentLog of
        Nothing ->
            Element.none

        Just currentLog ->
            let
                events2 =
                    Log.bigDateFilter model.currentTime model.eventCameBeforeString model.eventCameAfterString currentLog.data

                events =
                    case model.filterState of
                        NoGrouping ->
                            events2

                        GroupByDay ->
                            Log.eventsByDay events2
            in
            column [ Font.size 12, spacing 36, moveRight 40, width (px 400) ]
                [ row [ moveLeft 40 ] [ graph model events ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newEventPanel 350 model
                ]


logEventPanel model =
    case model.maybeCurrentEvent of
        Nothing ->
            Element.none

        Just evt ->
            column [ width (px 300), height (px 450), padding 12, Border.width 1, spacing 36 ]
                [ el [ Font.bold ] (text <| "Edit event " ++ String.fromInt evt.id)
                , column [ spacing 12 ]
                    [ inputChangeEventDuration model
                    , changeDurationButton model
                    ]
                , row [ spacing 12 ]
                    [ deleteEventButton model
                    , showIf (model.deleteEventSafety == DeleteEventSafetyOff) cancelDeleteEventButton
                    ]
                ]


graph model events_ =
    let
        events__ =
            List.reverse events_
    in
    Graph.barChart (gA model) (prepareData (getScaleFactor model) events__) |> Element.html



--
--
--


newEventPanel : Int -> Model -> Element FrontendMsg
newEventPanel w model =
    column [ spacing 24, width (px w) ]
        [ row [ Border.width 1, padding 12, spacing 12, width (px 300) ] [ submitEventButton, inputEventDuration model ]
        , largeElapsedTimePanel model
        ]


prepareData : Float -> List Event -> List Float
prepareData scaleFactor_ eventList =
    List.map (floatValueOfEvent scaleFactor_) eventList


floatValueOfEvent : Float -> Event -> Float
floatValueOfEvent scaleFactor_ event =
    event |> .duration |> convertToSeconds |> (\x -> x / scaleFactor_)


inputEventDuration model =
    Input.text (Style.inputStyle 60)
        { onChange = GotValueString
        , text = model.eventDurationString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


submitEventButton : Element FrontendMsg
submitEventButton =
    Input.button Style.button
        { onPress = Just MakeEvent
        , label = Element.text "New: minutes or hh:mm"
        }


setMinutesButton : Model -> Element FrontendMsg
setMinutesButton model =
    Input.button (Style.activeButton (model.outputUnit == Minutes))
        { onPress = Just (SetUnits Minutes)
        , label = el [ Font.size 12 ] (text "Minutes")
        }


setHoursButton : Model -> Element FrontendMsg
setHoursButton model =
    Input.button (Style.activeButton (model.outputUnit == Hours))
        { onPress = Just (SetUnits Hours)
        , label = el [ Font.size 12 ] (text "Hours")
        }


noFilterButton : Model -> Element FrontendMsg
noFilterButton model =
    Input.button (Style.activeButton (model.filterState == NoGrouping))
        { onPress = Just (SetGroupFilter NoGrouping)
        , label = el [ Font.size 12 ] (text "None")
        }


filterByDayButton : Model -> Element FrontendMsg
filterByDayButton model =
    Input.button (Style.activeButton (model.filterState == GroupByDay))
        { onPress = Just (SetGroupFilter GroupByDay)
        , label = el [ Font.size 12 ] (text "By day")
        }



--
-- TIMER
--


largeElapsedTimePanel : Model -> Element FrontendMsg
largeElapsedTimePanel model =
    column [ spacing 12, Border.width 1, padding 12, width (px 300) ]
        [ timerDisplay model
        , timerControls model
        ]


timerControls : Model -> Element FrontendMsg
timerControls model =
    row [ spacing 12, Font.size 12, width fill ]
        [ startTimerButton
        , pauseTimerButton model
        , resetTimerButton
        , logTimerButton
        ]


startTimerButton : Element FrontendMsg
startTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCStart)
        , label = el [ Font.size 14 ] (text "Start")
        }


pauseTimerButton : Model -> Element FrontendMsg
pauseTimerButton model =
    case model.timerState of
        TSPaused ->
            Input.button Style.smallButton
                { onPress = Just (TC TCContinue)
                , label = el [ Font.size 14 ] (text "Cont")
                }

        _ ->
            Input.button Style.button
                { onPress = Just (TC TCPause)
                , label = el [ Font.size 14 ] (text "Pause")
                }


resetTimerButton : Element FrontendMsg
resetTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCReset)
        , label = el [ Font.size 14 ] (text "Reset")
        }


logTimerButton : Element FrontendMsg
logTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCLog)
        , label = el [ Font.size 12 ] (text "Log")
        }


timerDisplay model =
    let
        t1 =
            TypedTime.sum [ model.elapsedTime, model.accumulatedTime ]

        t2 =
            TypedTime.multiply (1 / scaleFactor) t1
    in
    row [ spacing 8 ]
        [ el [ Font.size 36, Font.bold, padding 8, Font.color Style.red, Background.color Style.black ]
            (text <| TypedTime.timeAsStringWithUnit Seconds t1)
        ]


timeStringFromFloat : Float -> String
timeStringFromFloat t_ =
    let
        t =
            round t_

        s =
            modBy 60 t

        m =
            (t - s) // 60

        h =
            m // 60

        ss =
            String.pad 2 '0' (String.fromInt s)

        ms =
            String.pad 2 '0' (String.fromInt <| modBy 60 m)

        hs =
            String.pad 2 '0' (String.fromInt <| h)
    in
    hs ++ ":" ++ ms ++ ":" ++ ss


scaleFactor =
    1



-- VIEW HELPERS
--


showIf : Bool -> Element FrontendMsg -> Element FrontendMsg
showIf bit element =
    if bit then
        element

    else
        Element.none


showOne : Bool -> String -> String -> String
showOne bit str1 str2 =
    case bit of
        True ->
            str1

        False ->
            str2


newLog : Model -> Maybe Log
newLog model =
    case model.currentUser of
        Nothing ->
            Nothing

        Just user ->
            Just <|
                { id = -1
                , counter = 0
                , name = model.newLogName
                , username = user.username
                , note = ""
                , data = []
                }



--
-- ADMIN VIEW
--


adminView : Model -> Element FrontendMsg
adminView model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    adminView_ model user


adminView_ : Model -> User -> Element FrontendMsg
adminView_ model user =
    column Style.mainColumnX
        [ el [ Font.size 14 ] (text <| "Admin: " ++ user.username)
        , indexedTable
            [ spacing 4, Font.size 12, paddingXY 0 12, height (px 300), scrollbarY ]
            { data = model.userList
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k usr -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Username")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text usr.username)
                  }
                , { header = el [ Font.bold ] (text "Email")
                  , width = px 200
                  , view = \k usr -> el [ Font.size 12 ] (text usr.email)
                  }
                , { header = el [ Font.bold ] (text "Logs")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text <| displayNumberOfLogs usr.username model.userStats)
                  }
                , { header = el [ Font.bold ] (text "Events")
                  , width = px 80
                  , view = \k usr -> el [ Font.size 12 ] (text <| displayNumberOfEvents usr.username model.userStats)
                  }
                ]
            }
        , cleanDataButton model
        ]


displayNumberOfLogs : Username -> UserStats -> String
displayNumberOfLogs username userStats =
    Dict.get username userStats
        |> Maybe.map .numberOfLogs
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "-"


displayNumberOfEvents : Username -> UserStats -> String
displayNumberOfEvents username userStats =
    Dict.get username userStats
        |> Maybe.map .numberOfEvents
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "-"


cleanDataButton : Model -> Element FrontendMsg
cleanDataButton model =
    Input.button Style.headerButton
        { onPress = Just AdminCleanData
        , label = Element.text "Clean data"
        }



--
-- GRAPH HELPERS
--


gA model =
    let
        yTickMarks_ =
            4
    in
    { graphHeight = 200
    , graphWidth = 400
    , options = [ Color "blue", XTickmarks 7, YTickmarks yTickMarks_, DeltaX 10 ]
    }


getScaleFactor : Model -> Float
getScaleFactor model =
    case model.outputUnit of
        Seconds ->
            1

        Minutes ->
            60.0

        Hours ->
            3600.0


elapsedTypedTime : Model -> TypedTime
elapsedTypedTime model =
    case model.beginTime of
        Nothing ->
            TypedTime Seconds 0

        Just bt ->
            let
                milliSeconds2 =
                    Time.posixToMillis model.currentTime |> toFloat

                milliSeconds1 =
                    Time.posixToMillis bt |> toFloat

                dt =
                    (milliSeconds2 - milliSeconds1) / 1000.0
            in
            TypedTime Seconds dt


eventListDisplay : Model -> Element FrontendMsg
eventListDisplay model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLog model
        ]


filterPanel model =
    row [ spacing 8 ]
        [ el [ Font.bold ] (text "Filter:")
        , inputLogNameFilter model
        , row [ spacing 8 ]
            [ el [ Font.bold, Font.size 14 ] (text "After")
            , displayShiftedDate model.eventCameAfterString model.currentTime
            ]
        , inputEventCameAfterFilter model
        , row [ spacing 8 ]
            [ el [ Font.bold, Font.size 14 ] (text "Before")
            , displayShiftedDate model.eventCameBeforeString model.currentTime
            ]
        , inputEventCameBeforeFilter model

        --, row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, logModeButton model ]
        ]


displayShiftedDate : String -> Posix -> Element FrontendMsg
displayShiftedDate kDaysAgoString today =
    case String.toInt kDaysAgoString of
        Nothing ->
            el [ width (px 75), Font.bold, Font.size 14 ] (text "(days)")

        Just k ->
            let
                shiftedDate =
                    Log.kDaysAgo k today
            in
            el [ width (px 75), Font.bold, Font.size 14 ] (text <| DateTime.humanDateStringFromPosix shiftedDate)


inputLogNameFilter model =
    Input.text (Style.inputStyle 200)
        { onChange = GotLogFilter
        , text = model.logFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputEventCameBeforeFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotEventDateBeforeFilter
        , text = model.eventCameBeforeString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputEventCameAfterFilter model =
    Input.text (Style.inputStyle 50)
        { onChange = GotEventDateAfterFilter
        , text = model.eventCameAfterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


logListPanel : Model -> Element FrontendMsg
logListPanel model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLogs model
        ]


viewLogs : Model -> Element FrontendMsg
viewLogs model =
    let
        idx =
            logIdDisplay model.appMode

        grandTotal =
            Log.grandTotal model.logs

        fraction : Log -> String
        fraction log =
            TypedTime.divideBy grandTotal (Log.total log)
                |> (\x -> 100 * x)
                |> Utility.roundTo 0
                |> String.fromFloat
                |> String.padLeft 3 ' '
    in
    column [ spacing 12, padding 20, height (px 400) ]
        [ el [ Font.size 16, Font.bold ] (text "Logs")
        , indexedTable
            [ spacing 4, Font.size 12, height (px 370), scrollbarY ]
            { data = Log.filter model.logFilterString model.logs
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 20
                  , view = \k log -> el [ Font.size 12 ] (text <| idx k log)
                  }
                , { header = el [ Font.bold ] (text "Name")
                  , width = px 180
                  , view = \k log -> el [ Font.size 12 ] (logNameButton model.maybeCurrentLog log)
                  }
                , { header = el [ Font.bold ] (text "Total")
                  , width = px 60
                  , view = \k log -> el [ Font.size 12 ] (text <| timeAsStringWithUnit Minutes <| Log.total log)
                  }
                , { header = el [ Font.bold ] (text "pc")
                  , width = px 60
                  , view = \k log -> el [ Font.size 12, width (px 60), alignRight ] (text <| fraction log)
                  }
                ]
            }
        ]


logIdDisplay : AppMode -> Int -> Log -> String
logIdDisplay mode k log =
    case mode of
        Editing ->
            String.fromInt log.id

        _ ->
            String.fromInt (k + 1)


logNameButton : Maybe Log -> Log -> Element FrontendMsg
logNameButton currentLog log =
    Input.button (Style.titleButton (currentLog == Just log))
        { onPress = Just (GetEvents log.id)
        , label = Element.text log.name
        }


indexWidth : AppMode -> Int
indexWidth appMode =
    case appMode of
        Logging ->
            30

        Editing ->
            60

        _ ->
            60


indexButton : Model -> Int -> Event -> Element FrontendMsg
indexButton model k event =
    case model.appMode of
        Logging ->
            el [ Font.size 12 ] (text <| String.fromInt <| k + 1)

        Editing ->
            setCurrentEventButton model event k

        _ ->
            Element.none


setCurrentEventButton : Model -> Event -> Int -> Element FrontendMsg
setCurrentEventButton model event index =
    Input.button (Style.titleButton (Just event == model.maybeCurrentEvent))
        { onPress = Just (SetCurrentEvent event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt event.id)
        }



--
-- UPDATE HELPERS
--


type alias UpdateLogRecord =
    { currentLog : Log
    , logList : List Log
    , cmd : Cmd FrontendMsg
    }


addEventUsingString : Maybe User -> String -> Posix -> Log -> List Log -> UpdateLogRecord
addEventUsingString maybeUser eventDurationString currentTime log logList =
    case TypedTime.decodeHM eventDurationString of
        Nothing ->
            { currentLog = log, logList = logList, cmd = Cmd.none }

        Just duration ->
            addEvent maybeUser (TypedTime.convertFromSecondsWithUnit Seconds duration) currentTime log logList


addEvent : Maybe User -> TypedTime -> Posix -> Log -> List Log -> UpdateLogRecord
addEvent maybeUser duration currentTime log logList =
    let
        newLog_ =
            Log.insertEvent "" duration currentTime log

        newLogs =
            Log.replaceLog newLog_ logList

        cmd =
            sendToBackend timeoutInMs SentToBackendResult (SendLogToBackend maybeUser newLog_)
    in
    { currentLog = newLog_, logList = newLogs, cmd = cmd }


changeEventUsingString : Maybe User -> String -> String -> Event -> Log -> List Log -> UpdateLogRecord
changeEventUsingString maybeUser note eventDurationString event log logList =
    case TypedTime.decodeHM eventDurationString of
        Just duration ->
            changeEvent maybeUser note (TypedTime.convertFromSecondsWithUnit Seconds duration) event log logList

        Nothing ->
            { currentLog = log, logList = logList, cmd = Cmd.none }


changeEvent : Maybe User -> String -> TypedTime -> Event -> Log -> List Log -> UpdateLogRecord
changeEvent maybeUser note duration event log logList =
    let
        newLog_ =
            Log.updateEvent "" duration event log

        newLogs =
            Log.replaceLog newLog_ logList

        cmd =
            sendToBackend timeoutInMs SentToBackendResult (SendLogToBackend maybeUser newLog_)
    in
    { currentLog = newLog_, logList = newLogs, cmd = cmd }



--
-- END
--
