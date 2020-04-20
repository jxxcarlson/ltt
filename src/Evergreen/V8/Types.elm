module Evergreen.V8.Types exposing
    ( AppMode(..)
    , BackendModel
    , BackendMsg(..)
    , DeleteEventSafety(..)
    , DeleteLogSafety(..)
    , FrontendModel
    , FrontendMsg(..)
    , TimerCommand(..)
    , TimerState(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    , Visibility(..)
    )

import Browser exposing (UrlRequest(..))
import Lamdera exposing (ClientId)
import Log exposing (Event, EventGrouping(..), Log, Meta)
import Set exposing (Set)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (PasswordDict, User, UserDict)
import UserLog exposing (UserStats)


type alias BackendModel =
    { passwordDict : PasswordDict
    , userDict : UserDict Log
    , clients : Set ClientId
    }


type alias FrontendModel =
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
    , changedEventDateString : String
    , deleteEventSafety : DeleteEventSafety
    , eventDurationString : String
    , eventCameBeforeString : String
    , eventCameAfterString : String
    , maybeCurrentEvent : Maybe Event
    , filterState : EventGrouping

    --, dateFilters : List DateFilter
    -- LOGS
    , logs : List ( Log, Meta )
    , grandTotalTime : TypedTime
    , newLogName : String
    , changedLogName : String
    , maybeCurrentLog : Maybe ( Log, Meta )
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
    , fooBar : String
    }


type Visibility
    = Visible
    | Hidden


type TimerState
    = TSInitial
    | TSRunning
    | TSPaused


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | CleanData
    | RequestLogs (Maybe User)
    | RequestUsers
    | SendSignInInfo String String
    | SendSignUpInfo String String String
    | SendChangePasswordInfo User.Username String String
    | BEUpdateLog (Maybe User) Log
    | CreateLog (Maybe User) Log
    | DeleteLog (Maybe User) Log
    | SendChangeLogName (Maybe User) String Log
    | BEDeleteEvent (Maybe User) Log Int
    | GetUserStats
    | SendUserDict


type ToFrontend
    = NoOpToFrontend
    | SendMessage String
    | SendLogsToFrontend (List Log)
    | SendValidatedUser (Maybe User)
    | SendUserList (List User)
    | SendUserStats UserStats
    | SendLogToFrontend Log


type BackendMsg
    = NoOpBackendMsg


type FrontendMsg
    = NoOpFrontendMsg
      -- Admin
    | SendUsers
    | AdminCleanData
      -- App
    | SetAppMode AppMode
    | SendUserLogs User
      -- User
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | Test
    | SignUp
    | SignOut
      -- Url
    | ChangeUrl Url
    | ClickLink UrlRequest
    | ToggleLogs
      -- Time
    | SetUnits Unit
    | TimeChange Posix
    | TC TimerCommand
    | UpdateElapsedTime Float
    | GotValueString String
      -- Event
    | GotChangedEventDuration String
    | GotChangedEventDate String
    | ChangeDuration ( Log, Meta ) Event
    | ChangeEventDate ( Log, Meta ) Event
    | MakeEvent
    | DeleteEvent Int Int
    | SetDeleteEventSafety DeleteEventSafety
    | GotEventDateAfterFilter String
    | GotEventDateBeforeFilter String
    | GetEvents Int
    | SetCurrentEvent Event
    | SetGroupFilter EventGrouping
      -- Log
    | MakeNewLog
    | DeleteCurrentLog
    | SetDeleteLogSafety DeleteLogSafety
    | GotNewLogName String
    | GotLogFilter String
    | GotChangedLogName String
    | ChangeLogName
    | ClearFilters
    | ApplyFilters


type DeleteLogSafety
    = DeleteLogSafetyOn
    | DeleteLogSafetyOff


type DeleteEventSafety
    = DeleteEventSafetyOn
    | DeleteEventSafetyOff


type TimerCommand
    = TCStart
    | TCPause
    | TCContinue
    | TCLog
    | TCReset


type AppMode
    = Logging
    | Editing
    | UserValidation ValidationState
    | Admin


type ValidationState
    = SignInState
    | SignUpState
    | ChangePasswordState
