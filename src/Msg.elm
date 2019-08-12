module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , DeleteEventSafety(..)
    , DeleteLogSafety(..)
    , FrontendMsg(..)
    , TimerCommand(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Log exposing (Event, EventGrouping(..), Log, Meta)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User)
import UserLog exposing (UserStats)


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
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = NoOpFrontendMsg
      -- Admin
    | SendUsers
    | AdminCleanData
      -- App
    | SetAppMode AppMode
    | SendUserLogs User
    | SentToBackendResult (Result WsError ())
      -- User
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
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
