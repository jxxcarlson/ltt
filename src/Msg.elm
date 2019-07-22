module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , FrontendMsg(..)
    , TimerCommand(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ValidationState(..)
    )

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Log exposing (Event, EventGrouping(..), Log)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User)


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | RequestLogs (Maybe User)
    | RequestUsers
    | SendSignInInfo String String
    | SendSignUpInfo String String String
    | SendLogToBackend (Maybe User) Log
    | CreateLog (Maybe User) Log
    | SendChangeLogName (Maybe User) String Log
    | BEDeleteEvent (Maybe User) Log Int


type ToFrontend
    = NoOpToFrontend
    | SendLogsToFrontend (List Log)
    | SendValidatedUser (Maybe User)
    | SendUserList (List User)


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = NoOpFrontendMsg
      -- Admin
    | SendUsers
      -- App
    | SetAppMode AppMode
    | SendUserLogs User
    | SentToBackendResult (Result WsError ())
      -- User
    | GotUserName String
    | GotPassword String
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
    | ChangeDuration Log Event
    | MakeEvent
    | DeleteEvent Int Int
    | GotEventDateAfterFilter String
    | GotEventDateBeforeFilter String
    | GetEvents Int
    | SetCurrentEvent Event
    | SetGroupFilter EventGrouping
      -- Log
    | MakeNewLog
    | GotNewLogName String
    | GotLogFilter String
    | GotChangedLogName String
    | ChangeLogName


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
