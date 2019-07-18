module Msg exposing
    ( AppMode(..)
    , BackendMsg(..)
    , FrontendMsg(..)
    , TimerCommand(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Log exposing (Event, EventGrouping(..), Log)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (User, Username)


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | RequestLogs (Maybe User)
    | SendLogsToBackend (List Log)
    | SendSignInInfo String String
    | SendSignUpInfo String String
    | SendLogToBackend Log


type ToFrontend
    = NoOpToFrontend
    | SendLogsToFrontend (List Log)
    | SendValidatedUser (Maybe User)


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type FrontendMsg
    = NoOpFrontendMsg
    | SetAppMode AppMode
    | SendUserLogs Username
    | SentToBackendResult (Result WsError ())
      --
    | GotUserName String
    | GotPassword String
    | SignIn
    | SignUp
    | SignOut
      --
    | ChangeUrl Url
    | ClickLink UrlRequest
    | GetEvents Int
    | SetCurrentEvent Event
    | SetGroupFilter EventGrouping
    | SetUnits Unit
    | ToggleLogs
      --
    | TimeChange Posix
    | TC TimerCommand
    | UpdateElapsedTime Float
    | GotValueString String
    | MakeEvent


type TimerCommand
    = TCStart
    | TCPause
    | TCContinue
    | TCLog
    | TCReset


type AppMode
    = Logging
    | Editing
    | UserValidation
