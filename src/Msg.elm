module Msg exposing (BackendMsg(..), FrontendMsg(..), TimerCommand(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Log exposing (Event, EventGrouping(..), Log)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (UserId)


type ToBackend
    = NoOpToBackend
    | ClientJoin
    | RequestLogs
    | SendLogsToBackend (List Log)


type ToFrontend
    = NoOpToFrontend
    | SendLogsToFrontend (List Log)


type FrontendMsg
    = NoOpFrontendMsg
    | SendUserLogs UserId
    | SentToBackendResult (Result WsError ())
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


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())
