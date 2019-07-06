module Msg exposing (BackendMsg(..), FrontendMsg(..), TimerCommand(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest(..))
import Lamdera.Types exposing (ClientId, WsError)
import Log exposing (Event, EventGrouping(..))
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (UserId)


type FrontendMsg
    = NoOpFrontendMsg
    | SendUserLogs UserId
    | ClientJoin
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


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg
    | SentToFrontendResult ClientId (Result WsError ())


type ToFrontend
    = NoOpToFrontend
