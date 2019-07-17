module Frontend exposing (Model, app)

--

import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Date exposing (Date)
import DateTime exposing (NaiveDateTime(..))
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
import Log exposing (..)
import Msg exposing (BackendMsg(..), FrontendMsg(..), TimerCommand(..), ToBackend(..), ToFrontend(..))
import Style
import Task
import TestData exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)
import Url exposing (Url)
import User exposing (UserId)


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


type AppMode
    = Logging
    | Editing


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
    , message : String
    , eventDurationString : String
    , logs : List Log
    , maybeCurrentLog : Maybe Log
    , maybeCurrentEvent : Maybe Event
    , logFilterString : String
    , appMode : AppMode
    , visibilityOfLogList : Visibility
    , beginTime : Maybe Posix
    , currentTime : Posix
    , elapsedTime : TypedTime
    , accumulatedTime : TypedTime
    , doUpdateElapsedTime : Bool
    , timerState : TimerState
    , dateFilter : DateFilter
    , timeZoneOffset : Int
    , filterState : EventGrouping
    , outputUnit : Unit
    }



--
-- INIT
--


init : ( Model, Cmd FrontendMsg )
init =
    ( { input = "App started"
      , message = "App started"
      , eventDurationString = ""
      , logs = []
      , maybeCurrentLog = Nothing
      , maybeCurrentEvent = Nothing
      , logFilterString = ""
      , appMode = Logging
      , visibilityOfLogList = Visible
      , currentTime = Time.millisToPosix 0
      , beginTime = Nothing
      , doUpdateElapsedTime = False
      , elapsedTime = TypedTime Seconds 0
      , accumulatedTime = TypedTime Seconds 0
      , dateFilter = NoDateFilter
      , timeZoneOffset = 5
      , timerState = TSInitial
      , filterState = NoGrouping
      , outputUnit = Hours
      }
    , Cmd.batch
        [ sendToBackend timeoutInMs SentToBackendResult ClientJoin
        , sendToBackend timeoutInMs SentToBackendResult RequestLogs
        ]
    )


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
    ( case msg of
        NoOpToFrontend ->
            model

        SendLogsToFrontend newLogList ->
            { model | logs = newLogList }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        SendUserLogs userId ->
            ( model, Cmd.none )

        SentToBackendResult result ->
            ( model, Cmd.none )

        ChangeUrl url ->
            ( model, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Cmd.none )

                External url ->
                    ( model, Cmd.none )

        -- UI
        ToggleLogs ->
            ( { model | visibilityOfLogList = toggleVisibility model.visibilityOfLogList }, Cmd.none )

        -- EVENTS
        GotValueString str ->
            ( { model | eventDurationString = str }, Cmd.none )

        GetEvents logId ->
            let
                maybeLog =
                    List.filter (\log -> log.id == logId) model.logs
                        |> List.head
            in
            ( { model | maybeCurrentEvent = Nothing, maybeCurrentLog = maybeLog }, Cmd.none )

        SetCurrentEvent event_ ->
            ( { model | maybeCurrentEvent = Just event_ }, Cmd.none )

        SetGroupFilter filterState ->
            ( { model | filterState = filterState }, Cmd.none )

        SetUnits unit ->
            ( { model | outputUnit = unit }, Cmd.none )

        MakeEvent ->
            case model.maybeCurrentLog of
                Nothing ->
                    let
                        _ =
                            Debug.log "BAD BRANCH (1)"
                    in
                    ( { model | message = "No log available to make event" }, Cmd.none )

                Just log ->
                    let
                        r =
                            addEventUsingString (Debug.log "EVT STRING" model.eventDurationString) model.currentTime log model.logs
                    in
                    ( { model | logs = r.logList, maybeCurrentLog = Just r.currentLog }, r.cmd )

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
                            let
                                _ =
                                    Debug.log "BAD BRANCH"
                            in
                            ( model, Cmd.none )

                        Just log ->
                            let
                                _ =
                                    Debug.log "BAD BRANCH"

                                duration =
                                    Debug.log "TIME TO LOG" <|
                                        TypedTime.sum [ Debug.log "ACC" model.accumulatedTime, Debug.log "ELAPSED" model.elapsedTime ]

                                r =
                                    addEvent duration model.currentTime log model.logs
                            in
                            ( { model | logs = r.logList, maybeCurrentLog = Just r.currentLog }, r.cmd )

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
        , mainRow model
        ]



--
-- HEADER
--


header : Model -> Element FrontendMsg
header model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        ]
        [ toggleLogsButton model ]


toggleLogsButton : Model -> Element FrontendMsg
toggleLogsButton model =
    let
        message =
            showOne (model.visibilityOfLogList == Visible) "Hide Logs" "Show Logs"
    in
    Input.button Style.headerButton
        { onPress = Just ToggleLogs
        , label = Element.text message
        }



--
-- MAIN ROW
--


mainRow : Model -> Element FrontendMsg
mainRow model =
    row (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ -- filterPanel model
          showIf (model.visibilityOfLogList == Visible) (logListPanel model)
        , eventListDisplay model
        , eventPanel model
        ]


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
                    Log.dateFilter today model.dateFilter currentLog.data

                eventSum_ =
                    Log.eventSum events2

                events : List Event
                events =
                    groupingFilter model.filterState events2

                nEvents =
                    List.length events |> toFloat

                average =
                    TypedTime.multiply (1.0 / nEvents) eventSum_
            in
            column [ spacing 12, padding 20, height (px 430), scrollbarY ]
                [ el [ Font.size 16, Font.bold ] (text (Maybe.map .name model.maybeCurrentLog |> Maybe.withDefault "XXX"))
                , indexedTable [ spacing 4, Font.size 12 ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text "idx")
                          , width = px (indexWidth model.appMode)
                          , view = indexButton model
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80

                          --, view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          , view = \k event -> el [ Font.size 12 ] (text <| DateTime.naiveDateStringFromPosix <| event.insertedAt)
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
                    [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Average: " ++ TypedTime.timeAsStringWithUnit Minutes average)
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Total: " ++ TypedTime.timeAsStringWithUnit Minutes eventSum_)
                    ]
                ]



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
                today =
                    DateTime.naiveDateStringFromPosix model.currentTime

                events2 =
                    dateFilter model.currentTime model.dateFilter currentLog.data

                events =
                    case model.filterState of
                        NoGrouping ->
                            events2

                        GroupByDay ->
                            Log.eventsByDay events2
            in
            column [ Font.size 12, spacing 36, moveRight 40, width (px 450) ]
                [ row [ moveLeft 40 ] [ graph model events ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newEventPanel 350 model
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
    column [ Border.width 1, padding 12, spacing 24, width (px w) ]
        [ row [ spacing 12 ] [ submitEventButton, inputEventDuration model ]
        , largeElapsedTimePanel model
        ]


prepareData : Float -> List Event -> List Float
prepareData scaleFactor_ eventList =
    List.map (floatValueOfEvent scaleFactor_) eventList


floatValueOfEvent : Float -> Event -> Float
floatValueOfEvent scaleFactor_ event =
    event |> .duration |> convertToSeconds |> (\x -> x / scaleFactor_)


inputEventDuration model =
    Input.text inputStyle
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
    column [ spacing 12 ]
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


{-| xxx
-}
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


inputStyle =
    [ width (px 60)
    , height (px 30)
    , Background.color (Style.makeGrey 0.8)
    , Font.color Style.black
    , Font.size 12
    , Border.width 2
    ]


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



-- editPanel model =
--     column [ spacing 12, width (px 300), alignTop ]
--         [ el [ Font.size 16, Font.bold, alignTop ] (text "Edit Panel")
--         , logEditPanel model
--         , logEventPanel model
--         ]
--
--
-- filterPanel model =
--     row [ spacing 8 ]
--         [ el [ Font.bold ] (text "Filter:")
--         , inputLogNameFilter model
--         , el [ Font.bold ] (text "Since:")
--         , inputEventDateFilter model
--         , row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, logModeButton model ]
--         ]


logListPanel : Model -> Element FrontendMsg
logListPanel model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLogs model
        ]



--
-- eventListDisplay : Model -> Element FrontendMsg
-- eventListDisplay model =
--     column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
--         [ viewEvents model
--         ]
--
--
-- controlPanel model =
--     column [ padding 8, Border.width 1, width (px 562), spacing 12 ]
--         [ newLogPanel model
--         , el [ Font.size 14 ] (text <| model.message)
--         , el [ Font.size 11 ] (text <| "Server: " ++ Configuration.backend)
--         ]
--
--
--
-- STYLE
--


viewLogs : Model -> Element FrontendMsg
viewLogs model =
    column [ spacing 12, padding 20, height (px 400) ]
        [ el [ Font.size 16, Font.bold ] (text "Logs")
        , indexedTable
            [ spacing 4, Font.size 12 ]
            { data = Log.filter model.logFilterString model.logs
            , columns =
                [ { header = el [ Font.bold ] (text "k")
                  , width = px 40
                  , view = \k log -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                  }
                , { header = el [ Font.bold ] (text "Name")
                  , width = px 80
                  , view = \k log -> el [ Font.size 12 ] (logNameButton model.maybeCurrentLog log)
                  }
                ]
            }
        ]


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


indexButton : Model -> Int -> Event -> Element FrontendMsg
indexButton model k event =
    case model.appMode of
        Logging ->
            el [ Font.size 12 ] (text <| String.fromInt <| k + 1)

        Editing ->
            setCurrentEventButton model event k


setCurrentEventButton : Model -> Event -> Int -> Element FrontendMsg
setCurrentEventButton model event index =
    Input.button (Style.titleButton (Just event == model.maybeCurrentEvent))
        { onPress = Just (SetCurrentEvent event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt index ++ ": " ++ String.fromInt event.id)
        }



--
-- UPDATE HELPERS
--


type alias UpdateLogRecord =
    { currentLog : Log
    , logList : List Log
    , cmd : Cmd FrontendMsg
    }


addEventUsingString : String -> Posix -> Log -> List Log -> UpdateLogRecord
addEventUsingString eventDurationString currentTime log logList =
    case TypedTime.decodeHM eventDurationString of
        Nothing ->
            { currentLog = log, logList = logList, cmd = Cmd.none }

        Just duration ->
            addEvent duration currentTime log logList


addEvent : TypedTime -> Posix -> Log -> List Log -> UpdateLogRecord
addEvent duration currentTime log logList =
    let
        newLog =
            Log.insertEvent "" duration currentTime log

        newLogs =
            Log.replaceLog newLog logList

        cmd =
            sendToBackend timeoutInMs SentToBackendResult (SendLogsToBackend newLogs)
    in
    { currentLog = newLog, logList = newLogs, cmd = cmd }



--
-- END
--
