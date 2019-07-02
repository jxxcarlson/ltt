module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Date exposing (Date)
import DateTime exposing (NaiveDateTime(..))
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Option(..))
import Html exposing (Html)
import Log exposing (..)
import Style
import TestData exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)



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


type PhoneAppMode
    = ShowingLogs
    | ShowingEvents
    | ShowingChart


type TimerState
    = TSInitial
    | TSRunning
    | TSPaused


type TimerCommand
    = TCStart
    | TCPause
    | TCContinue
    | TCLog
    | TCReset



--
-- MAIN
--


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , message = "App started"
      , eventDurationString = ""
      , logs = [ TestData.log, TestData.log2 ]
      , maybeCurrentLog = Just TestData.log
      , maybeCurrentEvent = Just TestData.e1
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
    , Cmd.none
    )



--
-- MSG
--


type Msg
    = NoOp
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


type alias Flags =
    {}


subscriptions model =
    Time.every 1000 TimeChange



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
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
                    ( { model | message = "No log available to make event" }, Cmd.none )

                Just log ->
                    case TypedTime.decodeHM model.eventDurationString of
                        Nothing ->
                            ( { model | message = "Bad format for time" }, Cmd.none )

                        Just duration ->
                            ( createEvent (TypedTime Seconds duration) model, Cmd.none )

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
                    let
                        duration =
                            TypedTime.sum [ model.accumulatedTime, model.elapsedTime ]
                    in
                    ( createEvent duration model, Cmd.none )

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


view : Model -> Html Msg
view model =
    Element.layout [] (mainView model)


mainView : Model -> Element Msg
mainView model =
    column []
        [ header model
        , mainRow model
        ]



--
-- HEADER
--


header : Model -> Element Msg
header model =
    row
        [ width fill
        , paddingXY 40 8
        , Background.color Style.charcoal
        ]
        [ toggleLogsButton model ]


toggleLogsButton : Model -> Element Msg
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


mainRow : Model -> Element Msg
mainRow model =
    row (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ -- filterPanel model
          showIf (model.visibilityOfLogList == Visible) (logListPanel model)
        , eventListDisplay model
        , eventPanel model
        ]


showIf : Bool -> Element Msg -> Element Msg
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


viewLog : Model -> Element Msg
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


eventPanel : Model -> Element Msg
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
                [ row [ moveLeft 40 ] [ Graph.barChart (gA model) (prepareData (getScaleFactor model) events) |> Element.html ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newEventPanel 350 model
                ]



--
--
--


newEventPanel : Int -> Model -> Element Msg
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


submitEventButton : Element Msg
submitEventButton =
    Input.button Style.button
        { onPress = Just MakeEvent
        , label = Element.text "New: minutes or hh:mm"
        }


setMinutesButton : Model -> Element Msg
setMinutesButton model =
    Input.button (Style.activeButton (model.outputUnit == Minutes))
        { onPress = Just (SetUnits Minutes)
        , label = el [ Font.size 12 ] (text "Minutes")
        }


setHoursButton : Model -> Element Msg
setHoursButton model =
    Input.button (Style.activeButton (model.outputUnit == Hours))
        { onPress = Just (SetUnits Hours)
        , label = el [ Font.size 12 ] (text "Hours")
        }


noFilterButton : Model -> Element Msg
noFilterButton model =
    Input.button (Style.activeButton (model.filterState == NoGrouping))
        { onPress = Just (SetGroupFilter NoGrouping)
        , label = el [ Font.size 12 ] (text "None")
        }


filterByDayButton : Model -> Element Msg
filterByDayButton model =
    Input.button (Style.activeButton (model.filterState == GroupByDay))
        { onPress = Just (SetGroupFilter GroupByDay)
        , label = el [ Font.size 12 ] (text "By day")
        }



--
-- TIMER
--


largeElapsedTimePanel : Model -> Element Msg
largeElapsedTimePanel model =
    column [ spacing 12 ]
        [ timerDisplay model
        , timerControls model
        ]


timerControls : Model -> Element Msg
timerControls model =
    row [ spacing 12, Font.size 12, width fill ]
        [ startTimerButton
        , pauseTimerButton model
        , resetTimerButton
        , logTimerButton
        ]


{-| xxx
-}
startTimerButton : Element Msg
startTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCStart)
        , label = el [ Font.size 14 ] (text "Start")
        }


pauseTimerButton : Model -> Element Msg
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


resetTimerButton : Element Msg
resetTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCReset)
        , label = el [ Font.size 14 ] (text "Reset")
        }


logTimerButton : Element Msg
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


eventListDisplay : Model -> Element Msg
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


logListPanel : Model -> Element Msg
logListPanel model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLogs model
        ]



--
-- eventListDisplay : Model -> Element Msg
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


viewLogs : Model -> Element Msg
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


logNameButton : Maybe Log -> Log -> Element Msg
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


indexButton : Model -> Int -> Event -> Element Msg
indexButton model k event =
    case model.appMode of
        Logging ->
            el [ Font.size 12 ] (text <| String.fromInt <| k + 1)

        Editing ->
            setCurrentEventButton model event k


setCurrentEventButton : Model -> Event -> Int -> Element Msg
setCurrentEventButton model event index =
    Input.button (Style.titleButton (Just event == model.maybeCurrentEvent))
        { onPress = Just (SetCurrentEvent event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt index ++ ": " ++ String.fromInt event.id)
        }



--
-- UPDATE HELPERS
--


createEvent : TypedTime -> Model -> Model
createEvent duration model =
    let
        newEvent =
            { note = ""
            , id = -1
            , duration = duration
            , insertedAt = model.currentTime
            }
    in
    case model.maybeCurrentLog of
        Nothing ->
            { model | timerState = TSInitial }

        Just currentLog ->
            let
                newLog =
                    { currentLog | data = currentLog.data ++ [ newEvent ] }
            in
            { model
                | timerState = TSInitial
                , doUpdateElapsedTime = False
                , maybeCurrentLog = Just newLog
            }
