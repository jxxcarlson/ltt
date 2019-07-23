module Log exposing
    ( DateFilter(..)
    , Event
    , EventGrouping(..)
    , Log
    , bigDateFilter
    , deleteEvent
    , eventSum
    , eventsByDay
    , filter
    , groupingFilter
    , insertEvent
    , kDaysAgo
    , replaceLog
    , updateEvent
    )

import DateTime exposing (NaiveDateTime(..))
import List.Extra as LE
import Time exposing (Posix)
import TypedTime exposing (TypedTime(..), Unit(..))
import User exposing (Username)


type alias Log =
    { id : Int
    , counter : Int
    , name : String
    , note : String
    , username : Username
    , data : List Event
    }


type alias Event =
    { id : Int
    , note : String
    , duration : TypedTime
    , insertedAt : Posix
    }


type EventGrouping
    = NoGrouping
    | GroupByDay


type DateFilter
    = IdentityDateFilter
    | DateSuffixFilter Int
    | DatePrefixFilter Int


type Duration
    = Duration Float


bigDateFilter : Posix -> String -> String -> List Event -> List Event
bigDateFilter today prefixParameterString suffixParameterString eventList =
    eventList
        |> prefixFilter today prefixParameterString
        |> suffixFilter today suffixParameterString


suffixFilter : Posix -> String -> List Event -> List Event
suffixFilter today suffixParameterString eventList =
    case suffixParameterString |> String.toInt of
        Nothing ->
            eventList

        Just k ->
            dateSuffixFilter today k eventList


prefixFilter : Posix -> String -> List Event -> List Event
prefixFilter today prefixParameterString eventList =
    case prefixParameterString |> String.toInt of
        Nothing ->
            eventList

        Just k ->
            datePrefixFilter today k eventList


filter : String -> List Log -> List Log
filter filterString logs =
    List.filter (\log -> String.contains (String.toLower filterString) (String.toLower log.name)) logs


dateSuffixFilter : Posix -> Int -> List Event -> List Event
dateSuffixFilter today k eventList =
    let
        kDaysAgo_ =
            kDaysAgo k today
    in
    List.filter (\event -> posixInterval event.insertedAt kDaysAgo_ > 0) eventList


datePrefixFilter : Posix -> Int -> List Event -> List Event
datePrefixFilter today k eventList =
    let
        kDaysAgo_ =
            kDaysAgo k today
    in
    List.filter (\event -> posixInterval kDaysAgo_ event.insertedAt > 0) eventList


shiftPosix : Float -> Posix -> Posix
shiftPosix t p =
    ((Time.posixToMillis p |> toFloat) + (1000.0 * t))
        |> round
        |> Time.millisToPosix


kDaysAgo : Int -> Posix -> Posix
kDaysAgo k posix =
    shiftPosix (-86400.0 * toFloat k) posix


{-| Interval betwen two Posix times in Seconds
-}
posixInterval : Posix -> Posix -> Float
posixInterval p_ q_ =
    let
        p =
            Time.posixToMillis p_ |> toFloat

        q =
            Time.posixToMillis q_ |> toFloat
    in
    (p - q) / 1000.0



-- seconds


eventSum : List Event -> TypedTime
eventSum eventList =
    eventList
        |> List.map .duration
        |> TypedTime.sum


groupingFilter : EventGrouping -> List Event -> List Event
groupingFilter eventGrouping eventList =
    case eventGrouping of
        NoGrouping ->
            eventList

        GroupByDay ->
            -- eventsByDay eventList
            eventList


{-| Temporary (bad) fix
-}



-- eventsByDay : List Event -> List Event
-- eventsByDay list =
--     list


eventsByDay : List Event -> List Event
eventsByDay list =
    let
        referenceDT =
            Time.millisToPosix 0
    in
    list
        --|> List.map (\r -> offsetTimeZone timeZoneOffset r)
        |> timeSeries
        |> timeSeriesRD
        |> List.sortBy Tuple.first
        |> fillGaps ( referenceDT, 0 )
        |> group
        |> List.map sumList2
        |> List.reverse


timeSeriesRD : List ( Posix, Float ) -> List ( Int, ( Posix, Float ) )
timeSeriesRD listOfPairs =
    List.map augmentPair listOfPairs


augmentPair : ( Posix, Float ) -> ( Int, ( Posix, Float ) )
augmentPair ( p, f ) =
    ( DateTime.rataDieFromPosix p, ( p, f ) )



-- https://www.revolvy.com/page/Rata-Die
-- https://www.revolvy.com/page/Julian-day?cr=1
-- RD = floor( JD − 1 721 424.5 )
-- correctTimeZone : Int -> List Event -> List Event
-- correctTimeZone timeZoneOffset list =
--     list
--         |> List.map (\r -> offsetTimeZone timeZoneOffset r)
--
-- offsetTi©meZone : Int -> Event -> Event
-- offsetTimeZone offset event =
--     let
--         (NaiveDateTime str) =
--             event.insertedAt
--     in
--     { event | insertedAt = NaiveDateTime (DateTime.offsetDateTimeStringByHours offset str) }


timeSeries : List Event -> List ( Posix, Float )
timeSeries eventList =
    eventList
        |> List.map (\event -> ( event.insertedAt, event.duration |> TypedTime.convertToSeconds ))


filterValues : List ( a, Maybe b ) -> List ( a, b )
filterValues =
    List.foldr foldrValues []


foldrValues : ( a, Maybe b ) -> List ( a, b ) -> List ( a, b )
foldrValues pair list =
    case pair of
        ( _, Nothing ) ->
            list

        ( item, Just v ) ->
            ( item, v ) :: list


group : List ( a, b ) -> List (List ( a, b ))
group list =
    list
        |> LE.groupWhile (\x y -> Tuple.first x == Tuple.first y)
        |> List.map (\( u, v ) -> u :: v)


fillGaps : a -> List ( Int, a ) -> List ( Int, a )
fillGaps default list =
    let
        fillGap start end =
            List.range (start + 1) (end - 1)
                |> List.map (\i -> ( i, default ))

        ii : Int
        ii =
            Maybe.map Tuple.first (List.head list) |> Maybe.withDefault 0
    in
    List.foldl
        (\(( i, a ) as item) ( last, acc ) ->
            if i == last then
                ( last, item :: acc )

            else if i == last + 1 then
                ( last + 1, item :: acc )

            else
                ( i, item :: fillGap last i ++ acc )
        )
        ( ii, [] )
        list
        |> Tuple.second
        |> List.reverse


sumList2 :
    List ( Int, ( Posix, Float ) )
    -> Event
sumList2 list =
    let
        head =
            List.head list

        referenceDT =
            Time.millisToPosix 0

        index : Int
        index =
            Maybe.map Tuple.first head |> Maybe.withDefault -1

        tuple =
            Maybe.map Tuple.second head

        dt : Posix
        dt =
            Maybe.map Tuple.first tuple |> Maybe.withDefault referenceDT

        sum : Float
        sum =
            List.map (Tuple.second >> Tuple.second) list |> List.sum
    in
    { id = index + 1
    , note = "--"
    , duration = TypedTime Seconds sum
    , insertedAt = dt
    }


sumList : List ( Int, Float ) -> ( Maybe Int, Float )
sumList list =
    let
        sum =
            list |> List.map Tuple.second |> List.sum

        index =
            List.head list |> Maybe.map Tuple.first
    in
    ( index, sum )



--
--
--


replaceLog : Log -> List Log -> List Log
replaceLog log listLogs =
    LE.setIf (\item -> item.id == log.id) log listLogs


deleteEvent : Log -> Int -> Log
deleteEvent log eventId =
    let
        newData =
            List.filter (\event -> event.id /= eventId) log.data
    in
    { log | data = newData }


insertEvent : String -> TypedTime -> Posix -> Log -> Log
insertEvent note duration currentTime log =
    let
        newEvent =
            { note = note
            , id = log.counter + 1
            , duration = duration
            , insertedAt = currentTime
            }
    in
    { log | data = newEvent :: log.data, counter = log.counter + 1 }


updateEvent : String -> TypedTime -> Event -> Log -> Log
updateEvent note duration event log =
    let
        newEvent =
            { event | note = note, duration = duration }

        newData =
            LE.setIf (\e -> e.id == event.id) newEvent log.data
    in
    { log | data = newData }
