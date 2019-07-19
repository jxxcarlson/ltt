module DateTime exposing
    ( NaiveDateTime(..)
    , dateStringOfDateTimeString
    , isoStringFromNaiveDateTime
    , naiveDateStringFromPosix
    , naiveDateTimeFromPosix
    , naiveDateTimeValue
    , naiveTimeStringFromPosix
    , timeStringOfDateTimeString
    )

-- import Date exposing (Date, Unit(..), diff)

import Time exposing (Posix)


type NaiveDateTime
    = NaiveDateTime String


type alias TR =
    { hour : Int
    , minute : Int
    , second : Int
    }


naiveDateTimeValue : NaiveDateTime -> String
naiveDateTimeValue (NaiveDateTime str) =
    str


offsetTRByHours : Int -> TR -> TR
offsetTRByHours h tt =
    let
        newHours =
            tt.hour + h
    in
    if newHours >= 24 then
        { tt | hour = newHours - 24 }

    else if newHours > 0 then
        { tt | hour = newHours }

    else
        { tt | hour = newHours + 24 }


stringValueOfTR : TR -> String
stringValueOfTR tr =
    let
        h =
            String.padLeft 2 '0' <| String.fromInt tr.hour

        min =
            String.padLeft 2 '0' <| String.fromInt tr.minute

        s =
            String.padLeft 2 '0' <| String.fromInt tr.second
    in
    String.join ":" [ h, min, s ]


strToInt : String -> Int
strToInt str =
    str
        |> String.toInt
        |> Maybe.withDefault 0


isoStringFromNaiveDateTime : String -> Maybe String
isoStringFromNaiveDateTime str =
    String.split "T" str
        |> List.head


naiveDateStringFromPosix : Posix -> String
naiveDateStringFromPosix posix =
    let
        y =
            Time.toYear Time.utc posix |> String.fromInt

        m =
            Time.toMonth Time.utc posix |> monthToString

        d =
            Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    y ++ "-" ++ m ++ "-" ++ d


naiveTimeStringFromPosix : Posix -> String
naiveTimeStringFromPosix posix =
    let
        h =
            Time.toHour Time.utc posix |> String.fromInt |> String.padLeft 2 '0'

        m =
            Time.toMinute Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    h ++ ":" ++ m


naiveDateTimeFromPosix : Posix -> NaiveDateTime
naiveDateTimeFromPosix posix =
    NaiveDateTime (naiveDateStringFromPosix posix)


monthToString : Time.Month -> String
monthToString m =
    case m of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


timeStringOfDateTimeString : String -> String
timeStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "-"


dateStringOfDateTimeString : String -> String
dateStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.head
                |> Maybe.withDefault "-"
