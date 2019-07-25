module UserLog exposing (UserStats, clean, compile, create, delete, deleteEvent, update)

import Dict exposing (Dict)
import Log exposing (Log)
import TypedTime exposing (TypedTime)
import User exposing (UserDict, UserInfo, Username)


create : Username -> Log -> UserDict Log -> UserDict Log
create username log userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newLog =
                    { log | id = userInfo.counter + 1 }

                updater : Maybe (UserInfo Log) -> Maybe (UserInfo Log)
                updater =
                    Maybe.map (\uInfo -> { uInfo | counter = uInfo.counter + 1, data = newLog :: uInfo.data })
            in
            Dict.update username updater userDict


update : Username -> Log -> UserDict Log -> UserDict Log
update username log userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newLogs =
                    Log.replaceLog log userInfo.data

                newUserInfo =
                    { userInfo | data = newLogs }
            in
            Dict.update username (\x -> Just newUserInfo) userDict


delete : Username -> Log -> UserDict Log -> UserDict Log
delete username log userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newLogs =
                    List.filter (\lg -> lg.id /= log.id) userInfo.data

                newUserInfo =
                    { userInfo | data = newLogs }
            in
            Dict.update username (\x -> Just newUserInfo) userDict


clean : Username -> UserDict Log -> UserDict Log
clean username userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newData =
                    List.filter (\log -> log.id > 0) userInfo.data

                newUserInfo =
                    { userInfo | data = newData }
            in
            Dict.update username (\x -> Just newUserInfo) userDict


deleteEvent : Username -> Log -> UserDict Log -> Int -> UserDict Log
deleteEvent username log userDict eventId =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newLog =
                    Log.deleteEvent log eventId
            in
            update username newLog userDict



--
-- USER STATS
--


type alias UserStat =
    { numberOfLogs : Int
    , numberOfEvents : Int
    }


numberOfEvents_ : Username -> UserDict Log -> Int
numberOfEvents_ username userDict =
    let
        logs =
            Maybe.map .data (Dict.get username userDict) |> Maybe.withDefault []

        folder =
            \log acc -> acc + List.length log.data
    in
    List.foldl folder 0 logs


userStat : Username -> UserDict Log -> UserStat
userStat username userDict =
    case Dict.get username userDict of
        Nothing ->
            { numberOfLogs = 0, numberOfEvents = 0 }

        Just userInfo ->
            { numberOfLogs = List.length userInfo.data
            , numberOfEvents = numberOfEvents_ username userDict
            }


type alias UserStats =
    Dict Username UserStat


compile : UserDict Log -> UserStats
compile userDict =
    let
        folder =
            \uname dict -> Dict.insert uname (userStat uname userDict) dict
    in
    List.foldl folder Dict.empty (Dict.keys userDict)
