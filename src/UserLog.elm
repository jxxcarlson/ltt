module UserLog exposing (create, deleteEvent, update)

import Dict
import Log exposing (Log)
import User exposing (UserDict, UserInfo, Username)


create : Username -> Log -> UserDict Log -> UserDict Log
create username log userDict =
    case Dict.get username userDict of
        Nothing ->
            userDict

        Just userInfo ->
            let
                newLog =
                    { log | id = List.length userInfo.data + 1 }

                updater : Maybe (UserInfo Log) -> Maybe (UserInfo Log)
                updater =
                    Maybe.map (\uInfo -> { uInfo | data = log :: uInfo.data })
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



-- let
--     maybeTargetLog : Maybe Log
--     maybeTargetLog =
--         List.filter (\log -> log.id == logId) userInfo.data
--             |> List.head
--
--     maybeChangedData =
--         Maybe.map (List.filter (\event -> event.id /= eventId)) (Maybe.map .data maybeTargetLog)
--
--     maybeChangedLog =
--
--     updater : Log -> Maybe (UserInfo Log) -> Maybe (UserInfo Log)
--     updater changedLog_ =
--         Maybe.map (\uInfo -> { uInfo | data = Log.replaceLog changedLog_ uInfo.data })
-- in
-- -- case maybeChangedLog of
--     Nothing ->
--         ( model, Cmd.none )
--
--     Just changedLog ->
--         ( { model | userDict = Dict.update username (updater changedLog) model.userDict }, Cmd.none )
