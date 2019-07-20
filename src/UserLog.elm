module UserLog exposing (create, update)

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
