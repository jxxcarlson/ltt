module UserLog exposing (add)

import Dict
import Log exposing (Log)
import User exposing (UserDict, UserInfo, Username)


add : Username -> Log -> UserDict Log -> UserDict Log
add username log userDict =
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
