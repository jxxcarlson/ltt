module User exposing (UserDict, UserInfo, Username, addNewUser, getData, validateUser)

import Dict exposing (Dict)


type alias Username =
    String


type alias UserInfo a =
    { encryptedPassword : String, data : List a }


type alias UserDict a =
    Dict Username (UserInfo a)


getData : Username -> UserDict a -> Maybe (List a)
getData username dict =
    Dict.get username dict
        |> Maybe.map .data


encrypt : String -> String
encrypt str =
    "!@" ++ String.reverse str ++ "@!"


validatePassword : String -> String -> Bool
validatePassword password encryptedPassword =
    encrypt password == encryptedPassword


validateUser : UserDict a -> Username -> String -> Bool
validateUser userDict username passWord =
    case Dict.get username userDict of
        Nothing ->
            False

        Just userInfo ->
            validatePassword passWord userInfo.encryptedPassword


addNewUser : String -> String -> UserDict a -> Maybe (UserDict a)
addNewUser username password userDict =
    case ( Dict.member username userDict, passwordErrors password ) of
        ( False, [] ) ->
            Just <| Dict.insert username { encryptedPassword = encrypt password, data = [] } userDict

        _ ->
            Nothing


passwordErrors : String -> List String
passwordErrors str =
    []
