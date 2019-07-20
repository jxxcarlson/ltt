module User exposing (Password, PasswordDict, UserDict, UserInfo, Username, add, fromDict, getData, validateUser)

import Dict exposing (Dict)


type alias Username =
    String


type alias Password =
    String


type alias PasswordDict =
    Dict Username Password


type alias UserDict a =
    Dict Username (UserInfo a)


type alias UserInfo a =
    { email : String, admin : Bool, data : List a }


type alias User =
    { username : Username, email : String, admin : Bool }


fromDict : Username -> UserDict a -> Maybe User
fromDict username userDict =
    case Dict.get username userDict of
        Nothing ->
            Nothing

        Just userInfo ->
            Just { username = username, email = userInfo.email, admin = userInfo.admin }


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


validateUser : PasswordDict -> Username -> String -> Bool
validateUser passwordDict username passWord =
    case Dict.get username passwordDict of
        Nothing ->
            False

        Just encryptedPassword ->
            validatePassword passWord encryptedPassword


add : String -> String -> String -> ( PasswordDict, UserDict a ) -> Result String ( PasswordDict, UserDict a )
add username password email ( passwordDict, userDict ) =
    case ( Dict.member username userDict, passwordErrors password ) of
        ( False, [] ) ->
            let
                newPasswordDict =
                    Dict.insert username (encrypt password) passwordDict

                newUserInfo =
                    { email = email, admin = False, data = [] }

                newUserDict =
                    Dict.insert username newUserInfo userDict
            in
            Ok ( newPasswordDict, newUserDict )

        _ ->
            Err "user already exists"


passwordErrors : String -> List String
passwordErrors str =
    []
