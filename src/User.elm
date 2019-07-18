module User exposing (User, UserName, encrypt, validatePassword, validateUser)


type alias UserName =
    String


type alias User =
    { userName : UserName
    , encryptedPassword : String
    }


encrypt : String -> String
encrypt str =
    "!@" ++ String.reverse str ++ "@!"


validatePassword : String -> String -> Bool
validatePassword password encryptedPassword =
    encrypt password == encryptedPassword


validateUser userList userName passWord =
    case List.filter (\user -> userName == user.userName) userList of
        [] ->
            False

        [ user ] ->
            validatePassword passWord user.encryptedPassword

        _ ->
            False
