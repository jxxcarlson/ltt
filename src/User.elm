module User exposing (User, Username, addNewUser, validateUser)


type alias Username =
    String


type alias User =
    { username : Username
    , encryptedPassword : String
    }


encrypt : String -> String
encrypt str =
    "!@" ++ String.reverse str ++ "@!"


validatePassword : String -> String -> Bool
validatePassword password encryptedPassword =
    encrypt password == encryptedPassword


validateUser userList username passWord =
    case List.filter (\user -> username == user.username) userList of
        [] ->
            False

        [ user ] ->
            validatePassword passWord user.encryptedPassword

        _ ->
            False


addNewUser : String -> String -> List User -> Maybe ( User, List User )
addNewUser username password userList =
    case ( usernameExists username userList, passwordErrors password ) of
        ( False, [] ) ->
            Just (addNewUser_ username password userList)

        _ ->
            Nothing


addNewUser_ : String -> String -> List User -> ( User, List User )
addNewUser_ username password userList =
    let
        newUser =
            { username = username, encryptedPassword = encrypt password }
    in
    ( newUser, newUser :: userList )


usernameExists : String -> List User -> Bool
usernameExists username userList =
    (List.filter (\user -> user.username == username) userList |> List.length) > 0


passwordErrors : String -> List String
passwordErrors str =
    []
