module TestData exposing (passwordDict, userDict)

import Dict
import Log exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)
import User exposing (PasswordDict, UserDict, UserInfo)


passwordDict : PasswordDict
passwordDict =
    Dict.fromList
        [ ( "jxxcarlson", "!@oboLocol@!" )
        , ( "socrates", "!@citpeks@!" )
        ]


userDict : UserDict Log
userDict =
    Dict.fromList
        [ ( "jxxcarlson", userInfo1 )
        , ( "socrates", userInfo2 )
        ]


userInfo1 : UserInfo Log
userInfo1 =
    { email = "jxxcarlson@gmail.com", admin = True, data = [ log1, log2 ] }


userInfo2 : UserInfo Log
userInfo2 =
    { email = "socrates@philosophers.org", admin = False, data = [] }


e1 : Event
e1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 11.4
    , insertedAt = Time.millisToPosix 1563424248000
    }


e2 : Event
e2 =
    { id = 2
    , note = "--"
    , duration = TypedTime Minutes 4.1
    , insertedAt = Time.millisToPosix 1563337848000
    }


f1 : Event
f1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 44.1
    , insertedAt = Time.millisToPosix 1563424248000
    }


f2 : Event
f2 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 200.1
    , insertedAt = Time.millisToPosix 1563337848000
    }


log1 =
    { id = 1
    , counter = 2
    , name = "Piano practice"
    , note = "Practice for recital"
    , username = "jxxcarlson"
    , data = [ e1, e2 ]
    }


log2 =
    { id = 2
    , counter = 2
    , name = "Elm projects"
    , note = "Get ready for conference"
    , username = "jxxcarlson"
    , data = [ f1, f2 ]
    }
