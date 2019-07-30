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
    { email = "jxxcarlson@gmail.com", admin = True, counter = 2, data = [ log1, log2 ] }


userInfo2 : UserInfo Log
userInfo2 =
    { email = "socrates@philosophers.org", admin = False, counter = 0, data = [] }


e1 : Event
e1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 11.4
    , insertedAt = Time.millisToPosix 1563424248000
    , selected = True
    }


e2 : Event
e2 =
    { id = 2
    , note = "--"
    , duration = TypedTime Minutes 4.1
    , insertedAt = Time.millisToPosix 1563337848000
    , selected = True
    }


f1 : Event
f1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 44.1
    , insertedAt = Time.millisToPosix 1563424248000
    , selected = True
    }


f2 : Event
f2 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 200.1
    , insertedAt = Time.millisToPosix 1563337848000
    , selected = True
    }


log1 =
    { id = 1
    , counter = 2
    , name = "Piano practice"
    , note = "Practice for recital"
    , username = "jxxcarlson"
    , data = [ e1, e2 ]
    , selected = True
    }


log2 =
    { id = 2
    , counter = 2
    , name = "Elm projects"
    , note = "Get ready for conference"
    , username = "jxxcarlson"
    , data = [ f1, f2 ]
    , selected = True
    }
