module TestData exposing (e1, e2, f1, f2, log1, log2, user1, user2, userDict)

import Dict
import Log exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)
import User exposing (UserDict)


userDict : UserDict Log
userDict =
    Dict.fromList
        [ ( "jxxcarlson", { encryptedPassword = "!@oboLocol@!", data = [ log1, log2 ] } )
        , ( "socrates", { encryptedPassword = "!@citpeks@!", data = [] } )
        ]


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


user1 =
    { username = "jxxcarlson"
    , encryptedPassword = "!@oboLocol@!"
    }


user2 =
    { username = "socrates"
    , encryptedPassword = "!@citpeks@!"
    }
