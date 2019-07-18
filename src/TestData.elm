module TestData exposing (e1, e2, f1, f2, log1, log2, user1, user2)

import DateTime exposing (NaiveDateTime(..))
import Log exposing (..)
import Time exposing (Posix)
import TypedTime exposing (..)


e1 : Event
e1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 11.4
    , insertedAt = Time.millisToPosix 1561993648
    }


e2 : Event
e2 =
    { id = 2
    , note = "--"
    , duration = TypedTime Minutes 4.1
    , insertedAt = Time.millisToPosix 1561993648
    }


f1 : Event
f1 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 44.1
    , insertedAt = Time.millisToPosix 1561993648
    }


f2 : Event
f2 =
    { id = 1
    , note = "--"
    , duration = TypedTime Minutes 200.1
    , insertedAt = Time.millisToPosix 1561993648
    }


log1 =
    { id = 1
    , counter = 2
    , name = "Piano practice"
    , note = "Practice for recital"
    , userName = "jxxcarlson"
    , data = [ e1, e2 ]
    }


log2 =
    { id = 2
    , counter = 2
    , name = "Elm projects"
    , note = "Get ready for conference"
    , userName = "jxxcarlson"
    , data = [ f1, f2 ]
    }


user1 =
    { userName = "jxxcarlson"
    , encryptedPassword = "!@oboLocol@!"
    }


user2 =
    { userName = "socrates"
    , encryptedPassword = "!@citpeks@!"
    }
