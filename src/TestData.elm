module TestData exposing (e1, e2, f1, f2, log, log2)

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


type alias Log =
    { id : Int
    , counter : Int
    , name : String
    , note : String
    , userId : Int
    , data : List Event
    }


log =
    { id = 1
    , counter = 2
    , name = "Piano practice"
    , note = "Practice for recital"
    , userId = 1
    , data = [ e1, e2 ]
    }


log2 =
    { id = 2
    , counter = 2
    , name = "Elm projects"
    , note = "Get ready for conference"
    , userId = 1
    , data = [ f1, f2 ]
    }
