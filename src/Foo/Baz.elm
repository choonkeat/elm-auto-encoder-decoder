module Foo.Baz exposing
    ( Record
    , Transparent(..)
    )


type alias Record =
    { title : String
    }


type Transparent
    = Transparent Int
