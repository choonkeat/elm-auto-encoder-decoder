module Foo.Bar exposing (..)

{-|

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    optionValue : Option Int
    optionValue =
        None

    encodeFooBarOption Json.Encode.int optionValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded Option"
        |> Json.Decode.decodeString (decodeFooBarOption Json.Decode.int)
    --> Ok optionValue

-}

import Dict exposing (Dict)


type Option a
    = None
    | Some a


type alias Choice =
    Option Bool


{-|

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    helloValue : Hello Int
    helloValue =
        Good "Morning" 5.45

    encodeFooBarHello Json.Encode.int helloValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded Hello"
        |> Json.Decode.decodeString (decodeFooBarHello Json.Decode.int)
    --> Ok helloValue

-}
type Hello x
    = Hello
    | Good String String


{-|

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    personValue : Person
    personValue =
        { name = "Foo", age = 42 }

    encodeFooBarPerson personValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded Person"
        |> Json.Decode.decodeString decodeFooBarPerson
    --> Ok personValue

-}
type alias Person =
    { name : String
    , age : Int
    }


{-|

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    payloadValue : Payload
    payloadValue =
        { title = "Hello", author = { name = "Foo", age = 42 } }

    encodeFooBarPayload payloadValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded Payload"
        |> Json.Decode.decodeString decodeFooBarPayload
    --> Ok payloadValue

-}
type alias Payload =
    { title : String
    , author : Person
    }


{-|

    import Dict
    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    lookupValue : Lookup
    lookupValue =
        Dict.fromList [ ("one", 1), ("two", 2) ]

    encodeFooBarLookup lookupValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded Lookup"
        |> Json.Decode.decodeString decodeFooBarLookup
    --> Ok lookupValue

-}
type alias Lookup =
    Dict String Int
