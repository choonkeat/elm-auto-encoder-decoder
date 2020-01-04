module Foo.Bar exposing (..)

{-|

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    optionValue : Option Int
    optionValue =
        None

    encodeOption Json.Encode.int optionValue
        |> Json.Encode.encode 0
        |> Json.Decode.decodeString (decodeOption Json.Decode.int)
    --> Ok optionValue

    helloValue : Hello Int
    helloValue =
        Good "Morning" (Ok (Just "5.45"))

    encodeHello Json.Encode.int helloValue
        |> Json.Encode.encode 0
        |> Json.Decode.decodeString (decodeHello Json.Decode.int)
    --> Ok helloValue

    personValue : Person
    personValue =
        { name = "Foo", age = 42 }

    encodePerson personValue
        |> Json.Encode.encode 0
        |> Json.Decode.decodeString decodePerson
    --> Ok personValue

    payloadValue : Payload
    payloadValue =
        { title = "Hello", author = { name = "Foo", age = 42 } }

    encodePayload payloadValue
        |> Json.Encode.encode 0
        |> Json.Decode.decodeString decodePayload
    --> Ok payloadValue

-}

import Dict exposing (Dict)
import Main exposing (Msg)


type Option a
    = None
    | Some a


type alias Choice =
    Option Bool


type Hello x
    = Hello
    | Good String (Result x (Maybe String))


type alias Person =
    { name : String
    , age : Int
    }


type alias Payload =
    { -- title : { h1 : String, h2 : String }
      title : String
    , author : Person
    }


type alias Lookup =
    Dict String Int


type Msg
    = Noop
    | Changes (String -> Int -> String)
