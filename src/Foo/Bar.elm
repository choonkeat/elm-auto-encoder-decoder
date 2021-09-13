module Foo.Bar exposing
    ( Choice, Option(..)
    , Acknowledgement, Custom_word(..), Hello(..), Lookup, Payload, Person, ProtectedCustom
    )

{-| add docs to make elm-format break up module expose into different lines

@docs Choice, Option

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)

    optionValue : Option Int
    optionValue =
        None

    encodeFooBarOption Json.Encode.int optionValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded as"
        |> Json.Decode.decodeString (decodeFooBarOption Json.Decode.int)
    --> Ok optionValue

    helloValue : Hello Int
    helloValue =
        Good "Morning" (Ok (Just "5.45"))

    encodeFooBarHello Json.Encode.int helloValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded as"
        |> Json.Decode.decodeString (decodeFooBarHello Json.Decode.int)
    --> Ok helloValue

    personValue : Person
    personValue =
        { name = "Foo", age = 42 }

    encodeFooBarPerson personValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded as"
        |> Json.Decode.decodeString decodeFooBarPerson
    --> Ok personValue

    payloadValue : Payload
    payloadValue =
        { title = "Hello"
        , author_person = { name = "Foo", age = 42 }
        , comments = Just "lgtm 👍"
        , blob = Json.Encode.int 42
        , blob2 = Json.Encode.bool False
        , custom_word = Bespoke_sentence
        }

    encodeFooBarPayload payloadValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded as"
        |> Json.Decode.decodeString decodeFooBarPayload
    --> Ok payloadValue

    acknowledgementValue : Acknowledgement Int
    acknowledgementValue =
        Ok ()

    encodeFooBarAcknowledgement Json.Encode.int acknowledgementValue
        |> Json.Encode.encode 0
        |> Debug.log "encoded as"
        |> Json.Decode.decodeString (decodeFooBarAcknowledgement Json.Decode.int)
    --> Ok acknowledgementValue

-}

import Dict exposing (Dict)
import Foo.Baz
import Json.Decode
import Json.Encode
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
    , author_person : Person
    , comments : Maybe String
    , blob : Json.Encode.Value
    , blob2 : Json.Decode.Value
    , custom_word : Custom_word
    }


type Custom_word
    = Traditional_phrase
    | Bespoke_sentence


type alias Lookup =
    Dict String Foo.Baz.Record


{-| There will not be an Auto encoder/decoder for this type since it contains a function
-}
type Msg
    = Noop
    | Changes (String -> Int -> String)


type alias Acknowledgement x =
    Result x ()


type PrivateCustom
    = PrivateCustom Int


type ProtectedCustom
    = ProtectedCustom Int
