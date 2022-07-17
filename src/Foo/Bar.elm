module Foo.Bar exposing
    ( Acknowledgement
    , Choice
    , Coordinates(..)
    , CountryCode(..)
    , Custom_word(..)
    , Hello(..)
    , Lookup
    , Option(..)
    , Payload
    , Person
    , ProtectedCustom
    , WithTypeVariable
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Foo.Baz exposing (Record, Transparent(..))
import Json.Decode
import Json.Encode
import Main exposing (Msg)


{-| All tests in one place because we seem to be encountering <https://github.com/stoeffel/elm-verify-examples/issues/88>

    import Json.Encode
    import Json.Decode
    import Foo.Bar.Auto exposing (..)


    inputsOption : List (Option Int)
    inputsOption =
        [ None
        , Some 99
        ]

    inputsOption
        |> List.map (encodeFooBarOption Json.Encode.int)
        |> List.map (Json.Decode.decodeValue (decodeFooBarOption Json.Decode.int))
    --> List.map Ok inputsOption


    inputsHello : List (Hello String)
    inputsHello =
        [ Hello
        , Good "good string" (Err None)
        , Good "good string" (Err (Some "oops"))
        , Good "good string" (Ok (Just "okay"))
        ]

    inputsHello
        |> List.map (encodeFooBarHello Json.Encode.string)
        |> List.map (Json.Decode.decodeValue (decodeFooBarHello Json.Decode.string))
    --> List.map Ok inputsHello


    inputsPerson : List Person
    inputsPerson =
        [ { name = "Foo"
          , age = 42
          }
        ]

    inputsPerson
        |> List.map (encodeFooBarPerson)
        |> List.map (Json.Decode.decodeValue decodeFooBarPerson)
    --> List.map Ok inputsPerson


    inputsPayload : List Payload
    inputsPayload =
        [ { title = "Hello"
          , author_person = { name = "Foo", age = 42 }
          , comments = Just "lgtm 👍"
          , blob = Json.Encode.int 42
          , blob2 = Json.Encode.bool False
          , custom_word = Bespoke_sentence
          }
        ]

    inputsPayload
        |> List.map (encodeFooBarPayload)
        |> List.map (Json.Decode.decodeValue decodeFooBarPayload)
    --> List.map Ok inputsPayload


    inputsAcknowledgement : List (Acknowledgement Int)
    inputsAcknowledgement =
        [ Err 42
        , Ok ()
        ]

    inputsAcknowledgement
        |> List.map (encodeFooBarAcknowledgement Json.Encode.int)
        |> List.map (Json.Decode.decodeValue (decodeFooBarAcknowledgement Json.Decode.int))
    --> List.map Ok inputsAcknowledgement


    inputsWithTypeVariable : List (WithTypeVariable Bool)
    inputsWithTypeVariable =
        [ { meta = Ok
                { title = "titlestring"
                , author_person =
                    { name = "person name"
                    , age = 42
                    }
                , comments = Just "comments string"
                , blob = (Json.Encode.int 111)
                , blob2 = (Json.Encode.int 222)
                , custom_word = Traditional_phrase
                }
          , data = True
          }
        , { meta = Err Hello
          , data = True
          }
        , { meta = Err (Good "One" (Err None))
          , data = True
          }
        , { meta = Err (Good "One" (Err (Some True)))
          , data = True
          }
        , { meta = Err (Good "Two" (Ok (Just "Three")))
          , data = True
          }
        ]

    inputsWithTypeVariable
        |> List.map (encodeFooBarWithTypeVariable Json.Encode.bool)
        |> List.map (Json.Decode.decodeValue (decodeFooBarWithTypeVariable Json.Decode.bool))
    --> List.map Ok inputsWithTypeVariable


    inputsCoordinates : List Coordinates
    inputsCoordinates =
        [ LatLng ( 3.14, 1 )
        , Address ( "Elm Street", 9, AC )
        ]

    inputsCoordinates
        |> List.map (encodeFooBarCoordinates)
        |> List.map (Json.Decode.decodeValue decodeFooBarCoordinates)
    --> List.map Ok inputsCoordinates

-}
type Option a
    = None
    | Some a


type alias Choice =
    Option Bool


type Hello x
    = Hello
    | Good String (Result (Option x) (Maybe String))


type alias Person =
    { name : String
    , age : Int
    }


type alias Payload =
    { title : String
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
    Dict String (Array Foo.Baz.Record)



-- There will not be an Auto encoder/decoder for this type since it contains a function


type Msg
    = Noop
    | Changes (String -> Int -> String)


type alias Acknowledgement x =
    Result x ()


type PrivateCustom
    = PrivateCustom Int


type ProtectedCustom
    = ProtectedCustom Int


type alias WithTypeVariable a =
    { meta : Result (Hello a) Payload
    , data : a
    }


type Coordinates
    = LatLng ( Float, Float )
    | Address ( String, Int, CountryCode )


type CountryCode
    = AA
    | AB
    | AC
    | ZZ
