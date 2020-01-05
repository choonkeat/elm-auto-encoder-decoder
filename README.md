# Parser for Elm types

Enough to generate automatic encoder and decoder

# Usage

```
npx elm-auto-encoder-decoder src/Types.elm
```

any time `src/Types.elm` file changes, `src/Types/Auto.elm` will be regenerated.

# Intention

Given the content of a `Foo.Bar` Elm module

``` elm
module Foo.Bar exposing (..)

type alias Choice =
    Option Bool


type Option a
    = None
    | Some a
```

`Elm.Types.AutoEncoder.produceSourceCode` will produce the Elm functions that encode and decode the types in that file under the `Auto` submodule

``` elm
module Foo.Bar.Auto exposing (..)

import Foo.Bar
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


encodeFooBarChoice : Foo.Bar.Choice -> Json.Encode.Value
encodeFooBarChoice value =
    (encodeFooBarOption (encodeBool)) value


decodeFooBarChoice : Json.Decode.Decoder (Foo.Bar.Choice)
decodeFooBarChoice  =
    (decodeFooBarOption (decodeBool))


encodeFooBarOption : (a -> Json.Encode.Value) -> Foo.Bar.Option a -> Json.Encode.Value
encodeFooBarOption arga value =
    case value of
        (Foo.Bar.None) -> (Json.Encode.list identity [ encodeString "Foo.Bar.None" ])
        (Foo.Bar.Some m0) -> (Json.Encode.list identity [ encodeString "Foo.Bar.Some", (arga m0) ])


decodeFooBarOption : (Json.Decode.Decoder (a)) -> Json.Decode.Decoder (Foo.Bar.Option a)
decodeFooBarOption arga =
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Foo.Bar.None" -> (Json.Decode.succeed Foo.Bar.None)
                    "Foo.Bar.Some" -> (Json.Decode.succeed Foo.Bar.Some |> (Json.Decode.Pipeline.custom (Json.Decode.index 1 (arga))))
                    _ -> Json.Decode.fail ("Unexpected Foo.Bar.Option: " ++ word)
            )
```
