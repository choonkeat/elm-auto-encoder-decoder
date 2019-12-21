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

type Option a = None | Some a

type alias Choice =
    Option Bool
```

`Elm.Types.AutoEncoder.produceSourceCode` will produce the Elm functions that encode and decode the types in that file under the `Auto` submodule

``` elm
module Foo.Bar.Auto exposing (..)

import Foo.Bar
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


encodeFooBarOption : (a -> Json.Encode.Value) -> Foo.Bar.Option a -> Json.Encode.Value
encodeFooBarOption encodeArga value =
    case value of
        Foo.Bar.Some arg0 ->
            Json.Encode.list identity [ encodeString "Foo.Bar.Some", encodeArga arg0 ]

        Foo.Bar.None ->
            Json.Encode.list identity [ encodeString "Foo.Bar.None" ]


encodeFooBarChoice : Foo.Bar.Choice -> Json.Encode.Value
encodeFooBarChoice value =
    encodeFooBarOption encodeBool value


decodeFooBarOption : Json.Decode.Decoder a -> Json.Decode.Decoder (Foo.Bar.Option a)
decodeFooBarOption decodeArga =
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\word ->
                case word of
                    "Foo.Bar.Some" ->
                        Json.Decode.succeed Foo.Bar.Some
                            |> Json.Decode.Pipeline.custom (Json.Decode.index 1 decodeArga)

                    "Foo.Bar.None" ->
                        Json.Decode.succeed Foo.Bar.None

                    _ ->
                        Json.Decode.fail ("Unexpected Foo.Bar.Option: " ++ word)
            )


decodeFooBarChoice : Json.Decode.Decoder Foo.Bar.Choice
decodeFooBarChoice =
    decodeFooBarOption decodeBool
```

# Avoiding problems

To avoid generating functions of certain types, e.g. `ComplicatedType`, stick a comment under the `module` definition

``` elm
module Foo.Bar exposing (..)

{- noauto ComplicatedType -}
```
