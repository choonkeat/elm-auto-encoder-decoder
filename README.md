# elm-auto-encoder-decoder

Generate automatic encoder and decoder. Leveraged by [choonkeat/elm-webapp](https://github.com/choonkeat/elm-webapp#readme) `Client` and `Server` instances to pass `Msg` to and fro.

## Usage

```
npx elm-auto-encoder-decoder src/Types.elm
```

any time `src/Types.elm` file changes, `src/Types/Auto.elm` will be regenerated.

## ENV variable configurations

`GENERATED_SRC` will write `Auto.elm` files to a different directory than the source files, while keeping the directory hierarchy. e.g. `GENERATED_SRC=generated/src`

`WATCHING` when set to `false` will exit after a single run, instead of hanging around waiting for file changes. e.g. `WATCHING=false`

`EXTRA_IMPORT` is written to all `Auto.elm` files if present, e.g. `EXTRA_IMPORT='Time.Extra excposing (..)'`

## How it works

An Elm module `Foo.Bar` like below

``` elm
module Foo.Bar exposing (..)

type alias Choice =
    Option Bool


type Option a
    = None
    | Some a
```

will be encoded and decoded as JSON like


```json
["Foo.Bar.None"]
["Foo.Bar.Some",true]
```

by the generated Elm functions in the corresponding [`Foo.Bar.Auto` module function](https://github.com/choonkeat/elm-auto-encoder-decoder/blob/21a3cd4d46e8bbdd418a3069fb5b79f6efd6a1be/src/Foo/Bar/Auto.elm#L242-L246)

### Auto encoder decoders of related types

If `src/A.elm` imports from `src/B.elm`, its better to provide both filenames to the cli. The generated encoder/decoder of `A` will automatically reference the generated encoder/decoder of `B`

### Exposed types

No encoder/decoder functions will be generated for types that are not fully exposed. i.e. a custom type has to be exposed with `(..)` e.g. `MyType(..)`.

However, if there are exposed types that are composed from unexposed types, you're expected to supply the encoder/decoder functions manually by naming convention (see next point)

### Don't be alarmed with "I cannot find ... variable" compiler errors

If your `src/Types.elm` contain types imported from other modules, e.g. `Time.Posix`

```elm
type alias User = { name : String, createdAt : Time.Posix }
```

the generated code will likely not compile

```
-- NAMING ERROR --------------------------------------------- src/Types/Auto.elm

I cannot find a `decodeTimePosix` variable:

248|         |> Json.Decode.map2 (|>) (Json.Decode.at [ "createdAt" ] (decodeTimePosix))
                                                                       ^^^^^^^^^^^^^^^
-- NAMING ERROR --------------------------------------------- src/Types/Auto.elm

I cannot find a `encodeTimePosix` variable:

197|         , ("createdAt", (encodeTimePosix) value.createdAt)
                              ^^^^^^^^^^^^^^^
```

**just implement and expose those function names** in your source `src/Types.elm` (or a different file specified by `EXTRA_IMPORT` env), for example:

``` elm
decodeTimePosix : Json.Decode.Decoder Time.Posix
decodeTimePosix =
    Json.Decode.int
        |> Json.Decode.map Time.millisToPosix


encodeTimePosix : Time.Posix -> Json.Encode.Value
encodeTimePosix t =
    Json.Encode.int (Time.posixToMillis t)
```
