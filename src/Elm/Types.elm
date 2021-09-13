module Elm.Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias ElmFile =
    { modulePrefix : String
    , exposing_ : Exposing
    , imports : Set String -- ["Json.Decode"]
    , importExposing : Dict String Exposing -- Dict.fromList [("Json.Decode", ExposingEverything)]
    , preludeResolver : Dict String String -- Dict.fromList [("string", "Json.Encode.string")]
    , importResolver : Dict String String -- Dict.fromList [("string", "Json.Encode.string")]
    , knownTypes : Dict String ElmTypeDef -- Dict.fromList [("Json.Encode.Value", ...)]
    , skipTypes : Set String
    }



-- HEADERS


{-| module Elm.Types.Parser exposing (..)
-}
type ModuleDef
    = ModuleDef TitleCaseDotPhrase Exposing


{-| `Elm.Types.Parser`
-}
type TitleCaseDotPhrase
    = TitleCaseDotPhrase String


{-| (..) or ((|.), (|=), Parser)
-}
type Exposing
    = ExposingEverything
    | ExposingOnly (List String)


{-| refers to entire `import Json.Decoder as D exposing (Value)`
-}
type ImportDef
    = ImportDef TitleCaseDotPhrase (Maybe String) Exposing



-- TYPES


type ElmTypeDef
    = CustomTypeDef CustomType
    | TypeAliasDef TypeAlias


{-| refers to `Person x` in `type Person x = { username : x }`
-}
type TypeName
    = TypeName String (List String)



-- a) TYPE ALIAS


{-| refers to entire `type alias Person x = { result : Result x a, name : String }`
-}
type TypeAlias
    = AliasRecordType TypeName (List FieldPair)
    | AliasCustomType TypeName CustomTypeConstructor


{-| refers to `result : Result x a` or `name : String` of `{ result : Result x a, name : String }`
-}
type FieldPair
    = CustomField FieldName CustomTypeConstructor
    | NestedField FieldName (List FieldPair)


type FieldName
    = FieldName String



-- b) CUSTOM TYPE


{-| refers to entire `type Person x = Anonymous | LoggedIn x`
-}
type alias CustomType =
    { name : TypeName
    , constructors : List CustomTypeConstructor
    }


type CustomTypeConstructor
    = CustomTypeConstructor TitleCaseDotPhrase (List CustomTypeConstructor)
    | ConstructorTypeParam String
    | Tuple2 CustomTypeConstructor CustomTypeConstructor
    | Tuple3 CustomTypeConstructor CustomTypeConstructor CustomTypeConstructor
    | Function CustomTypeConstructor CustomTypeConstructor
