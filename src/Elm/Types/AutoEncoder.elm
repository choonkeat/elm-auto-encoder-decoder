module Elm.Types.AutoEncoder exposing (produceSourceCode)

import Dict exposing (Dict)
import Elm.Types.Parser exposing (..)
import Json.Decode
import Json.Encode
import Parser
import Set exposing (Set)


safeModuleName : String -> String
safeModuleName =
    String.replace "." "" >> String.replace " " ""


funcName : String -> String -> String
funcName prefix word =
    if isTypeParameter word then
        prefix ++ "Arg" ++ safeModuleName word

    else
        prefix ++ safeModuleName word


typeImports : ElmFile -> Set String
typeImports elmFile =
    Dict.foldl
        (\k v acc ->
            if String.contains "." v then
                List.head (List.reverse (String.split " " v))
                    |> Maybe.map parentModuleName
                    |> Maybe.andThen
                        (\s ->
                            if s == "" then
                                Just acc

                            else
                                Just (Set.insert s acc)
                        )
                    |> Maybe.withDefault acc

            else
                acc
        )
        Set.empty
        elmFile.importResolver_


produceSourceCode : ElmFile -> String
produceSourceCode file =
    let
        filteredTypes =
            -- Dict.filter (\k v -> String.startsWith file.modulePrefix k)
            file.knownTypes

        importStatements =
            typeImports file
                |> Set.toList
                |> List.map
                    (\s ->
                        if s ++ "." == file.modulePrefix then
                            "import " ++ s ++ " exposing (..)"

                        else
                            "import " ++ s
                    )
                |> String.join "\n"
    in
    """module """
        ++ file.modulePrefix
        ++ """Auto exposing (..)

-- imports: """
        ++ Debug.toString file.imports
        ++ """
-- importResolver_: """
        ++ Debug.toString file.importResolver_
        ++ """
-- file.knownTypes: """
        ++ Debug.toString file.knownTypes
        ++ """

"""
        ++ importStatements
        ++ """
import Set
import Dict
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


encodeString : String -> Json.Encode.Value
encodeString =
    Json.Encode.string


encodeInt : Int -> Json.Encode.Value
encodeInt =
    Json.Encode.int


encodeFloat : Float -> Json.Encode.Value
encodeFloat =
    Json.Encode.float


encodeBool : Bool -> Json.Encode.Value
encodeBool =
    Json.Encode.bool


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe encodera value =
    Maybe.map encodera value
        |> Maybe.withDefault Json.Encode.null


encodeList : (a -> Json.Encode.Value) -> List a -> Json.Encode.Value
encodeList =
    Json.Encode.list


encodeSetSet : (comparable -> Json.Encode.Value) -> Set.Set comparable -> Json.Encode.Value
encodeSetSet encoder =
    Set.toList >> encodeList encoder


encodeDictDict : (a -> String) -> (b -> Json.Encode.Value) -> (Dict.Dict a b) -> Json.Encode.Value
encodeDictDict =
    Json.Encode.dict


--


decodeString : Json.Decode.Decoder String
decodeString =
    Json.Decode.string


decodeInt : Json.Decode.Decoder Int
decodeInt =
    Json.Decode.int


decodeFloat : Json.Decode.Decoder Float
decodeFloat =
    Json.Decode.float


decodeBool : Json.Decode.Decoder Bool
decodeBool =
    Json.Decode.bool


decodeMaybe : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
decodeMaybe =
    Json.Decode.maybe


decodeList : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeList =
    Json.Decode.list


decodeSetSet : Json.Decode.Decoder comparable -> Json.Decode.Decoder (Set.Set comparable)
decodeSetSet =
    Json.Decode.list >> Json.Decode.map Set.fromList


decodeDictDict : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict.Dict String a)
decodeDictDict =
    Json.Decode.dict


"""
        ++ String.join "\n\n" (Dict.foldl (\k v acc -> produceEncoder file (qualifyType file.importResolver_ v) :: acc) [] filteredTypes)
        ++ "\n\n"
        ++ String.join "\n" (Dict.foldl (\k v acc -> produceDecoder file (qualifyType file.importResolver_ v) :: acc) [] filteredTypes)


stringFromTypeName : TypeName -> String
stringFromTypeName (TypeName fullName typeParams) =
    fullName ++ " " ++ String.join " " (List.map (\(TypeParam word) -> word) typeParams)


produceEncoder : ElmFile -> ElmType -> String
produceEncoder file elmType =
    let
        currentTypeName =
            nameFromElmType elmType

        (TypeName fullName typeParams) =
            currentTypeName

        paramSigs =
            List.append (List.map (\(TypeParam word) -> "(" ++ word ++ " -> Json.Encode.Value)") typeParams) [ stringFromTypeName currentTypeName ]

        params =
            List.append (List.map (\(TypeParam word) -> funcName "encode" word) typeParams) [ "value" ]

        header =
            "\nencode"
                ++ safeModuleName fullName
                ++ """ : """
                ++ String.join " -> " paramSigs
                ++ """ -> Json.Encode.Value"""
                ++ "\nencode"
                ++ safeModuleName fullName
                ++ " "
                ++ String.join " " params
                ++ " =\n    -- "
                ++ Debug.toString elmType
    in
    case encodeElmType file.importResolver_ elmType of
        Ok body ->
            header ++ body

        Err body ->
            "\n{-\n" ++ header ++ body ++ "\n-}"


produceDecoder : ElmFile -> ElmType -> String
produceDecoder file elmType =
    let
        currentTypeName =
            nameFromElmType elmType

        (TypeName fullName typeParams) =
            currentTypeName

        key =
            nameFromElmType elmType

        paramSigs =
            List.append (List.map (\(TypeParam word) -> "Json.Decode.Decoder (" ++ word ++ ")") typeParams) [ "Json.Decode.Decoder (" ++ stringFromTypeName currentTypeName ++ ")" ]

        params =
            List.map (\(TypeParam word) -> funcName "decode" word) typeParams

        header =
            "\ndecode"
                ++ safeModuleName fullName
                ++ """ : """
                ++ String.join " -> " paramSigs
                ++ "\ndecode"
                ++ safeModuleName fullName
                ++ " "
                ++ String.join " " params
                ++ " =\n    -- "
                ++ Debug.toString elmType
    in
    case decodeElmType file.importResolver_ elmType of
        Ok body ->
            header ++ body

        Err body ->
            "\n{-\n" ++ header ++ body ++ "\n-}"


jsonKey : String -> String
jsonKey key =
    Json.Encode.encode 0 (Json.Encode.string key)


encodeElmType : Dict String String -> ElmType -> Result String String
encodeElmType dict elmType =
    case elmType of
        ElmCustomType { name, constructors } ->
            let
                (TypeName typeName typeParams) =
                    name

                ( problem, constructorCases ) =
                    List.map (qualifyCustomTypeConstructor dict) constructors
                        |> List.foldl
                            (\(CustomTypeConstructor s) ( hasError, acc ) ->
                                case String.words s of
                                    x :: xs ->
                                        let
                                            errContainFunc =
                                                -- we cannot encode/decode functions
                                                List.any isFunction xs

                                            matchVars =
                                                List.map (safeModuleName >> String.toLower) xs

                                            first =
                                                x ++ " " ++ String.join " " matchVars ++ " -> Json.Encode.list identity [ encodeString " ++ jsonKey x

                                            rest =
                                                List.map (\param -> funcName "encode" param ++ " " ++ String.toLower (safeModuleName param)) xs
                                        in
                                        ( hasError || errContainFunc, (String.join ", " (first :: rest) ++ "]") :: acc )

                                    [] ->
                                        -- shouldn't happen
                                        ( hasError, acc )
                            )
                            ( False, [] )

                sourceCode =
                    "\n    case value of\n        " ++ String.join "\n        " constructorCases
            in
            if problem then
                Err sourceCode

            else
                Ok sourceCode

        ElmTypeAlias (AliasRecordType (TypeName typeName typeParams) fieldPairs) ->
            let
                sourceCode =
                    "\n    Json.Encode.object ["
                        ++ String.join ", " (List.map (encodeFieldPair "value") fieldPairs)
                        ++ "]"
            in
            Ok sourceCode

        ElmTypeAlias (AliasCustomType { constructors }) ->
            let
                errContainFunc =
                    -- we cannot encode/decode functions
                    List.any (\(CustomTypeConstructor s) -> isFunction s) constructors

                sourceCode =
                    List.foldl
                        (\(CustomTypeConstructor s) acc ->
                            List.append
                                (List.map (funcName "encode") (String.words s))
                                acc
                        )
                        [ "value" ]
                        constructors
                        |> String.join " "
                        |> (++) "\n    "
            in
            if errContainFunc then
                Err sourceCode

            else
                Ok sourceCode


encodeFieldPair : String -> FieldPair -> String
encodeFieldPair varName (FieldPair (FieldName key) (TypeName value typeParams)) =
    let
        encoder =
            funcName "encode" value
                :: List.map (\(TypeParam s) -> funcName "encode" s) typeParams
                |> String.join " "
    in
    "(" ++ jsonKey key ++ ", (" ++ encoder ++ ") " ++ varName ++ "." ++ key ++ ")"


decodeElmType : Dict String String -> ElmType -> Result String String
decodeElmType dict elmType =
    case elmType of
        ElmCustomType { name, constructors } ->
            let
                (TypeName typeName typeParams) =
                    name

                ( problem, constructorCases ) =
                    List.map (qualifyCustomTypeConstructor dict) constructors
                        |> List.foldl
                            (\(CustomTypeConstructor s) ( hasError, acc ) ->
                                case String.words s of
                                    x :: xs ->
                                        let
                                            errContainFunc =
                                                -- we cannot encode/decode functions
                                                List.any isFunction xs

                                            matchVars =
                                                List.map String.toLower xs

                                            first =
                                                jsonKey x ++ " -> Json.Decode.succeed " ++ x

                                            rest =
                                                List.indexedMap
                                                    (\index param ->
                                                        " |> Json.Decode.Pipeline.custom (Json.Decode.index " ++ String.fromInt (index + 1) ++ " " ++ funcName "decode" param ++ ")"
                                                    )
                                                    xs
                                        in
                                        ( hasError || errContainFunc, (first ++ String.join "" rest) :: acc )

                                    [] ->
                                        -- shouldn't happen
                                        ( hasError, acc )
                            )
                            ( False, [] )

                sourceCode =
                    """
    Json.Decode.index 0 Json.Decode.string
        |> Json.Decode.andThen
            (\\word ->
                case word of
                    """ ++ String.join "\n                    " constructorCases ++ """
                    _ ->
                        Json.Decode.fail ("Unexpected """ ++ typeName ++ """: " ++ word)
            )
            """
            in
            if problem then
                Err sourceCode

            else
                Ok sourceCode

        ElmTypeAlias (AliasRecordType (TypeName typeName typeParams) fieldPairs) ->
            let
                sourceCode =
                    "\n    Json.Decode.succeed "
                        ++ typeName
                        ++ " |> "
                        ++ String.join " |> " (List.map decodeFieldPair fieldPairs)
            in
            Ok sourceCode

        ElmTypeAlias (AliasCustomType { constructors }) ->
            let
                errContainFunc =
                    -- we cannot encode/decode functions
                    List.any (\(CustomTypeConstructor s) -> isFunction s) constructors

                sourceCode =
                    List.foldl
                        (\(CustomTypeConstructor s) acc ->
                            List.append
                                (List.map (funcName "decode") (String.words s))
                                acc
                        )
                        []
                        constructors
                        |> String.join " "
                        |> (++) "\n    "
            in
            if errContainFunc then
                Err sourceCode

            else
                Ok sourceCode


decodeFieldPair : FieldPair -> String
decodeFieldPair (FieldPair (FieldName key) (TypeName value typeParams)) =
    let
        decoder =
            funcName "decode" value
                :: List.map (\(TypeParam s) -> funcName "decode" s) typeParams
                |> String.join " "
    in
    "(Json.Decode.Pipeline.custom (Json.Decode.at [" ++ jsonKey key ++ "] (" ++ decoder ++ ")))"
