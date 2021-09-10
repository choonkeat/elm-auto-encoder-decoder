module Elm.Types.Parser exposing (..)

import Dict exposing (Dict)
import Elm.Types exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run titleCaseWord "Elm.Types.Parser a b"
    --> Ok ("Elm")

    Parser.run titleCaseWord "elm.Types.Parser a b"
    --> Err [{ col = 1, problem = Parser.UnexpectedChar, row = 1 }]

-}
titleCaseWord =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile isNameCharacter
        |> Parser.getChompedString


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run titleCaseDotPhrase "Elm.Types.Parser a b"
    --> Ok (TitleCaseDotPhrase "Elm.Types.Parser")

-}
titleCaseDotPhrase : Parser.Parser TitleCaseDotPhrase
titleCaseDotPhrase =
    let
        ifProgress acc =
            Parser.oneOf
                [ Parser.succeed (\s -> Parser.Loop (s :: acc))
                    |= Parser.oneOf
                        [ titleCaseWord
                        , Parser.symbol "." |> Parser.map (always ".")
                        ]
                , Parser.succeed (Parser.Done (List.reverse acc))
                ]

        loopedParser =
            Parser.loop [] ifProgress
    in
    Parser.succeed (\s list -> TitleCaseDotPhrase (s ++ String.join "" list))
        |= titleCaseWord
        |= loopedParser


{-| Parsing the section after `module X exposing` or `import X exposing`

    import Parser
    import Elm.Types exposing (..)

    Parser.run namedExports "(..)"
    --> Ok ExposingEverything

    Parser.run namedExports "((=>), world)"
    --> Ok (ExposingOnly ["=>", "world"])

    Parser.run namedExports "(hello, (=>))"
    --> Ok (ExposingOnly ["hello", "=>"])

    Parser.run namedExports "(hello world)"
    --> Err [{ col = 8, problem = Parser.Expecting ",", row = 1 },{ col = 8, problem = Parser.Expecting ")", row = 1 }]

    Parser.run namedExports "hello world"
    --> Err [{ col = 1, problem = Parser.Expecting "(", row = 1 }]

-}
namedExports : Parser.Parser Exposing
namedExports =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = namedExport
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\list ->
                case list of
                    [ ".." ] ->
                        ExposingEverything

                    _ ->
                        ExposingOnly list
            )


{-| Parsing a named export; removes parenthesis

    import Parser
    import Elm.Types exposing (..)

    Parser.run namedExport "(=>), world"
    --> Ok "=>"

    Parser.run namedExport "hello world"
    --> Ok "hello"

-}
namedExport : Parser.Parser String
namedExport =
    let
        wordInParen =
            Parser.chompWhile (isNoneOf [ ' ', ',', ')', '(' ])
    in
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.symbol "("
            |. Parser.chompUntilEndOr ")"
            |. Parser.symbol ")"
            |> Parser.getChompedString
            |> Parser.map (String.dropLeft 1 >> String.dropRight 1)
        , Parser.backtrackable wordInParen
            |. Parser.symbol "(..)"
            |> Parser.getChompedString
        , wordInParen |> Parser.getChompedString
        ]


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run moduleDefinition "module Elm.Types.Parser exposing (..)"
    --> Ok (ModuleDef (TitleCaseDotPhrase "Elm.Types.Parser") ExposingEverything)

    Parser.run moduleDefinition "module Elm.Types.Parser exposing (fieldName, moduleDefinition)"
    --> Ok (ModuleDef (TitleCaseDotPhrase "Elm.Types.Parser") (ExposingOnly ["fieldName", "moduleDefinition"]))

    Parser.run moduleDefinition "module Parser exposing ((|.), (|=), Parser, Step(..))"
    --> Ok (ModuleDef (TitleCaseDotPhrase "Parser") (ExposingOnly ["|.", "|=", "Parser", "Step(..)"]))

-}
moduleDefinition : Parser.Parser ModuleDef
moduleDefinition =
    Parser.succeed ModuleDef
        |. Parser.keyword "module"
        |. Parser.symbol " "
        |= titleCaseDotPhrase
        |. Parser.symbol " "
        |. Parser.keyword "exposing"
        |. Parser.spaces
        |= namedExports


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run importDefinition "import Elm.Types.Parser exposing (..)"
    --> Ok (ImportDef (TitleCaseDotPhrase "Elm.Types.Parser") Nothing ExposingEverything)

    Parser.run importDefinition "import Elm.Types.Parser as X exposing (hello)"
    --> Ok (ImportDef (TitleCaseDotPhrase "Elm.Types.Parser") (Just "X") (ExposingOnly ["hello"]))

    Parser.run importDefinition "import Elm.Types.Parser as X"
    --> Ok (ImportDef (TitleCaseDotPhrase "Elm.Types.Parser") (Just "X") (ExposingOnly []))

    Parser.run importDefinition "import Elm.Types.Parser"
    --> Ok (ImportDef (TitleCaseDotPhrase "Elm.Types.Parser") Nothing (ExposingOnly []))

-}
importDefinition : Parser.Parser ImportDef
importDefinition =
    Parser.succeed ImportDef
        |. Parser.keyword "import"
        |. Parser.symbol " "
        |= titleCaseDotPhrase
        |= Parser.oneOf
            [ Parser.backtrackable <|
                Parser.succeed Just
                    |. Parser.symbol " as "
                    |= titleCaseWord
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol " exposing "
                |= namedExports
            , Parser.succeed (ExposingOnly [])
            ]


nameFromCustomType : CustomType -> TypeName
nameFromCustomType { name } =
    name


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run typeName "Alpha.Beta.Charlie a b | Dude"
    --> Ok (TypeName "Alpha.Beta.Charlie" [ "a", "b" ])

-}
typeName : Parser.Parser TypeName
typeName =
    Parser.succeed TypeName
        |= (titleCaseDotPhrase |> Parser.map (\(TitleCaseDotPhrase s) -> s))
        |. Parser.spaces
        |= (Parser.chompWhile (isNoneOf [ '\n', '\u{000D}', ',', '=', '}', '|' ])
                |> Parser.getChompedString
                |> Parser.map String.trim
                |> Parser.map
                    (\s ->
                        if s == "" then
                            []

                        else
                            String.words s
                    )
           )


{-| Refers to the `x` or `a` in `Result x a`
-}
constructorTypeParam : Parser.Parser CustomTypeConstructor
constructorTypeParam =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile (\c -> Char.isAlpha c && Char.isLower c)
        |> Parser.getChompedString
        |> Parser.map ConstructorTypeParam


{-| Refers to the entire `Result x a`, thus must begin with `titleCaseDotPhrase`

    import Parser
    import Elm.Types exposing (..)

    aString : CustomTypeConstructor
    aString =
        CustomTypeConstructor (TitleCaseDotPhrase "String") []

    Parser.run nestedTypeName "String"
    --> Ok aString

    maybeString : CustomTypeConstructor
    maybeString =
        CustomTypeConstructor (TitleCaseDotPhrase "Maybe") [ aString ]

    Parser.run nestedTypeName "Maybe String"
    --> Ok maybeString

    Parser.run nestedTypeName "List (Maybe String)"
    --> Ok (CustomTypeConstructor (TitleCaseDotPhrase "List") [ maybeString ])

    resultXInt : CustomTypeConstructor
    resultXInt =
        CustomTypeConstructor (TitleCaseDotPhrase "Result")
            [ ConstructorTypeParam "x", CustomTypeConstructor (TitleCaseDotPhrase "Int") [] ]

    Parser.run nestedTypeName "Result x Int"
    --> Ok resultXInt

    Parser.run nestedTypeName "Maybe (Result x Int)"
    --> Ok (CustomTypeConstructor (TitleCaseDotPhrase "Maybe") [ resultXInt ])

    Parser.run nestedTypeName "Dict x (Result x Int)"
    --> Ok (CustomTypeConstructor (TitleCaseDotPhrase "Dict") [ ConstructorTypeParam "x", resultXInt ])

    modelMsgModel : CustomTypeConstructor
    modelMsgModel =
        Function
            (CustomTypeConstructor (TitleCaseDotPhrase "Model") [])
            (Function
                (CustomTypeConstructor (TitleCaseDotPhrase "Msg") [])
                (CustomTypeConstructor (TitleCaseDotPhrase "Model") [])
            )

    Parser.run nestedTypeName "Model -> Msg -> Model"
    --> Ok (modelMsgModel)

-}
nestedTypeName : Parser.Parser CustomTypeConstructor
nestedTypeName =
    Parser.succeed CustomTypeConstructor
        |= titleCaseDotPhrase
        |. Parser.oneOf
            [ Parser.symbol " "
            , Parser.succeed ()
            ]
        |= restOfCustomTypeConstructor
        |> Parser.map functionFromCustomTypeConstructor


functionArrow =
    ConstructorTypeParam "->"


functionFromCustomTypeConstructor : CustomTypeConstructor -> CustomTypeConstructor
functionFromCustomTypeConstructor ct =
    let
        toFunction name acc rest =
            case rest of
                (ConstructorTypeParam "->") :: (CustomTypeConstructor name2 list2) :: xs ->
                    Function (CustomTypeConstructor name acc) (toFunction name2 list2 xs)

                (ConstructorTypeParam "->") :: x :: [] ->
                    Function (CustomTypeConstructor name acc) x

                [ ConstructorTypeParam "->" ] ->
                    CustomTypeConstructor name acc

                x :: xs ->
                    toFunction name (List.append acc [ x ]) xs

                [] ->
                    CustomTypeConstructor name acc
    in
    case ct of
        CustomTypeConstructor name list ->
            toFunction name [] list

        ConstructorTypeParam _ ->
            ct

        Tuple2 ct1 ct2 ->
            Tuple2 (functionFromCustomTypeConstructor ct1) (functionFromCustomTypeConstructor ct2)

        Tuple3 ct1 ct2 ct3 ->
            Tuple3 (functionFromCustomTypeConstructor ct1) (functionFromCustomTypeConstructor ct2) (functionFromCustomTypeConstructor ct3)

        Function ct1 ct2 ->
            Function (functionFromCustomTypeConstructor ct1) (functionFromCustomTypeConstructor ct2)


{-| Refers to the whole or any of valid token `Result x (Maybe a)`

  - `Result x (Maybe a)`
  - `x`
  - `Maybe a`
  - `a`

-}
anyCustomTypeConstructor : Parser.Parser CustomTypeConstructor
anyCustomTypeConstructor =
    Parser.oneOf
        [ Parser.succeed (CustomTypeConstructor (TitleCaseDotPhrase "()") [])
            |. Parser.symbol "()"
        , Parser.backtrackable (parenthesised tupleParams)
        , parenthesised nestedTypeName
        , Parser.succeed (\s -> CustomTypeConstructor s [])
            |= titleCaseDotPhrase
        , constructorTypeParam
        ]


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run restOfCustomTypeConstructor "-> Msg -> Model"
    --> Ok [ConstructorTypeParam "->",CustomTypeConstructor (TitleCaseDotPhrase "Msg") [],ConstructorTypeParam "->",CustomTypeConstructor (TitleCaseDotPhrase "Model") []]

-}
restOfCustomTypeConstructor : Parser.Parser (List CustomTypeConstructor)
restOfCustomTypeConstructor =
    let
        restOfCustomTypeConstructorHelp revList =
            Parser.oneOf
                [ Parser.succeed (Parser.Loop (functionArrow :: revList))
                    |. Parser.symbol "-> "
                , Parser.succeed (\token -> Parser.Loop (token :: revList))
                    |= anyCustomTypeConstructor
                    |. Parser.oneOf [ Parser.symbol " ", Parser.symbol "\n", Parser.end ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] restOfCustomTypeConstructorHelp


{-| Refers to `a, b` of `Coordinate (a, b)`
-}
tupleParams : Parser.Parser CustomTypeConstructor
tupleParams =
    let
        tupleParamsHelp revList =
            Parser.oneOf
                [ Parser.succeed (\token -> Parser.Loop (token :: revList))
                    |= anyCustomTypeConstructor
                    |. Parser.oneOf
                        [ Parser.succeed ()
                            |. Parser.symbol ", "
                        , Parser.succeed ()
                        ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] tupleParamsHelp
        |> Parser.andThen
            (\list ->
                case list of
                    [ a, b ] ->
                        Parser.succeed (Tuple2 a b)

                    [ a, b, c ] ->
                        Parser.succeed (Tuple3 a b c)

                    _ ->
                        Parser.problem ("Bad tuple: " ++ String.fromInt (List.length list) ++ " items")
            )


parenthesised : Parser.Parser a -> Parser.Parser a
parenthesised parser =
    Parser.multiComment "(" ")" Parser.Nestable
        |> Parser.getChompedString
        |> Parser.map (\s -> String.dropRight 1 (String.dropLeft 1 s))
        |> Parser.andThen
            (\s ->
                case Parser.run parser s of
                    Ok value ->
                        Parser.succeed value

                    Err err ->
                        Parser.problem (Debug.toString err)
            )


customTypeConstructorList : Parser.Parser (List CustomTypeConstructor)
customTypeConstructorList =
    let
        customTypeConstructorListHelp revList =
            Parser.oneOf
                [ Parser.succeed (\s -> Parser.Loop (s :: revList))
                    |. comments
                    |= nestedTypeName
                    |. comments
                    |. Parser.oneOf [ Parser.symbol "|", Parser.succeed () ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] customTypeConstructorListHelp


{-|

    import Parser
    import Elm.Types exposing (..)

    orderCustomType : CustomType
    orderCustomType =
        CustomType
            (TypeName "Order" [])
            [ CustomTypeConstructor (TitleCaseDotPhrase "LT") []
            , CustomTypeConstructor (TitleCaseDotPhrase "EQ") []
            , CustomTypeConstructor (TitleCaseDotPhrase "GT") []
            ]

    Parser.run customType (String.trim ("""
        type Order
            = LT
            | EQ
              -- interrupting comment
            | GT
    """))
    --> Ok orderCustomType

    nameFromCustomType orderCustomType
    --> TypeName "Order" []

    maybeCustomType : CustomType
    maybeCustomType =
        CustomType
            (TypeName "Maybe" [ "a" ])
            [ CustomTypeConstructor (TitleCaseDotPhrase "Nothing") []
            , CustomTypeConstructor (TitleCaseDotPhrase "Just") [ ConstructorTypeParam "a" ]
            ]

    Parser.run customType (String.trim ("""
        type Maybe a
            = Nothing
            | Just a
    """))
    --> Ok maybeCustomType

    nameFromCustomType maybeCustomType
    --> TypeName "Maybe" [ "a" ]

    dictCustomType : CustomType
    dictCustomType =
        CustomType
            (TypeName "Dict" [ "a", "b" ])
            [ CustomTypeConstructor (TitleCaseDotPhrase "Dict")
                [ CustomTypeConstructor (TitleCaseDotPhrase "Set")
                    [ Tuple2 (ConstructorTypeParam "a") (ConstructorTypeParam "b") ]
                ]
            ]

    Parser.run customType (String.trim ("""
        type Dict a b = Dict (Set (a, b))
    """))
    --> Ok dictCustomType

    nameFromCustomType dictCustomType
    --> TypeName "Dict" [ "a", "b" ]

-}
customType : Parser.Parser CustomType
customType =
    Parser.succeed CustomType
        |. Parser.keyword "type"
        |. comments
        |= typeName
        |. comments
        |. Parser.symbol "="
        |. comments
        |= customTypeConstructorList


nameFromTypeAlias : TypeAlias -> TypeName
nameFromTypeAlias t =
    case t of
        AliasRecordType name _ ->
            name

        AliasCustomType name t2 ->
            name


fieldName : Parser.Parser FieldName
fieldName =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile isNameCharacter
        |> Parser.getChompedString
        |> Parser.map FieldName


isNameCharacter : Char -> Bool
isNameCharacter c =
    c == '_' || Char.isAlphaNum c


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run fieldPair "userID : String"
    --> Ok (CustomField (FieldName "userID") (CustomTypeConstructor (TitleCaseDotPhrase "String") []))

-}
fieldPair : Parser.Parser FieldPair
fieldPair =
    Parser.oneOf
        [ Parser.backtrackable <|
            Parser.succeed CustomField
                |= fieldName
                |. Parser.symbol " : "
                |= nestedTypeName
        , Parser.succeed NestedField
            |= fieldName
            |. Parser.symbol " : "
            |= Parser.lazy (\() -> fieldPairList)
        ]


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run fieldPairList ("""{
          userID : String
        , email : Email
    }""")
    --> Ok [ CustomField (FieldName "userID") (CustomTypeConstructor (TitleCaseDotPhrase "String") []), CustomField (FieldName "email") (CustomTypeConstructor (TitleCaseDotPhrase "Email") []) ]

    Parser.run fieldPairList ("""{ userID : String }""")
    --> Ok [ CustomField (FieldName "userID") (CustomTypeConstructor (TitleCaseDotPhrase "String") []) ]

-}
fieldPairList : Parser.Parser (List FieldPair)
fieldPairList =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = comments
        , item = fieldPair
        , trailing = Parser.Forbidden
        }


aliasRecordType : Parser.Parser TypeAlias
aliasRecordType =
    Parser.succeed AliasRecordType
        |. Parser.keyword "type"
        |. comments
        |. Parser.keyword "alias"
        |. comments
        |= typeName
        |. comments
        |. Parser.symbol "="
        |. comments
        |= fieldPairList


aliasCustomType : Parser.Parser TypeAlias
aliasCustomType =
    Parser.succeed AliasCustomType
        |. Parser.keyword "type"
        |. comments
        |. Parser.keyword "alias"
        |. comments
        |= typeName
        |. comments
        |. Parser.symbol "="
        |. comments
        |= Parser.andThen
            (\list ->
                case list of
                    [ ct ] ->
                        Parser.succeed ct

                    _ ->
                        Parser.problem "Bad type alias"
            )
            customTypeConstructorList


{-|

    import Parser
    import Elm.Types exposing (..)

    Parser.run typeAlias ("""type alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok (AliasRecordType (TypeName "User" []) [ CustomField (FieldName "userID") (CustomTypeConstructor (TitleCaseDotPhrase "String") []), CustomField (FieldName "email") (CustomTypeConstructor (TitleCaseDotPhrase "Email") []) ])

    Parser.run typeAlias ("""type alias User = String""")
    --> Ok (AliasCustomType (TypeName "User" []) (CustomTypeConstructor (TitleCaseDotPhrase "String") []))

    Parser.run typeAlias ("""type alias User = Dict String (Result x Int)""")
    --> Ok (AliasCustomType (TypeName "User" []) (CustomTypeConstructor (TitleCaseDotPhrase "Dict") [CustomTypeConstructor (TitleCaseDotPhrase "String") [],CustomTypeConstructor (TitleCaseDotPhrase "Result") [ConstructorTypeParam "x",CustomTypeConstructor (TitleCaseDotPhrase "Int") []]]))

-}
typeAlias : Parser.Parser TypeAlias
typeAlias =
    Parser.oneOf
        [ Parser.backtrackable aliasRecordType
        , aliasCustomType
        ]


nameFromElmTypeDef : ElmTypeDef -> TypeName
nameFromElmTypeDef elmTypeDef =
    case elmTypeDef of
        CustomTypeDef t ->
            nameFromCustomType t

        TypeAliasDef t ->
            nameFromTypeAlias t


{-|

    import Parser
    import Elm.Types exposing (..)

    orderCustomType : CustomType
    orderCustomType =
        CustomType
            (TypeName "Order" [])
            [ CustomTypeConstructor (TitleCaseDotPhrase "LT") []
            , CustomTypeConstructor (TitleCaseDotPhrase "EQ") []
            , CustomTypeConstructor (TitleCaseDotPhrase "GT") []
            ]

    boolCustomType : CustomType
    boolCustomType =
        CustomType
            (TypeName "Bool" [])
            [ CustomTypeConstructor (TitleCaseDotPhrase "True") []
            , CustomTypeConstructor (TitleCaseDotPhrase "False") []
            ]

    userTypeAlias : TypeAlias
    userTypeAlias =
        AliasRecordType
            (TypeName "User" [])
            [ CustomField (FieldName "userID") (CustomTypeConstructor (TitleCaseDotPhrase "String") [])
            , CustomField (FieldName "email") (CustomTypeConstructor (TitleCaseDotPhrase "Email") [])
            ]

    Parser.run elmTypeDefList (String.trim ("""
        type Order
            = LT
            | EQ
            | GT
    """))
    --> Ok [ CustomTypeDef orderCustomType ]

    Parser.run elmTypeDefList (String.trim ("""
        type Bool
            = True
            | False
    """))
    --> Ok [ CustomTypeDef boolCustomType]

    Parser.run elmTypeDefList (String.trim ("""
        type Order
            = LT
            | EQ
            | GT\n\ntype Bool
            = True
            | False
    """))
    --> Ok [ CustomTypeDef orderCustomType, CustomTypeDef boolCustomType]

    Parser.run elmTypeDefList ("""type alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok [TypeAliasDef userTypeAlias]


    Parser.run elmTypeDefList ("""type Order
            = LT
            | EQ
            | GT\n\ntype alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok [CustomTypeDef orderCustomType, TypeAliasDef userTypeAlias]

-}
elmTypeDefList : Parser.Parser (List ElmTypeDef)
elmTypeDefList =
    let
        elmTypeDefListHelp revList =
            Parser.oneOf
                [ Parser.succeed (\s -> Parser.Loop (TypeAliasDef s :: revList))
                    |= Parser.backtrackable typeAlias
                , Parser.succeed (\s -> Parser.Loop (CustomTypeDef s :: revList))
                    |= customType
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] elmTypeDefListHelp


problemIfEmpty : List a -> Parser (List a)
problemIfEmpty list =
    case list of
        [] ->
            Parser.problem "empty"

        _ ->
            Parser.succeed list


parentModuleName : String -> String
parentModuleName =
    String.split "." >> List.reverse >> List.drop 1 >> List.reverse >> String.join "."


prefixIfMissing : Set String -> String -> String -> Maybe String -> Maybe String
prefixIfMissing fileImports modulePrefix phrase maybeExist =
    case ( maybeExist, Set.member (parentModuleName phrase) fileImports ) of
        ( Nothing, False ) ->
            Just (modulePrefix ++ phrase)

        _ ->
            maybeExist


addReferencedTypesFieldPair : Set String -> String -> FieldPair -> Dict String String -> Dict String String
addReferencedTypesFieldPair fileImports modulePrefix pair dict =
    case pair of
        CustomField _ ct ->
            addReferencedCustomTypeConstructor fileImports modulePrefix ct dict

        NestedField _ list ->
            List.foldl (addReferencedTypesFieldPair fileImports modulePrefix) dict list


addReferencedCustomTypeConstructor : Set String -> String -> CustomTypeConstructor -> Dict String String -> Dict String String
addReferencedCustomTypeConstructor fileImports modulePrefix ct acc =
    case ct of
        CustomTypeConstructor (TitleCaseDotPhrase name) tokens ->
            acc
                |> Dict.update name (prefixIfMissing fileImports modulePrefix name)

        ConstructorTypeParam name ->
            acc
                |> Dict.update name (prefixIfMissing fileImports modulePrefix name)

        Tuple2 type1 type2 ->
            acc
                |> addReferencedCustomTypeConstructor fileImports modulePrefix type1
                |> addReferencedCustomTypeConstructor fileImports modulePrefix type2

        Tuple3 type1 type2 type3 ->
            acc
                |> addReferencedCustomTypeConstructor fileImports modulePrefix type1
                |> addReferencedCustomTypeConstructor fileImports modulePrefix type2
                |> addReferencedCustomTypeConstructor fileImports modulePrefix type3

        Function argType returnType ->
            acc
                |> addReferencedCustomTypeConstructor fileImports modulePrefix argType
                |> addReferencedCustomTypeConstructor fileImports modulePrefix returnType


addReferencedTypes : String -> ElmTypeDef -> ElmFile -> ElmFile
addReferencedTypes modulePrefix elmTypeDef file =
    case elmTypeDef of
        CustomTypeDef { constructors } ->
            let
                newImportResolver =
                    List.foldl (addReferencedCustomTypeConstructor file.imports modulePrefix)
                        file.importResolver
                        constructors
            in
            { file | importResolver = newImportResolver }

        TypeAliasDef (AliasRecordType _ fieldPairs) ->
            let
                newImportResolver =
                    List.foldl (addReferencedTypesFieldPair file.imports modulePrefix)
                        file.importResolver
                        fieldPairs
            in
            { file | importResolver = newImportResolver }

        TypeAliasDef (AliasCustomType tname ct) ->
            addReferencedTypes modulePrefix (CustomTypeDef { name = tname, constructors = [ ct ] }) file


addElmTypeDef : ElmTypeDef -> ElmFile -> ElmFile
addElmTypeDef elmTypeDef file =
    let
        (TypeName shortName typeParams) =
            nameFromElmTypeDef elmTypeDef

        absoluteName =
            file.modulePrefix ++ shortName

        newElmFile =
            addReferencedTypes
                file.modulePrefix
                elmTypeDef
                { file | importResolver = Dict.insert shortName absoluteName file.importResolver }
    in
    if Set.member shortName file.skipTypes then
        file

    else
        { newElmFile | knownTypes = Dict.insert shortName elmTypeDef file.knownTypes }


addModuleDef : ElmFile -> ModuleDef -> String -> ElmFile
addModuleDef file (ModuleDef (TitleCaseDotPhrase name) exposing_) moduleComments =
    let
        newSkipTypes =
            if String.startsWith "{- noauto " moduleComments then
                moduleComments
                    |> String.dropLeft 10
                    |> String.dropRight 3
                    |> String.split ", "
                    |> Set.fromList
                    |> Set.union file.skipTypes

            else
                file.skipTypes
    in
    { file | modulePrefix = name ++ ".", skipTypes = newSkipTypes }


addImportDef : ElmFile -> ImportDef -> ElmFile
addImportDef file (ImportDef (TitleCaseDotPhrase name) maybeAliasName exposing_) =
    let
        newImportResolver =
            -- Dict.insert name name
            file.importResolver

        importResolverWithMaybeAliasName =
            Maybe.map (\aliasName -> Dict.insert aliasName name newImportResolver) maybeAliasName
                |> Maybe.withDefault newImportResolver
    in
    case exposing_ of
        ExposingEverything ->
            { file
                | imports = Set.insert name file.imports
                , importExposing = Dict.insert name exposing_ file.importExposing
                , importResolver = importResolverWithMaybeAliasName
            }

        ExposingOnly [] ->
            { file
                | imports = Set.insert name file.imports
                , importResolver = importResolverWithMaybeAliasName
            }

        ExposingOnly (x :: xs) ->
            let
                namespaced =
                    if List.any (not << isNameCharacter) (String.toList x) then
                        -- for infix operators, restore the parenthesis
                        name ++ "." ++ "(" ++ x ++ ")"

                    else
                        name ++ "." ++ x

                newFile =
                    { file
                        | importResolver = Dict.insert x namespaced newImportResolver
                        , importExposing = Dict.insert x exposing_ file.importExposing
                    }
            in
            addImportDef newFile (ImportDef (TitleCaseDotPhrase name) maybeAliasName (ExposingOnly xs))


fileContent : Parser.Parser ElmFile
fileContent =
    let
        fileContentHelp currentFile =
            Parser.oneOf
                [ Parser.succeed (List.foldl addElmTypeDef currentFile >> Parser.Loop)
                    |. comments
                    |= Parser.andThen problemIfEmpty elmTypeDefList
                    |. comments
                , Parser.succeed (\m c -> Parser.Loop (addModuleDef currentFile m c))
                    |= moduleDefinition
                    |= (comments |> Parser.getChompedString |> Parser.map String.trim)
                , Parser.succeed (addImportDef currentFile >> Parser.Loop)
                    |= importDefinition
                    |. comments
                , Parser.succeed (Parser.Done { currentFile | knownTypes = qualifyKnownTypes currentFile.importResolver currentFile.knownTypes })
                    |. Parser.end
                , Parser.succeed (Parser.Loop currentFile)
                    |. Parser.chompUntilEndOr "\n"
                    |. comments
                ]
    in
    Parser.loop
        { modulePrefix = ""
        , imports = Set.empty
        , importExposing = Dict.empty
        , importResolver =
            Dict.fromList
                [ -- (givenString, qualifiedString)
                  ( "String", "String" )
                , ( "String.String", "String" )
                , ( "Set", "Set.Set" )
                , ( "Dict", "Dict.Dict" )
                , ( "Never", "Never" )
                , ( "Result", "Result" )
                , ( "Maybe", "Maybe" )
                , ( "Maybe.Maybe", "Maybe" )
                , ( "Array", "Array" )
                , ( "Char", "Char" )
                , ( "List", "List" )
                , ( "List.List", "List" )
                , ( "Int", "Int" )
                , ( "Float", "Float" )
                , ( "Bool", "Bool" )
                , ( "ProcessId", "Platform.ProcessId" )
                , ( "Task", "Platform.Task" )
                , ( "Tuple", "Tuple" )
                ]
        , knownTypes = Dict.empty
        , skipTypes = Set.empty
        }
        fileContentHelp


{-| <https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#lineComment>
<https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#multiComment>
-}
comments : Parser ()
comments =
    let
        ifProgress parser offset =
            Parser.succeed identity
                |. parser
                |= Parser.getOffset
                |> Parser.map
                    (\newOffset ->
                        if offset == newOffset then
                            Parser.Done ()

                        else
                            Parser.Loop newOffset
                    )
    in
    Parser.loop 0 <|
        ifProgress <|
            Parser.oneOf
                [ Parser.lineComment "--"
                , Parser.multiComment "{-" "-}" Parser.Nestable
                , Parser.spaces
                ]


isNoneOf : List Char -> Char -> Bool
isNoneOf characters char =
    not (List.member char characters)


{-|

    isTypeParameter "msg"
    --> True

    isTypeParameter "Msg"
    --> False

    isTypeParameter ""
    --> False

-}
isTypeParameter : String -> Bool
isTypeParameter phrase =
    case String.uncons phrase of
        Just ( c, xs ) ->
            not (Char.isUpper c)

        Nothing ->
            False


containFunctionElmTypeDef : ElmTypeDef -> Bool
containFunctionElmTypeDef elmTypeDef =
    case elmTypeDef of
        CustomTypeDef ct ->
            containFunctionCustomType ct

        TypeAliasDef ta ->
            containFunctionTypeAlias ta


containFunctionTypeAlias : TypeAlias -> Bool
containFunctionTypeAlias ta =
    case ta of
        AliasRecordType _ list ->
            List.any containFunctionFieldPair list

        AliasCustomType _ ct ->
            containFunctionCustomTypeConstructor ct


containFunctionCustomType : CustomType -> Bool
containFunctionCustomType { constructors } =
    List.any containFunctionCustomTypeConstructor constructors


containFunctionFieldPair : FieldPair -> Bool
containFunctionFieldPair pair =
    case pair of
        CustomField _ ct ->
            containFunctionCustomTypeConstructor ct

        NestedField _ list ->
            List.any containFunctionFieldPair list


containFunctionCustomTypeConstructor : CustomTypeConstructor -> Bool
containFunctionCustomTypeConstructor ct =
    case ct of
        CustomTypeConstructor _ list ->
            List.any containFunctionCustomTypeConstructor list

        ConstructorTypeParam s ->
            False

        Tuple2 t1 t2 ->
            containFunctionCustomTypeConstructor t1
                || containFunctionCustomTypeConstructor t2

        Tuple3 t1 t2 t3 ->
            containFunctionCustomTypeConstructor t1
                || containFunctionCustomTypeConstructor t2
                || containFunctionCustomTypeConstructor t3

        Function argType returnType ->
            True



-- FULLY QUALIFIED WITH MODULE NAME


qualifyName : Dict String String -> String -> String
qualifyName dict string =
    Maybe.withDefault string (Dict.get string dict)


qualifyConstructor : Dict String String -> CustomTypeConstructor -> CustomTypeConstructor
qualifyConstructor dict ct =
    case ct of
        CustomTypeConstructor (TitleCaseDotPhrase name) tokens ->
            CustomTypeConstructor (TitleCaseDotPhrase (qualifyName dict name))
                (List.map (qualifyConstructor dict) tokens)

        ConstructorTypeParam name ->
            ConstructorTypeParam (qualifyName dict name)

        Tuple2 type1 type2 ->
            Tuple2 (qualifyConstructor dict type1) (qualifyConstructor dict type2)

        Tuple3 type1 type2 type3 ->
            Tuple3 (qualifyConstructor dict type1) (qualifyConstructor dict type2) (qualifyConstructor dict type3)

        Function argType returnType ->
            Function (qualifyConstructor dict argType) (qualifyConstructor dict returnType)


qualifyFieldPair : Dict String String -> FieldPair -> FieldPair
qualifyFieldPair dict fieldpair =
    case fieldpair of
        CustomField fname ct ->
            CustomField fname (qualifyConstructor dict ct)

        NestedField fname list ->
            NestedField fname (List.map (qualifyFieldPair dict) list)


qualifyCustomType : Dict String String -> CustomType -> CustomType
qualifyCustomType dict { name, constructors } =
    let
        (TypeName tname typeParams) =
            name

        newConstructors =
            List.map (qualifyConstructor dict) constructors
    in
    { name = TypeName (qualifyName dict tname) typeParams, constructors = newConstructors }


qualifyElmTypeDef : Dict String String -> ElmTypeDef -> ElmTypeDef
qualifyElmTypeDef dict elmTypeDef =
    case elmTypeDef of
        CustomTypeDef t ->
            CustomTypeDef (qualifyCustomType dict t)

        TypeAliasDef (AliasRecordType (TypeName tname typeParams) fieldPairs) ->
            TypeAliasDef (AliasRecordType (TypeName (qualifyName dict tname) typeParams) (List.map (qualifyFieldPair dict) fieldPairs))

        TypeAliasDef (AliasCustomType (TypeName tname typeParams) ct) ->
            TypeAliasDef (AliasCustomType (TypeName (qualifyName dict tname) typeParams) (qualifyConstructor dict ct))


qualifyKnownTypes : Dict String String -> Dict String ElmTypeDef -> Dict String ElmTypeDef
qualifyKnownTypes dict knownTypes =
    Dict.map (\k v -> qualifyElmTypeDef dict v) knownTypes
