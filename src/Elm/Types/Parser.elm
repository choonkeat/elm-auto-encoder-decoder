module Elm.Types.Parser exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


{-| module Elm.Types.Parser exposing (..)
-}
type ModuleDefinition
    = ModuleDefinition TitleCaseDotPhrase Exposing


type TitleCaseDotPhrase
    = TitleCaseDotPhrase String


titleCaseWord =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString


{-|

    import Parser

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


parenList : Parser.Parser (List String)
parenList =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = parenWord
        , trailing = Parser.Forbidden
        }


{-|

    import Parser

    Parser.run parenWord "(=>), world"
    --> Ok "=>"

    Parser.run parenWord "hello world"
    --> Ok "hello"

-}
parenWord : Parser.Parser String
parenWord =
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


type Exposing
    = ExposingEverything
    | ExposingOnly (List String)


{-|

    import Parser

    Parser.run moduleDefinition "module Elm.Types.Parser exposing (..)"
    --> Ok (ModuleDefinition (TitleCaseDotPhrase "Elm.Types.Parser") ExposingEverything)

    Parser.run moduleDefinition "module Elm.Types.Parser exposing (fieldName, moduleDefinition)"
    --> Ok (ModuleDefinition (TitleCaseDotPhrase "Elm.Types.Parser") (ExposingOnly ["fieldName", "moduleDefinition"]))

    Parser.run moduleDefinition "module Parser exposing ((|.), (|=), Parser, Step(..))"
    --> Ok (ModuleDefinition (TitleCaseDotPhrase "Parser") (ExposingOnly ["|.", "|=", "Parser", "Step(..)"]))

-}
moduleDefinition : Parser.Parser ModuleDefinition
moduleDefinition =
    Parser.succeed ModuleDefinition
        |. Parser.keyword "module"
        |. Parser.spaces
        |= titleCaseDotPhrase
        |. Parser.spaces
        |. Parser.keyword "exposing"
        |. Parser.spaces
        |= (parenList
                |> Parser.map
                    (\list ->
                        case list of
                            [ ".." ] ->
                                ExposingEverything

                            _ ->
                                ExposingOnly list
                    )
           )


{-| import Json.Decoder as D exposing (Value)
-}
type ImportDefinition
    = ImportDefinition TitleCaseDotPhrase (Maybe String) Exposing


{-|

    import Parser

    Parser.run importDefinition "import Elm.Types.Parser exposing (..)"
    --> Ok (ImportDefinition (TitleCaseDotPhrase "Elm.Types.Parser") Nothing ExposingEverything)

    Parser.run importDefinition "import Elm.Types.Parser as X exposing (hello)"
    --> Ok (ImportDefinition (TitleCaseDotPhrase "Elm.Types.Parser") (Just "X") (ExposingOnly ["hello"]))

    Parser.run importDefinition "import Elm.Types.Parser as X"
    --> Ok (ImportDefinition (TitleCaseDotPhrase "Elm.Types.Parser") (Just "X") (ExposingOnly []))

    Parser.run importDefinition "import Elm.Types.Parser"
    --> Ok (ImportDefinition (TitleCaseDotPhrase "Elm.Types.Parser") Nothing (ExposingOnly []))

-}
importDefinition : Parser.Parser ImportDefinition
importDefinition =
    Parser.succeed ImportDefinition
        |. Parser.keyword "import"
        |. Parser.spaces
        |= titleCaseDotPhrase
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.keyword "as"
                |. Parser.spaces
                |= titleCaseWord
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.keyword "exposing"
                |. Parser.spaces
                |= (parenList
                        |> Parser.map
                            (\list ->
                                case list of
                                    [ ".." ] ->
                                        ExposingEverything

                                    _ ->
                                        ExposingOnly list
                            )
                   )
            , Parser.succeed (ExposingOnly [])
            ]


type alias CustomType =
    { name : TypeName
    , constructors : List CustomTypeConstructor
    }


nameFromCustomType : CustomType -> TypeName
nameFromCustomType { name } =
    name


type TypeParam
    = TypeParam String


type TypeName
    = TypeName String (List TypeParam)


type CustomTypeConstructor
    = CustomTypeConstructor String


{-|

    import Parser

    Parser.run typeName "Alpha.Beta.Charlie a b | Dude"
    --> Ok (TypeName "Alpha.Beta.Charlie" [ TypeParam "a", TypeParam "b" ])

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
                            List.map TypeParam (String.words s)
                    )
           )


{-|

    import Parser

    Parser.run customTypeConstructor "Types.OnNotificationRegistered (Result.Result String String.String)"
    --> Ok (CustomTypeConstructor "Types.OnNotificationRegistered (Result.Result String String.String)")

-}
customTypeConstructor : Parser.Parser CustomTypeConstructor
customTypeConstructor =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile (isNoneOf [ '\n', '\u{000D}', '|' ])
        |> Parser.getChompedString
        |> Parser.map CustomTypeConstructor


customTypeConstructorList : Parser.Parser (List CustomTypeConstructor)
customTypeConstructorList =
    let
        customTypeConstructorListHelp revList =
            Parser.oneOf
                [ Parser.succeed (\s -> Parser.Loop (s :: revList))
                    |. comments
                    |= customTypeConstructor
                    |. comments
                    |. Parser.oneOf [ Parser.symbol "|", Parser.succeed () ]
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] customTypeConstructorListHelp


{-| NestedString
-}
type NS
    = NS String (List NS)



--
-- {-| Parser.run constructorLiteral "(OnEvent Request (Result x a))"
-- --> Ok (NS "OnEvent" [NS "Request" [], NS "Result" [NS "x" [], NS "a" []]])
--
--     import Parser
--
--     Parser.run constructorLiteral "One Two (Three)"
--     --> Ok (NS "One" [NS "Two" [], NS "Three" []])
--
-- -}
-- constructorLiteral : Parser.Parser NS
-- constructorLiteral =
--     let
--         helper revList =
--             Parser.oneOf
--                 [ Parser.succeed (\s -> )
--                     |= Parser.
--                 ]
--     in
--     Parser.loop [] helper


{-|

    import Parser

    typesFromCustomTypeConstructor (CustomTypeConstructor "Hello")
    --> NS "Hello" []

-}
typesFromCustomTypeConstructor : CustomTypeConstructor -> NS
typesFromCustomTypeConstructor (CustomTypeConstructor s) =
    NS s []


{-|

    import Parser

    orderCustomType : CustomType
    orderCustomType =
        CustomType
            (TypeName "Order" [])
            [ CustomTypeConstructor "LT"
            , CustomTypeConstructor "EQ"
            , CustomTypeConstructor "GT"
            ]

    Parser.run customType (String.trim ("""
        type Order
            = LT
            | EQ
            | GT
    """))
    --> Ok orderCustomType

    maybeCustomType : CustomType
    maybeCustomType =
        CustomType
            (TypeName "Maybe" [ TypeParam "a" ])
            [ CustomTypeConstructor "Nothing"
            , CustomTypeConstructor "Just a"
            ]

    Parser.run customType (String.trim ("""
        type Maybe a
            = Nothing
            | Just a
    """))
    --> Ok maybeCustomType

    dictCustomType : CustomType
    dictCustomType =
        CustomType
            (TypeName "Dict" [ TypeParam "a", TypeParam "b" ])
            [ CustomTypeConstructor "Dict (Set (a, b))"
            ]

    Parser.run customType (String.trim ("""
        type Dict a b = Dict (Set (a, b))
    """))
    --> Ok dictCustomType

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


type FieldName
    = FieldName String


type FieldPair
    = FieldPair FieldName TypeName


type TypeAlias
    = AliasRecordType TypeName (List FieldPair)
    | AliasCustomType CustomType


nameFromTypeAlias : TypeAlias -> TypeName
nameFromTypeAlias t =
    case t of
        AliasRecordType name _ ->
            name

        AliasCustomType t2 ->
            nameFromCustomType t2


fieldName : Parser.Parser FieldName
fieldName =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map FieldName


{-|

    import Parser

    Parser.run fieldPair "userID : String"
    --> Ok (FieldPair (FieldName "userID") (TypeName "String" []))

-}
fieldPair : Parser.Parser FieldPair
fieldPair =
    Parser.succeed FieldPair
        |= fieldName
        |. comments
        |. Parser.symbol ":"
        |. comments
        |= typeName


{-|

    import Parser

    Parser.run fieldPairList ("""{
          userID : String
        , email : Email
    }""")
    --> Ok [(FieldPair (FieldName "userID") (TypeName "String" [])),FieldPair (FieldName "email") (TypeName "Email" [])]

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
    Parser.succeed (\name list -> AliasCustomType (CustomType name list))
        |. Parser.keyword "type"
        |. comments
        |. Parser.keyword "alias"
        |. comments
        |= typeName
        |. comments
        |. Parser.symbol "="
        |. comments
        |= customTypeConstructorList


{-|

    import Parser

    Parser.run typeAlias ("""type alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok (AliasRecordType (TypeName "User" []) [(FieldPair (FieldName "userID") (TypeName "String" [])),FieldPair (FieldName "email") (TypeName "Email" [])])

    Parser.run typeAlias ("""type alias User = String""")
    --> Ok (AliasCustomType (CustomType (TypeName "User" []) [CustomTypeConstructor "String"]))

-}
typeAlias : Parser.Parser TypeAlias
typeAlias =
    Parser.oneOf
        [ Parser.backtrackable aliasRecordType
        , aliasCustomType
        ]


type ElmType
    = ElmCustomType CustomType
    | ElmTypeAlias TypeAlias


nameFromElmType : ElmType -> TypeName
nameFromElmType elmType =
    case elmType of
        ElmCustomType t ->
            nameFromCustomType t

        ElmTypeAlias t ->
            nameFromTypeAlias t


{-|

    import Parser

    orderCustomType : CustomType
    orderCustomType =
        CustomType
            (TypeName "Order" [])
            [ CustomTypeConstructor "LT"
            , CustomTypeConstructor "EQ"
            , CustomTypeConstructor "GT"
            ]

    boolCustomType : CustomType
    boolCustomType =
        CustomType
            (TypeName "Bool" [])
            [ CustomTypeConstructor "True"
            , CustomTypeConstructor "False"
            ]

    userTypeAlias : TypeAlias
    userTypeAlias =
        AliasRecordType
            (TypeName "User" [])
            [ FieldPair (FieldName "userID") (TypeName "String" [])
            , FieldPair (FieldName "email") (TypeName "Email" [])
            ]

    Parser.run elmTypeList (String.trim ("""
        type Order
            = LT
            | EQ
            | GT\n\ntype Bool
            = True
            | False
    """))
    --> Ok [ ElmCustomType orderCustomType, ElmCustomType boolCustomType]

    Parser.run elmTypeList ("""type alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok [ElmTypeAlias userTypeAlias]


    Parser.run elmTypeList ("""type Order
            = LT
            | EQ
            | GT\n\ntype alias User = {
          userID : String
        , email : Email
    }""")
    --> Ok [ElmCustomType orderCustomType, ElmTypeAlias userTypeAlias]

-}
elmTypeList : Parser.Parser (List ElmType)
elmTypeList =
    let
        elmTypeListHelp revList =
            Parser.oneOf
                [ Parser.succeed (\s -> Parser.Loop (ElmTypeAlias s :: revList))
                    |= Parser.backtrackable typeAlias
                , Parser.succeed (\s -> Parser.Loop (ElmCustomType s :: revList))
                    |= customType
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse revList))
                ]
    in
    Parser.loop [] elmTypeListHelp


problemIfEmpty : List a -> Parser (List a)
problemIfEmpty list =
    case list of
        [] ->
            Parser.problem "empty"

        _ ->
            Parser.succeed list


type alias ElmFile =
    { modulePrefix : String
    , imports : Set String -- ["Json.Decode"]
    , importResolver_ : Dict String String -- Dict.fromList [("chompWhile", "Parser.Advanced.Parser")]
    , knownTypes : Dict String ElmType -- Dict.fromList [("Json.Encode.Value", ...)]
    , skipTypes : Set String
    }


qualifyName : Dict String String -> String -> String
qualifyName dict string =
    Maybe.withDefault string (Dict.get string dict)


qualifyConstructor : Dict String String -> CustomTypeConstructor -> CustomTypeConstructor
qualifyConstructor dict (CustomTypeConstructor name) =
    CustomTypeConstructor (String.join " " (List.map (qualifyName dict) (String.words name)))


qualifyFieldPair : Dict String String -> FieldPair -> FieldPair
qualifyFieldPair dict (FieldPair fname (TypeName tname typeParams)) =
    let
        qualifiedTypeParams =
            List.map (\(TypeParam s) -> TypeParam (qualifyName dict s)) typeParams
    in
    FieldPair fname (TypeName (qualifyName dict tname) qualifiedTypeParams)


qualifyCustomType : Dict String String -> CustomType -> CustomType
qualifyCustomType dict { name, constructors } =
    let
        (TypeName tname typeParams) =
            name

        newConstructors =
            List.map (qualifyConstructor dict) constructors
    in
    { name = TypeName (qualifyName dict tname) typeParams, constructors = newConstructors }


qualifyType : Dict String String -> ElmType -> ElmType
qualifyType dict elmType =
    case elmType of
        ElmCustomType t ->
            ElmCustomType (qualifyCustomType dict t)

        ElmTypeAlias (AliasRecordType (TypeName tname typeParams) fieldPairs) ->
            ElmTypeAlias (AliasRecordType (TypeName (qualifyName dict tname) typeParams) (List.map (qualifyFieldPair dict) fieldPairs))

        ElmTypeAlias (AliasCustomType t) ->
            ElmTypeAlias (AliasCustomType (qualifyCustomType dict t))


qualifyCustomTypeConstructor : Dict String String -> CustomTypeConstructor -> CustomTypeConstructor
qualifyCustomTypeConstructor dict (CustomTypeConstructor s) =
    Dict.get s dict
        |> Maybe.withDefault s
        |> CustomTypeConstructor


parentModuleName : String -> String
parentModuleName =
    String.split "." >> List.reverse >> List.drop 1 >> List.reverse >> String.join "."


addReferencedTypes : String -> ElmType -> ElmFile -> ElmFile
addReferencedTypes modulePrefix elmType file =
    let
        prefixIfMissing phrase maybeExist =
            case ( maybeExist, Set.member (parentModuleName phrase) file.imports ) of
                ( Nothing, False ) ->
                    Just (modulePrefix ++ phrase)

                _ ->
                    maybeExist
    in
    case elmType of
        ElmCustomType { constructors } ->
            let
                newImportResolver =
                    List.foldl
                        (\(CustomTypeConstructor s) acc -> Dict.update s (prefixIfMissing s) acc)
                        file.importResolver_
                        constructors
            in
            { file | importResolver_ = newImportResolver }

        ElmTypeAlias (AliasRecordType _ fieldPairs) ->
            let
                newImportResolver =
                    List.foldl
                        (\(FieldPair _ (TypeName s typeParams)) acc -> Dict.update s (prefixIfMissing s) acc)
                        file.importResolver_
                        fieldPairs
            in
            { file | importResolver_ = newImportResolver }

        ElmTypeAlias (AliasCustomType t) ->
            addReferencedTypes modulePrefix (ElmCustomType t) file


addElmType : ElmType -> ElmFile -> ElmFile
addElmType elmType file =
    let
        (TypeName shortName typeParams) =
            nameFromElmType elmType

        absoluteName =
            file.modulePrefix ++ shortName

        newElmFile =
            addReferencedTypes
                file.modulePrefix
                elmType
                { file | importResolver_ = Dict.insert shortName absoluteName file.importResolver_ }
    in
    if Set.member shortName file.skipTypes then
        file

    else
        { newElmFile | knownTypes = Dict.insert shortName elmType file.knownTypes }


addModuleDefinition : ElmFile -> ModuleDefinition -> String -> ElmFile
addModuleDefinition file (ModuleDefinition (TitleCaseDotPhrase name) exposing_) moduleComments =
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


addImportDefinition : ElmFile -> ImportDefinition -> ElmFile
addImportDefinition file (ImportDefinition (TitleCaseDotPhrase name) maybeAliasName exposing_) =
    let
        newImportResolver =
            -- Dict.insert name name
            file.importResolver_

        importResolverWithMaybeAliasName =
            Maybe.map (\aliasName -> Dict.insert aliasName name newImportResolver) maybeAliasName
                |> Maybe.withDefault newImportResolver
    in
    case exposing_ of
        ExposingEverything ->
            { file | imports = Set.insert name file.imports, importResolver_ = importResolverWithMaybeAliasName }

        ExposingOnly [] ->
            { file | imports = Set.insert name file.imports, importResolver_ = importResolverWithMaybeAliasName }

        ExposingOnly (x :: xs) ->
            let
                namespaced =
                    if List.any (not << Char.isAlphaNum) (String.toList x) then
                        -- for infix operators, restore the parenthesis
                        name ++ "." ++ "(" ++ x ++ ")"

                    else
                        name ++ "." ++ x

                newFile =
                    { file | importResolver_ = Dict.insert x namespaced newImportResolver }
            in
            addImportDefinition newFile (ImportDefinition (TitleCaseDotPhrase name) maybeAliasName (ExposingOnly xs))


fileContent : Parser.Parser ElmFile
fileContent =
    let
        fileContentHelp currentFile =
            Parser.oneOf
                [ Parser.succeed (List.foldl addElmType currentFile >> Parser.Loop)
                    |. comments
                    |= Parser.andThen problemIfEmpty elmTypeList
                    |. comments
                , Parser.succeed (\m c -> Parser.Loop (addModuleDefinition currentFile m c))
                    |= moduleDefinition
                    |= (comments |> Parser.getChompedString |> Parser.map String.trim)
                , Parser.succeed (addImportDefinition currentFile >> Parser.Loop)
                    |= importDefinition
                    |. comments
                , Parser.succeed (Parser.Done currentFile)
                    |. Parser.end
                , Parser.succeed (Parser.Loop currentFile)
                    |. Parser.chompUntilEndOr "\n"
                    |. comments
                ]
    in
    Parser.loop
        { modulePrefix = ""
        , imports = Set.empty
        , importResolver_ =
            Dict.fromList
                [ ( "String", "String" )
                , ( "String.String", "String" )
                , ( "Set", "Set.Set" )
                , ( "Dict", "Dict.Dict" )
                , ( "Maybe", "Maybe" )
                , ( "Maybe.Maybe", "Maybe" )
                , ( "List", "List" )
                , ( "List.List", "List" )
                , ( "Int", "Int" )
                , ( "Float", "Float" )
                , ( "Bool", "Bool" )
                ]
        , knownTypes = Dict.empty
        , skipTypes = Set.empty
        }
        fileContentHelp



--


isNoneOf : List Char -> Char -> Bool
isNoneOf characters char =
    not (List.member char characters)


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
                , Parser.spaces -- only here
                ]


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


isFunction : String -> Bool
isFunction =
    String.contains "->"
