port module Main exposing (..)

import Elm.Types exposing (ElmFile)
import Elm.Types.AutoEncoder
import Elm.Types.Parser
import Json.Decode
import Json.Encode
import Parser
import Set exposing (Set)


type alias Flags =
    {}


type Model
    = Model


type Msg
    = OnFileRead (Result Json.Decode.Error ReadFile)
    | OnFileWritten (Result Json.Decode.Error WroteFile)


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFileRead (Ok event) ->
            case ( stdlib, Parser.run Elm.Types.Parser.fileContent event.data ) of
                ( Ok prelude, Ok elmFile ) ->
                    ( model
                    , writeUTF8
                        { filename = String.replace ".elm" "/Auto.elm" event.filename
                        , content =
                            Elm.Types.AutoEncoder.produceSourceCode
                                prelude
                                elmFile
                                event.extraImport
                                event.autoModules
                        }
                    )

                ( _, Err err ) ->
                    let
                        _ =
                            Debug.log "Elm.Types.Parser.fileContent" ( err, event.filename )
                    in
                    ( model, exit 1 )

                ( Err err, _ ) ->
                    let
                        _ =
                            Debug.log "stdlib" err
                    in
                    ( model, exit 1 )

        OnFileRead (Err err) ->
            let
                _ =
                    Debug.log "read" err
            in
            ( model, exit 1 )

        OnFileWritten (Ok { filename }) ->
            let
                _ =
                    Debug.log "wrote" filename
            in
            ( model, exit 0 )

        OnFileWritten (Err err) ->
            let
                _ =
                    Debug.log "wrote" err
            in
            ( model, exit 1 )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onReadUTF8 OnFileRead
        , onWriteUTF OnFileWritten
        ]



--


{-| Define some built-in types; use our own Parser to generate encoder/decoder for these
-}
stdlib : Result (List Parser.DeadEnd) String
stdlib =
    let
        data =
            """

type Maybe a
    = Nothing
    | Just a

type Result x a
    = Err x
    | Ok a

            """
    in
    case Parser.run Elm.Types.Parser.fileContent data of
        Ok elmFile ->
            Elm.Types.AutoEncoder.encoderDefinitions elmFile
                ++ "\n\n"
                ++ Elm.Types.AutoEncoder.decoderDefinitions elmFile
                |> Ok

        Err err ->
            Err err



--


type alias ReadFile =
    { filename : String
    , encoding : String
    , extraImport : Maybe String
    , autoModules : Set String
    , data : String
    }


type alias WroteFile =
    { filename : String
    , encoding : String
    }


readUTF8 : String -> Cmd msg
readUTF8 filename =
    readFile
        (Json.Encode.object
            [ ( "encoding", Json.Encode.string "utf8" )
            , ( "filename", Json.Encode.string filename )
            ]
        )


writeUTF8 : { filename : String, content : String } -> Cmd msg
writeUTF8 { filename, content } =
    writeFile
        (Json.Encode.object
            [ ( "encoding", Json.Encode.string "utf8" )
            , ( "filename", Json.Encode.string filename )
            , ( "data", Json.Encode.string content )
            ]
        )


onReadUTF8 : (Result Json.Decode.Error ReadFile -> msg) -> Sub msg
onReadUTF8 tagger =
    onFileContent (Json.Decode.decodeValue decodeReadFile >> tagger)


onWriteUTF : (Result Json.Decode.Error WroteFile -> msg) -> Sub msg
onWriteUTF tagger =
    onFileWritten (Json.Decode.decodeValue decodeWroteFile >> tagger)


decodeReadFile : Json.Decode.Decoder ReadFile
decodeReadFile =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.maybe (Json.Decode.field "err" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "data" Json.Decode.string))
        |> Json.Decode.andThen
            (\( err, data ) ->
                case ( err, data ) of
                    ( Nothing, Just s ) ->
                        Json.Decode.map5 ReadFile
                            (Json.Decode.field "filename" Json.Decode.string)
                            (Json.Decode.field "encoding" Json.Decode.string)
                            (Json.Decode.field "extraImport" (Json.Decode.maybe Json.Decode.string))
                            (Json.Decode.field "autoModules" (Json.Decode.list Json.Decode.string) |> Json.Decode.map Set.fromList)
                            (Json.Decode.succeed s)

                    ( maybeErr, _ ) ->
                        Json.Decode.fail (Debug.toString maybeErr)
            )


decodeWroteFile : Json.Decode.Decoder WroteFile
decodeWroteFile =
    Json.Decode.map3 (\a b c -> { filename = a, encoding = b, err = c })
        (Json.Decode.field "filename" Json.Decode.string)
        (Json.Decode.field "encoding" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "err" Json.Decode.string))
        |> Json.Decode.andThen
            (\{ filename, encoding, err } ->
                case err of
                    Nothing ->
                        Json.Decode.succeed { filename = filename, encoding = encoding }

                    Just error ->
                        Json.Decode.fail (Debug.toString error)
            )


port readFile : Json.Encode.Value -> Cmd msg


port writeFile : Json.Encode.Value -> Cmd msg


port onFileContent : (Json.Encode.Value -> msg) -> Sub msg


port onFileWritten : (Json.Encode.Value -> msg) -> Sub msg


port exit : Int -> Cmd msg
