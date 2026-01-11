module VersionConflictTest exposing (..)

import Base64
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import Url


-- Test data structures matching Main.elm
type Action
    = RollAction Int Int Bool
    | AskAction String
    | ShowAction Int
    | ClearAction
    | ResetSeed Int


type Entry
    = Txt (List String)
    | Img Int


type alias PersistedState =
    { version : Int
    , seed : Int
    , actions : List Action
    }


schemaVersion : Int
schemaVersion =
    1


-- Encoders
actionEncoder : Action -> E.Value
actionEncoder action =
    case action of
        RollAction sides amount explode ->
            E.object [ ( "RollAction", E.list E.int [ sides, amount, if explode then 1 else 0 ] ) ]

        AskAction q ->
            E.object [ ( "AskAction", E.string q ) ]

        ShowAction idx ->
            E.object [ ( "ShowAction", E.int idx ) ]

        ClearAction ->
            E.object [ ( "ClearAction", E.null ) ]

        ResetSeed s ->
            E.object [ ( "ResetSeed", E.int s ) ]


persistedStateEncoder : PersistedState -> E.Value
persistedStateEncoder state =
    E.object
        [ ( "version", E.int state.version )
        , ( "seed", E.int state.seed )
        , ( "actions", E.list actionEncoder state.actions )
        ]


-- Decoders
actionDecoder : D.Decoder Action
actionDecoder =
    D.oneOf
        [ D.field "RollAction" (D.list D.int |> D.map (\vals ->
            case vals of
                [sides, amount, explode] ->
                    RollAction sides amount (explode == 1)
                _ ->
                    ClearAction
          ))
        , D.field "AskAction" D.string |> D.map AskAction
        , D.field "ShowAction" D.int |> D.map ShowAction
        , D.field "ClearAction" (D.succeed ClearAction)
        , D.field "ResetSeed" D.int |> D.map ResetSeed
        ]


persistedStateDecoder : D.Decoder PersistedState
persistedStateDecoder =
    D.map3 PersistedState
        (D.field "version" D.int |> D.map (\v -> if v <= 0 then 1 else v))
        (D.field "seed" D.int)
        (D.field "actions" (D.list actionDecoder))


-- Encoding/Decoding functions
encodeStateToString : PersistedState -> String
encodeStateToString state =
    state
        |> persistedStateEncoder
        |> E.encode 0
        |> Base64.encode
        |> Url.percentEncode


decodeStateFromString : String -> Maybe PersistedState
decodeStateFromString encoded =
    case Url.percentDecode encoded of
        Nothing ->
            Nothing

        Just decoded ->
            case Base64.decode decoded of
                Err _ ->
                    Nothing

                Ok json ->
                    case D.decodeString persistedStateDecoder json of
                        Ok state ->
                            Just state

                        Err _ ->
                            Nothing


-- Helper function to detect version conflict
hasVersionConflict : PersistedState -> Bool
hasVersionConflict state =
    state.version > schemaVersion


suite : Test
suite =
    describe "Version Conflict Detection"
        [ test "detects conflict when state version > schemaVersion" <|
            \_ ->
                let
                    state =
                        { version = 2
                        , seed = 12345
                        , actions = [ RollAction 6 1 False ]
                        }
                in
                hasVersionConflict state |> Expect.equal True
        , test "no conflict when state version == schemaVersion" <|
            \_ ->
                let
                    state =
                        { version = 1
                        , seed = 12345
                        , actions = [ RollAction 6 1 False ]
                        }
                in
                hasVersionConflict state |> Expect.equal False
        , test "no conflict when state version < schemaVersion" <|
            \_ ->
                let
                    state =
                        { version = 0
                        , seed = 12345
                        , actions = []
                        }
                in
                hasVersionConflict state |> Expect.equal False
        , test "encode and decode preserves version on normal state" <|
            \_ ->
                let
                    original =
                        { version = 1
                        , seed = 42
                        , actions = [ RollAction 6 2 True ]
                        }

                    encoded =
                        encodeStateToString original

                    decoded =
                        decodeStateFromString encoded
                in
                decoded |> Expect.equal (Just original)
        , test "encode and decode preserves version on conflict state" <|
            \_ ->
                let
                    original =
                        { version = 2
                        , seed = 99
                        , actions = [ AskAction "test", ShowAction 5 ]
                        }

                    encoded =
                        encodeStateToString original

                    decoded =
                        decodeStateFromString encoded
                in
                decoded |> Expect.equal (Just original)
        , test "decode recognizes version 2 state as conflict" <|
            \_ ->
                let
                    state =
                        { version = 2
                        , seed = 12345
                        , actions = [ RollAction 6 1 False ]
                        }

                    encoded =
                        encodeStateToString state

                    decoded =
                        decodeStateFromString encoded |> Maybe.withDefault { version = 0, seed = 0, actions = [] }
                in
                hasVersionConflict decoded |> Expect.equal True
        , test "decode handles malformed base64" <|
            \_ ->
                decodeStateFromString "not-valid-base64!!!" |> Expect.equal Nothing
        , test "decode handles invalid JSON in base64" <|
            \_ ->
                let
                    invalidJson =
                        "invalid json"
                            |> Base64.encode
                            |> Url.percentEncode
                in
                decodeStateFromString invalidJson |> Expect.equal Nothing
        , test "round-trip with multiple actions preserves all data" <|
            \_ ->
                let
                    original =
                        { version = 1
                        , seed = 777
                        , actions =
                            [ RollAction 6 2 False
                            , AskAction "What happened?"
                            , ShowAction 10
                            , ClearAction
                            , ResetSeed 999
                            ]
                        }

                    encoded =
                        encodeStateToString original

                    decoded =
                        decodeStateFromString encoded
                in
                decoded |> Expect.equal (Just original)
        ]
