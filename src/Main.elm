port module Main exposing (..)

--   https://guide.elm-lang.org/effects/random.html

import Array exposing (..)
import Base64
import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Platform.Cmd as Cmd
import Process
import Random
import Svg exposing (image, svg)
import Svg.Attributes as SvgAttr
import Tables exposing (..)
import Task
import Url


viewShareNotice : Model -> Html Msg
viewShareNotice model =
    case model.shareNotice of
        Nothing ->
            text ""

        Just notice ->
            div [ class "share-notice" ] [ text notice ]

version : String
version =
    "0.6.0"


schemaVersion : Int
schemaVersion =
    1


type alias PersistedState =
    { version : Int
    , seed : Int
    , actions : List Action
    }


type Action
    = RollAction Int Int Bool
    | AskAction String
    | ShowAction Int
    | ClearAction
    | ResetSeed Int


actionEncoder : Action -> E.Value
actionEncoder action =
    case action of
        RollAction sides amount explode ->
            E.object
                [ ( "tag", E.string "roll" )
                , ( "sides", E.int sides )
                , ( "amount", E.int amount )
                , ( "explode", E.bool explode )
                ]

        AskAction table ->
            E.object
                [ ( "tag", E.string "ask" )
                , ( "table", E.string table )
                ]

        ShowAction iconAmount ->
            E.object
                [ ( "tag", E.string "show" )
                , ( "iconAmount", E.int iconAmount )
                ]

        ClearAction ->
            E.object [ ( "tag", E.string "clear" ) ]

        ResetSeed value ->
            E.object [ ( "tag", E.string "resetSeed" ), ( "value", E.int value ) ]


actionDecoder : D.Decoder Action
actionDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "roll" ->
                        D.map3 RollAction
                            (D.field "sides" D.int)
                            (D.field "amount" D.int)
                            (D.field "explode" D.bool)

                    "ask" ->
                        D.field "table" D.string |> D.map AskAction

                    "show" ->
                        D.field "iconAmount" D.int |> D.map ShowAction

                    "clear" ->
                        D.succeed ClearAction

                    "resetSeed" ->
                        D.field "value" D.int |> D.map ResetSeed

                    _ ->
                        D.fail "Unknown action tag"
            )



-- MAIN


main =
    Browser.application
        { init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }



-- STORAGE


port setStorage : E.Value -> Cmd msg


port clearStorage : () -> Cmd msg


port reloadApp : () -> Cmd msg


port copyToClipboard : String -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel

        shouldSaveToStorage =
            case msg of
                GotIcons _ ->
                    False

                NoOp ->
                    False

                _ ->
                    True
    in
    ( newModel
    , if shouldSaveToStorage then
        Cmd.batch [ setStorage (encode newModel), cmds ]

      else
        cmds
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "version", E.int schemaVersion )
        , ( "seed", E.int model.seedValue )
        , ( "actions", E.list actionEncoder model.actions )
        ]


decoder : D.Decoder PersistedState
decoder =
    D.map3 PersistedState
        (D.field "version" D.int |> D.map (\v -> if v <= 0 then 1 else v))
        (D.field "seed" D.int)
        (D.field "actions" (D.list actionDecoder))


entryEncoder : Entry -> E.Value
entryEncoder entry =
    case entry of
        Txt list ->
            E.object [ ( "Txt", E.list E.string list ) ]

        Img idx ->
            E.object [ ( "Img", E.int idx ) ]


entryDecoder : D.Decoder Entry
entryDecoder =
    D.oneOf
        [ D.field "Txt" (D.map Txt (D.list D.string))
        , D.field "Img" (D.map Img D.int)
        ]



-- MODEL


type alias Model =
    { version : Int
    , seedValue : Int
    , seed : Random.Seed
    , seedReady : Bool
    , actions : List Action
    , dice_sides : Int
    , dice_amount : Int
    , dice_explode : Bool
    , table : String
    , icon_amount : Int
    , icons : List String
    , journal : List Entry
    , pendingHydration : Maybe PersistedState
    , versionConflict : Maybe PersistedState
    , key : Nav.Key
    , shareNotice : Maybe String
    }


type Entry
    = Txt (List String)
    | Img Int


init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        persistedFromUrl =
            parseStateFromUrl url

        persistedFromStorage =
            case D.decodeValue decoder flags of
                Ok state ->
                    Just state

                Err _ ->
                    Nothing

        chosenPersisted =
            case persistedFromUrl of
                Just state ->
                    Just state

                Nothing ->
                    persistedFromStorage

        versionConflict =
            chosenPersisted
                |> Maybe.andThen
                    (\state ->
                        if state.version > schemaVersion then
                            Just state

                        else
                            Nothing
                    )

        actualPersisted =
            if versionConflict /= Nothing then
                Nothing

            else
                chosenPersisted

        baseModel =
            { version = schemaVersion
            , seedValue = 0
            , seed = Random.initialSeed 0
            , seedReady = False
            , actions = []
            , dice_sides = 6
            , dice_amount = 1
            , dice_explode = False
            , table = Dict.keys tables |> List.head |> Maybe.withDefault ""
            , icon_amount = 3
            , icons = []
            , journal = []
            , pendingHydration = actualPersisted
            , versionConflict = versionConflict
            , key = key
            , shareNotice = Nothing
            }

        seededModel =
            case actualPersisted of
                Just state ->
                    { baseModel
                        | version = state.version
                        , seedValue = state.seed
                        , seed = Random.initialSeed state.seed
                        , seedReady = True
                        , actions = state.actions
                    }

                Nothing ->
                    baseModel

        seedCmd =
            case actualPersisted of
                Just _ ->
                    Cmd.none

                Nothing ->
                    Random.generate SeedGenerated (Random.int 1 2147483646)
    in
    ( seededModel
    , Cmd.batch
        [ Http.get
            { url = "./icons/icons.json"
            , expect = Http.expectJson GotIcons iconsDecoder
            }
        , seedCmd
        ]
    )


iconsDecoder : D.Decoder (List String)



-- From:
-- {
--   "icons": {
--     "1x1": {
--       "andymeneely": [ "police-badge", "riposte" ],
--       "aussiesim": [ "card-10-clubs", … ],
--       …
--     }
--   }
-- }
-- To:
-- [
-- "icons/ffffff/transparent/1x1/andymeneely/police-badge.svg",
-- "icons/ffffff/transparent/1x1/andymeneely/riposte.svg",
-- "icons/ffffff/transparent/1x1/aussiesim/card-10-clubs.svg",
-- …
-- ]


iconsDecoder =
    D.field "icons" (D.dict (D.dict (D.list D.string)))
        |> D.map
            (\dict ->
                dict
                    |> Dict.toList
                    |> List.concatMap
                        (\( size, vendorDict ) ->
                            vendorDict
                                |> Dict.toList
                                |> List.concatMap
                                    (\( vendor, icons ) ->
                                        List.map (\icon -> "./icons/ffffff/transparent/" ++ size ++ "/" ++ vendor ++ "/" ++ icon ++ ".svg") icons
                                    )
                        )
            )



-- URL STATE ENCODING/DECODING


parseStateFromUrl : Url.Url -> Maybe PersistedState
parseStateFromUrl url =
    let
        getStateParam query =
            query
                |> String.split "&"
                |> List.filter (String.startsWith "state=")
                |> List.head
                |> Maybe.map (String.dropLeft (String.length "state="))
    in
    url.query
        |> Maybe.andThen getStateParam
        |> Maybe.andThen decodeStateFromString


decodeStateFromString : String -> Maybe PersistedState
decodeStateFromString encoded =
    Url.percentDecode encoded
        |> Maybe.andThen
            (\decoded ->
                case Base64.decode decoded of
                    Err _ ->
                        Nothing

                    Ok json ->
                        case D.decodeString urlStateDecoder json of
                            Ok state ->
                                Just state

                            Err _ ->
                                Nothing
            )


encodeStateToString : Model -> String
encodeStateToString model =
    encode model
        |> E.encode 0
        |> Base64.encode
        |> Url.percentEncode


urlStateDecoder : D.Decoder PersistedState
urlStateDecoder =
    decoder


urlEntryEncoder : Entry -> E.Value
urlEntryEncoder entry =
    case entry of
        Txt list ->
            E.object [ ( "Txt", E.list E.string list ) ]

        Img idx ->
            E.object [ ( "Img", E.int idx ) ]


urlEntryDecoder : D.Decoder Entry
urlEntryDecoder =
    D.oneOf
        [ D.field "Txt" (D.map Txt (D.list D.string))
        , D.field "Img" (D.map Img D.int)
        ]



-- UPDATE


type Msg
    = GotIcons (Result Http.Error (List String))
    | UpdateDiceSides String
    | UpdateDiceAmount String
    | UpdateDiceExplode
    | Roll
    | UpdateTable String
    | Ask
    | UpdateIconAmount String
    | Show
    | Clear
    | SeedGenerated Int
    | SeedRefreshed Int
    | ShareUrl
    | ClearShareNotice
    | DismissVersionConflict
    | ReloadWithNewVersion
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIcons (Ok iconsList) ->
            let
                normalizedIcons =
                    List.sort iconsList

                hydratedModel =
                    case model.pendingHydration of
                        Just state ->
                            hydrateFromPersisted state model.key normalizedIcons

                        Nothing ->
                            model
            in
            ( { hydratedModel | icons = normalizedIcons, pendingHydration = Nothing }, Cmd.none )

        GotIcons (Err _) ->
            ( model, Cmd.none )

        SeedGenerated seedInt ->
            ( { model
                | seedValue = seedInt
                , seed = Random.initialSeed seedInt
                , seedReady = True
              }
            , Cmd.none
            )

        SeedRefreshed seedInt ->
            ( { model
                | seedValue = seedInt
                , seed = Random.initialSeed seedInt
                , seedReady = True
                , actions = model.actions ++ [ ResetSeed seedInt ]
              }
            , Cmd.none
            )

        UpdateDiceSides sides ->
            case String.toInt sides of
                Just value ->
                    ( { model | dice_sides = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateDiceAmount amount ->
            case String.toInt amount of
                Just value ->
                    ( { model | dice_amount = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateDiceExplode ->
            ( { model | dice_explode = not model.dice_explode }, Cmd.none )

        Roll ->
            ( recordAction (RollAction model.dice_sides model.dice_amount model.dice_explode) model, Cmd.none )

        UpdateTable table ->
            ( { model | table = table }, Cmd.none )

        Ask ->
            ( recordAction (AskAction model.table) model, Cmd.none )

        UpdateIconAmount amount ->
            case String.toInt amount of
                Just value ->
                    ( { model | icon_amount = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Show ->
            ( recordAction (ShowAction model.icon_amount) model, Cmd.none )

        Clear ->
            ( recordAction ClearAction model, Random.generate SeedRefreshed (Random.int 1 2147483646) )

        ShareUrl ->
            let
                stateEncoded =
                    encodeStateToString model

                shareableUrl =
                    "?state=" ++ stateEncoded

                clearNoticeCmd =
                    Task.perform (\_ -> ClearShareNotice) (Process.sleep 2500)
            in
            ( { model | shareNotice = Just "Link copied to clipboard" }
            , Cmd.batch
                [ Nav.pushUrl model.key shareableUrl
                , copyToClipboard shareableUrl
                , clearNoticeCmd
                ]
            )

        ClearShareNotice ->
            ( { model | shareNotice = Nothing }, Cmd.none )

        DismissVersionConflict ->
            ( { model | versionConflict = Nothing }, Cmd.none )

        ReloadWithNewVersion ->
            ( model, reloadApp () )

        NoOp ->
            ( model, Cmd.none )



-- RANDOM GENERATION


rollExplodingDice : Int -> Int -> Random.Generator (List Int)
rollExplodingDice amount sides =
    Random.map List.concat (Random.list amount (explosiveDieRoll sides))


explosiveDieRoll : Int -> Random.Generator (List Int)
explosiveDieRoll sides =
    Random.int 1 sides
        |> Random.andThen
            (\roll ->
                if roll == sides then
                    Random.map ((::) roll) (explosiveDieRoll sides)

                else
                    Random.constant [ roll ]
            )


randomIcons : List String -> Int -> Random.Generator (List String)
randomIcons icons amount =
    Random.list amount (Random.int 0 (List.length icons - 1))
        |> Random.map (List.map (\i -> List.Extra.getAt i icons |> Maybe.withDefault ""))


randomIconIndices : List String -> Int -> Random.Generator (List Int)
randomIconIndices icons amount =
    Random.list amount (Random.int 0 (List.length icons - 1))


applyAction : Action -> Model -> Model
applyAction action model =
    case action of
        RollAction sides amount explode ->
            if model.seedReady then
                let
                    generator =
                        if explode then
                            rollExplodingDice amount sides

                        else
                            Random.list amount (Random.int 1 sides)

                    ( dice, nextSeed ) =
                        Random.step generator model.seed
                in
                { model
                    | seed = nextSeed
                    , journal =
                        Txt
                            [ "Dice ("
                                ++ String.fromInt amount
                                ++ "d"
                                ++ String.fromInt sides
                                ++ (if explode then
                                        "!"

                                    else
                                        ""
                                   )
                                ++ "): "
                                ++ String.join ", " (List.map String.fromInt dice)
                            ]
                            :: model.journal
                }

            else
                model

        AskAction table ->
            if model.seedReady then
                let
                    tableGenerator =
                        Dict.get table tables |> Maybe.withDefault (Random.constant "Table not found")

                    ( answer, nextSeed ) =
                        Random.step tableGenerator model.seed
                in
                { model
                    | seed = nextSeed
                    , journal = Txt [ "Table (" ++ table ++ "): " ++ answer ] :: model.journal
                }

            else
                model

        ShowAction iconAmount ->
            if model.seedReady && not (List.isEmpty model.icons) then
                let
                    ( iconIndices, nextSeed ) =
                        Random.step (randomIconIndices model.icons iconAmount) model.seed
                in
                { model
                    | seed = nextSeed
                    , journal =
                        Txt [ "Icons: " ] :: List.map Img iconIndices ++ model.journal
                }

            else
                model

        ClearAction ->
            { model | journal = [] }

        ResetSeed value ->
            { model
                | seedValue = value
                , seed = Random.initialSeed value
                , seedReady = True
            }


recordAction : Action -> Model -> Model
recordAction action model =
    let
        updated =
            applyAction action model
    in
    { updated | actions = model.actions ++ [ action ] }


hydrateFromPersisted : PersistedState -> Nav.Key -> List String -> Model
hydrateFromPersisted state key icons =
    let
        baseModel =
            { version = state.version
            , seedValue = state.seed
            , seed = Random.initialSeed state.seed
            , seedReady = True
            , actions = []
            , dice_sides = 6
            , dice_amount = 1
            , dice_explode = False
            , table = Dict.keys tables |> List.head |> Maybe.withDefault ""
            , icon_amount = 3
            , icons = icons
            , journal = []
            , pendingHydration = Nothing
            , versionConflict = Nothing
            , key = key
            , shareNotice = Nothing
            }
    in
    List.foldl applyAction baseModel state.actions
        |> (\m -> { m | actions = state.actions })


viewVersionConflict : Model -> Html Msg
viewVersionConflict model =
    case model.versionConflict of
        Nothing ->
            text ""

        Just state ->
            div [ class "version-warning" ]
                [ h2 [] [ text "⚠️ Newer Version Detected" ]
                , p []
                    [ text "This shared link was created with app version "
                    , strong [] [ text (String.fromInt state.version) ]
                    , text ", but you are running version "
                    , strong [] [ text (String.fromInt schemaVersion) ]
                    , text "."
                    ]
                , p [] [ text "To view this shared state, you need to update the app." ]
                , div [ class "version-warning-actions" ]
                    [ button [ type_ "button", class "my-btn", onClick ReloadWithNewVersion ]
                        [ text "Update & Load State" ]
                    , button [ type_ "button", class "my-btn", onClick DismissVersionConflict ]
                        [ text "Continue Without Loading" ]
                    ]
                , p [ class "version-note" ]
                    [ text "Note: If offline or the app cannot update, you won't be able to load this state." ]
                ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dice Roller (v" ++ version ++ ")"
    , body =
        [ main_ []
            [ header []
                [ h1 [ class "title" ] [ text ("Dice Roller (v" ++ version ++ ")") ]
                , viewVersionConflict model
                , viewShareNotice model
                , Html.form [ Html.Events.onSubmit NoOp ]
                    [ fieldset [ class "my-inline-container" ]
                        [ legend [] [ text "Dices" ]
                        , div [ class "my-input" ]
                            [ label [ for "dice_sides" ] [ text "Sides" ]
                            , input [ id "dice_sides", type_ "text", placeholder (String.fromInt model.dice_sides), onInput UpdateDiceSides ] []
                            ]
                        , div [ class "my-input" ]
                            [ label [ for "dice_amount" ] [ text "Amount" ]
                            , input [ id "dice_amount", type_ "text", placeholder (String.fromInt model.dice_amount), onInput UpdateDiceAmount ] []
                            ]
                        , div [ class "my-input" ]
                            [ input [ id "dice_explode", type_ "checkbox", checked model.dice_explode, onClick UpdateDiceExplode ] []
                            , label [ for "dice_explode" ] [ text "Explode" ]
                            ]
                        , button [ type_ "button", class "my-btn", onClick Roll ] [ text "Roll" ]
                        ]
                    , fieldset [ class "my-inline-container" ]
                        [ legend [] [ text "Tables" ]
                        , div [ class "my-select" ]
                            [ label [ for "tables" ] [ text "Table" ]
                            , select [ id "tables", onInput UpdateTable ]
                                (List.map (\x -> option [ value x, selected (x == model.table) ] [ text x ]) (Dict.keys tables))
                            ]
                        , button [ type_ "button", class "my-btn", onClick Ask ] [ text "Ask" ]
                        ]
                    , fieldset [ class "my-inline-container" ]
                        [ legend [] [ text "Icons" ]
                        , div [ class "my-input" ]
                            [ label [ for "icon_amount" ] [ text "Amount" ]
                            , input [ id "icon_amount", type_ "text", placeholder (String.fromInt model.icon_amount), onInput UpdateIconAmount ] []
                            ]
                        , button [ type_ "button", class "my-btn", onClick Show ] [ text "Show" ]
                        ]
                    , button [ type_ "button", class "my-btn", onClick Clear ] [ text "Clear" ]
                    , button [ type_ "button", class "my-btn", onClick ShareUrl ] [ text "Share" ]
                    ]
                ]
            , article []
                [ h1 [ class "title" ] [ text "Journal" ]
                , div []
                    (if List.isEmpty model.icons then
                        [ p [] [ text "Loading icons..." ] ]

                     else
                        List.map
                            (\entry ->
                                case entry of
                                    Txt list ->
                                        p [] (List.map text list)

                                    Img idx ->
                                        let
                                            iconSrc =
                                                List.Extra.getAt idx model.icons
                                                    |> Maybe.withDefault ""
                                        in
                                        if String.isEmpty iconSrc then
                                            p [] [ text ("Icon #" ++ String.fromInt idx ++ " (loading...)") ]

                                        else
                                            svg [ SvgAttr.width "100", SvgAttr.height "100" ]
                                                [ image 
                                                    [ SvgAttr.xlinkHref iconSrc
                                                    , SvgAttr.width "100"
                                                    , SvgAttr.height "100"
                                                    , attribute "loading" "lazy"
                                                    ] 
                                                    [] 
                                                ]
                            )
                            model.journal
                    )
                ]
            ]
        ]
    }
