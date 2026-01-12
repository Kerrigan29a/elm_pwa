port module Main exposing (..)

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
import Svg
import Svg.Attributes as SvgAttr
import Tables exposing (..)
import Task
import Time exposing (Posix, Zone)
import Url


version : String
version =
    "0.8.0"


schemaVersion : Int
schemaVersion =
    1


type ThemeMode
    = Auto
    | Light
    | Dark


type Tab
    = ActionsTab
    | HistoryTab


type Action
    = RollAction Int Int Bool
    | AskAction String
    | ShowAction Int
    | ClearAction
    | ResetSeed Int


type alias RecordedAction =
    { action : Action
    , timestamp : Posix
    }


type alias PersistedState =
    { version : Int
    , seed : Int
    , actions : List RecordedAction
    , theme : ThemeMode
    }


actionEncoder : RecordedAction -> E.Value
actionEncoder recorded =
    case recorded.action of
        RollAction sides amount explode ->
            E.object
                [ ( "tag", E.string "roll" )
                , ( "sides", E.int sides )
                , ( "amount", E.int amount )
                , ( "explode", E.bool explode )
                , ( "ts", E.int (Time.posixToMillis recorded.timestamp) )
                ]

        AskAction table ->
            E.object
                [ ( "tag", E.string "ask" )
                , ( "table", E.string table )
                , ( "ts", E.int (Time.posixToMillis recorded.timestamp) )
                ]

        ShowAction iconAmount ->
            E.object
                [ ( "tag", E.string "show" )
                , ( "iconAmount", E.int iconAmount )
                , ( "ts", E.int (Time.posixToMillis recorded.timestamp) )
                ]

        ClearAction ->
            E.object
                [ ( "tag", E.string "clear" )
                , ( "ts", E.int (Time.posixToMillis recorded.timestamp) )
                ]

        ResetSeed value ->
            E.object
                [ ( "tag", E.string "resetSeed" )
                , ( "value", E.int value )
                , ( "ts", E.int (Time.posixToMillis recorded.timestamp) )
                ]


actionDecoder : D.Decoder RecordedAction
actionDecoder =
    let
        tsDecoder =
            D.oneOf
                [ D.field "ts" D.int |> D.map Time.millisToPosix
                , D.succeed (Time.millisToPosix 0)
                ]
    in
    D.map2
        (\timestamp action -> { action = action, timestamp = timestamp })
        tsDecoder
        (D.field "tag" D.string
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
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dice Roller (v" ++ version ++ ")"
    , body =
        [ div
            [ class "app-shell"
            , attribute "data-theme" (themeToString model.theme)
            , attribute "data-tab" (tabToString model.tab)
            ]
            [ viewVersionConflict model
            , viewShareNotice model
            , viewHeader model
            , div [ class "app-layout" ]
                [ div [ class "pane actions-pane" ] [ viewActions model ]
                , div [ class "pane history-pane" ] [ viewHistory model ]
                ]
            , viewTabs model
            ]
        ]
    }


viewShareNotice : Model -> Html Msg
viewShareNotice model =
    case model.shareNotice of
        Nothing ->
            text ""

        Just notice ->
            div [ class "notice success", attribute "role" "status", attribute "aria-live" "polite" ] [ text notice ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "topbar" ]
        [ div [ class "title-block" ]
            [ img [ class "app-icon", src "favicon-32x32.png", alt "Dice Roller" ] []
            , h1 [ class "app-title" ] [ text "Dice Roller" ]
            , span [ class "app-version" ] [ text ("v" ++ version) ]
            ]
        , div [ class "topbar-actions" ]
            [ viewThemeToggle model ]
        ]


viewThemeToggle : Model -> Html Msg
viewThemeToggle model =
    div [ class "theme-toggle" ]
        [ label [ class "control-label" ] [ text "Theme:" ]
        , select [ class "theme-select", onInput (themeFromString >> SetTheme) ]
            [ option [ value "auto", selected (model.theme == Auto) ] [ text "Auto" ]
            , option [ value "light", selected (model.theme == Light) ] [ text "Light" ]
            , option [ value "dark", selected (model.theme == Dark) ] [ text "Dark" ]
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "tab-bar" ]
        [ tabButton ActionsTab "Actions" model.tab
        , tabButton HistoryTab "History" model.tab
        ]


tabButton : Tab -> String -> Tab -> Html Msg
tabButton tab labelText activeTab =
    button
        [ class "tab-btn"
        , classList [ ( "active", tab == activeTab ) ]
        , type_ "button"
        , onClick (SwitchTab tab)
        ]
        [ text labelText ]


tabToString : Tab -> String
tabToString tab =
    case tab of
        ActionsTab ->
            "actions"

        HistoryTab ->
            "history"


viewActions : Model -> Html Msg
viewActions model =
    div [ class "actions-section" ]
        [ div [ class "action-cards" ]
            [ viewDiceCard model
            , viewTablesCard model
            , viewIconsCard model
            ]
        ]


viewDiceCard : Model -> Html Msg
viewDiceCard model =
    div [ class "action-card" ]
        [ h2 [ class "card-title" ] [ text "🎲 Numbers" ]
        , div [ class "custom-roll" ]
            [ label [ class "input-label" ] [ text "Amount:" ]
            , input
                [ type_ "number"
                , class "roll-input"
                , value (String.fromInt model.dice_amount)
                , onInput UpdateDiceAmount
                , placeholder "Amount"
                ]
                []
            , label [ class "input-label" ] [ text "Sides:" ]
            , input
                [ type_ "number"
                , class "roll-input"
                , value (String.fromInt model.dice_sides)
                , onInput UpdateDiceSides
                , placeholder "Sides"
                ]
                []
            , label [ class "explode-label" ]
                [ input [ type_ "checkbox", checked model.dice_explode, onClick UpdateDiceExplode ] []
                , text "Exploding"
                ]
            , button [ type_ "button", class "primary-btn", onClick Roll ] [ text "Roll" ]
            ]
        , case model.dice_result of
            Just entries ->
                div [ class "action-result" ]
                    [ h3 [ class "action-result-title" ] [ text "Result" ]
                    , div [ class "action-result-body" ] (List.map (viewEntry model) entries)
                    ]

            Nothing ->
                text ""
        ]


viewTablesCard : Model -> Html Msg
viewTablesCard model =
    div [ class "action-card" ]
        [ h2 [ class "card-title" ] [ text "📜 Words" ]
        , div [ class "table-input" ]
            [ label [ class "input-label" ] [ text "Table:" ]
            , select [ class "table-select", onInput UpdateTable ]
                (List.map (\x -> option [ value x, selected (x == model.table) ] [ text x ]) (Dict.keys tables))
            ]
        , button [ type_ "button", class "primary-btn", onClick Ask ] [ text "Ask" ]
        , case model.table_result of
            Just entries ->
                div [ class "action-result" ]
                    [ h3 [ class "action-result-title" ] [ text "Result" ]
                    , div [ class "action-result-body" ] (List.map (viewEntry model) entries)
                    ]

            Nothing ->
                text ""
        ]


viewIconsCard : Model -> Html Msg
viewIconsCard model =
    div [ class "action-card" ]
        [ h2 [ class "card-title" ] [ text "🎨 Pictograms" ]
        , div [ class "icon-input" ]
        [ label [ class "input-label" ] [ text "Amount:" ]
            , input
                [ type_ "number"
                , class "roll-input"
                , value (String.fromInt model.icon_amount)
                , onInput UpdateIconAmount
                ]
                []
            ]
        , button [ type_ "button", class "primary-btn", onClick Show ] [ text "Show" ]
        , case model.icons_result of
            Just entries ->
                div [ class "action-result" ]
                    [ h3 [ class "action-result-title" ] [ text "Result" ]
                    , div [ class "action-result-body" ] (List.map (viewEntry model) entries)
                    ]

            Nothing ->
                text ""
        ]


viewHistory : Model -> Html Msg
viewHistory model =
    div [ class "history-section" ]
        [ div [ class "history-header" ]
            [ h2 [] [ text "History" ]
            , div [ class "history-actions" ]
                [ button [ type_ "button", class "ghost-btn", onClick ShareUrl ] [ text "Copy URL" ]
                , button [ type_ "button", class "ghost-btn", onClick Clear ] [ text "Clear" ]
                ]
            ]
        , if List.isEmpty model.journal then
            div [ class "empty-history" ] [ text "No history yet. Try rolling some dice!" ]

          else
            div [ class "history-entries" ]
                (List.map (viewHistoryEntry model) (List.filter (\item -> 
                    case item.action of
                        ResetSeed _ -> False
                        _ -> True
                ) model.journal))
        ]


viewHistoryEntry : Model -> HistoryItem -> Html Msg
viewHistoryEntry model item =
    let
        timestampLabel =
            formatTimestamp model.zone item.timestamp

        body =
            List.map (viewEntry model) item.entries

        hasIcons =
            List.any (
                \entry ->
                    case entry of
                        Img _ ->
                            True

                        _ ->
                            False
            ) item.entries

        ( actionLabel, actionDetails ) =
            case item.action of
                RollAction sides amount explode ->
                    ( "Numbers"
                    , String.fromInt amount
                        ++ "d"
                        ++ String.fromInt sides
                        ++ (if explode then
                                " (Exploding)"

                            else
                                ""
                           )
                    )

                AskAction table ->
                    ( "Words", "(" ++ table ++ ")" )

                ShowAction iconCount ->
                    ( "Pictograms", "(" ++ String.fromInt iconCount ++ " icons)" )

                ClearAction ->
                    ( "Clear", "" )

                ResetSeed value ->
                    ( "Seed", "(" ++ String.fromInt value ++ ")" )
    in
    button
        [ class "history-entry clickable"
        , classList
            [ ( "has-icons", hasIcons )
            , ( "text-only", not hasIcons )
            ]
        , type_ "button"
        , onClick (ReplayAction item.action)
        ]
        [ div [ class "history-meta" ]
            [ span [ class "history-action" ] [ text (actionLabel ++ " " ++ actionDetails) ]
            , span [ class "history-timestamp" ] [ text timestampLabel ]
            ]
        , div [ class "history-body" ] body
        ]


viewEntry : Model -> Entry -> Html Msg
viewEntry model entry =
    case entry of
        Txt list ->
            div [ class "history-text" ] (List.map (\x -> div [] [ text x ]) list)

        Img i ->
            List.Extra.getAt i model.icons
                |> Maybe.map
                    (\name ->
                        div [ class "history-icon" ]
                            [ Svg.svg
                                [ SvgAttr.width "100%"
                                , SvgAttr.height "100%"
                                , SvgAttr.preserveAspectRatio "xMidYMid meet"
                                ]
                                [ Svg.image
                                    [ SvgAttr.xlinkHref name
                                    , SvgAttr.width "100%"
                                    , SvgAttr.height "100%"
                                    , SvgAttr.preserveAspectRatio "xMidYMid meet"
                                    ]
                                    []
                                ]
                            ]
                    )
                |> Maybe.withDefault (text "")


formatTimestamp : Zone -> Posix -> String
formatTimestamp zone posix =
    let
        year =
            Time.toYear zone posix |> String.fromInt

        month =
            Time.toMonth zone posix
                |> (\m ->
                        case m of
                            Time.Jan ->
                                "01"

                            Time.Feb ->
                                "02"

                            Time.Mar ->
                                "03"

                            Time.Apr ->
                                "04"

                            Time.May ->
                                "05"

                            Time.Jun ->
                                "06"

                            Time.Jul ->
                                "07"

                            Time.Aug ->
                                "08"

                            Time.Sep ->
                                "09"

                            Time.Oct ->
                                "10"

                            Time.Nov ->
                                "11"

                            Time.Dec ->
                                "12"
                   )

        day =
            Time.toDay zone posix |> String.fromInt |> String.padLeft 2 '0'

        hour =
            Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0'

        minute =
            Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'

        second =
            Time.toSecond zone posix |> String.fromInt |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second



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
        , ( "theme", E.string (themeToString model.theme) )
        ]


decoder : D.Decoder PersistedState
decoder =
    D.map4 PersistedState
        (D.field "version" D.int
            |> D.map
                (\v ->
                    if v <= 0 then
                        1

                    else
                        v
                )
        )
        (D.field "seed" D.int)
        (D.field "actions" (D.list actionDecoder))
        (D.oneOf [ D.field "theme" (D.string |> D.map themeFromString), D.succeed Auto ])


themeToString : ThemeMode -> String
themeToString themeMode =
    case themeMode of
        Auto ->
            "auto"

        Light ->
            "light"

        Dark ->
            "dark"


themeFromString : String -> ThemeMode
themeFromString value =
    case String.toLower value of
        "light" ->
            Light

        "dark" ->
            Dark

        _ ->
            Auto


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
    , actions : List RecordedAction
    , dice_sides : Int
    , dice_amount : Int
    , dice_explode : Bool
    , table : String
    , icon_amount : Int
    , icons : List String
    , journal : List HistoryItem
    , dice_result : Maybe (List Entry)
    , table_result : Maybe (List Entry)
    , icons_result : Maybe (List Entry)
    , pendingHydration : Maybe PersistedState
    , versionConflict : Maybe PersistedState
    , key : Nav.Key
    , shareNotice : Maybe String
    , theme : ThemeMode
    , tab : Tab
    , zone : Zone
    }


type Entry
    = Txt (List String)
    | Img Int


type alias HistoryItem =
    { action : Action
    , timestamp : Posix
    , entries : List Entry
    }


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
            , dice_result = Nothing
            , table_result = Nothing
            , icons_result = Nothing
            , pendingHydration = actualPersisted
            , versionConflict = versionConflict
            , key = key
            , shareNotice = Nothing
            , theme = Auto
            , tab = ActionsTab
            , zone = Time.utc
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
                        , theme = state.theme
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
        , Task.perform GotZone Time.here
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
    | RecordActionWithTime Action Posix
    | ShareUrl
    | ClearShareNotice
    | DismissVersionConflict
    | ReloadWithNewVersion
    | ReplayAction Action
    | SetTheme ThemeMode
    | SwitchTab Tab
    | GotZone Zone
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
                            hydrateFromPersisted state model.key normalizedIcons model.zone

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
            let
                refreshed =
                    { model
                        | seedValue = seedInt
                        , seed = Random.initialSeed seedInt
                        , seedReady = True
                    }
            in
            ( refreshed, Task.perform (RecordActionWithTime (ResetSeed seedInt)) Time.now )

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
            ( model, Task.perform (RecordActionWithTime (RollAction model.dice_sides model.dice_amount model.dice_explode)) Time.now )

        UpdateTable table ->
            ( { model | table = table }, Cmd.none )

        Ask ->
            ( model, Task.perform (RecordActionWithTime (AskAction model.table)) Time.now )

        UpdateIconAmount amount ->
            case String.toInt amount of
                Just value ->
                    ( { model | icon_amount = value }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Show ->
            ( model, Task.perform (RecordActionWithTime (ShowAction model.icon_amount)) Time.now )

        Clear ->
            ( model
            , Cmd.batch
                [ Task.perform (RecordActionWithTime ClearAction) Time.now
                , Random.generate SeedRefreshed (Random.int 1 2147483646)
                ]
            )

        RecordActionWithTime action timestamp ->
            ( recordAction action timestamp model, Cmd.none )

        ShareUrl ->
            let
                stateEncoded =
                    encodeStateToString model

                shareableUrl =
                    "?state=" ++ stateEncoded

                clearNoticeCmd =
                    Task.perform (\_ -> ClearShareNotice) (Process.sleep 2500)
            in
            ( { model | shareNotice = Just "URL copied" }
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

        ReplayAction action ->
            ( model, Task.perform (RecordActionWithTime action) Time.now )

        SetTheme themeMode ->
            ( { model | theme = themeMode }, Cmd.none )

        SwitchTab tab ->
            ( { model | tab = tab }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

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


applyAction : Action -> Model -> ( Model, List Entry )
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

                    entry =
                        Txt
                            [ String.join ", " (List.map String.fromInt dice)
                            ]
                in
                ( { model | seed = nextSeed, dice_result = Just [ entry ], table_result = Nothing, icons_result = Nothing }, [ entry ] )

            else
                ( model, [] )

        AskAction table ->
            if model.seedReady then
                let
                    tableGenerator =
                        Dict.get table tables |> Maybe.withDefault (Random.constant "Table not found")

                    ( answer, nextSeed ) =
                        Random.step tableGenerator model.seed

                    entry =
                        Txt [ answer ]
                in
                ( { model | seed = nextSeed, table_result = Just [ entry ], dice_result = Nothing, icons_result = Nothing }, [ entry ] )

            else
                ( model, [] )

        ShowAction iconAmount ->
            if model.seedReady && not (List.isEmpty model.icons) then
                let
                    ( iconIndices, nextSeed ) =
                        Random.step (randomIconIndices model.icons iconAmount) model.seed

                    entries =
                        List.map Img iconIndices
                in
                ( { model | seed = nextSeed, icons_result = Just entries, dice_result = Nothing, table_result = Nothing }, entries )

            else
                ( model, [] )

        ClearAction ->
            ( { model | journal = [] }, [] )

        ResetSeed value ->
            ( { model
                | seedValue = value
                , seed = Random.initialSeed value
                , seedReady = True
              }
            , []
            )


recordAction : Action -> Posix -> Model -> Model
recordAction action timestamp model =
    let
        ( updatedModel, entries ) =
            applyAction action model

        historyItem =
            { action = action
            , timestamp = timestamp
            , entries = entries
            }

        updatedJournal =
            case action of
                ClearAction ->
                    []

                _ ->
                    historyItem :: updatedModel.journal
    in
    { updatedModel
        | actions = model.actions ++ [ { action = action, timestamp = timestamp } ]
        , journal = updatedJournal
    }


hydrateFromPersisted : PersistedState -> Nav.Key -> List String -> Zone -> Model
hydrateFromPersisted state key icons zone =
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
            , dice_result = Nothing
            , table_result = Nothing
            , icons_result = Nothing
            , pendingHydration = Nothing
            , versionConflict = Nothing
            , key = key
            , shareNotice = Nothing
            , theme = state.theme
            , tab = ActionsTab
            , zone = zone
            }

        applyRecordedAction recorded acc =
            let
                ( updated, entries ) =
                    applyAction recorded.action acc

                updatedJournal =
                    case recorded.action of
                        ClearAction ->
                            []

                        _ ->
                            { action = recorded.action, timestamp = recorded.timestamp, entries = entries } :: updated.journal
            in
            { updated
                | actions = acc.actions ++ [ recorded ]
                , journal = updatedJournal
            }
    in
    List.foldl applyRecordedAction baseModel state.actions


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
                    [ button [ type_ "button", class "btn primary", onClick ReloadWithNewVersion ]
                        [ text "Update & Load State" ]
                    , button [ type_ "button", class "btn ghost", onClick DismissVersionConflict ]
                        [ text "Continue Without Loading" ]
                    ]
                , p [ class "version-note" ]
                    [ text "Note: If offline or the app cannot update, you won't be able to load this state." ]
                ]
