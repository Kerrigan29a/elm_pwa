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
import Random
import Svg exposing (image, svg)
import Svg.Attributes as SvgAttr
import Tables exposing (..)
import Url
import Url.Parser as Parser exposing (Parser)
import Url.Parser.Query as Query


version : String
version =
    "0.5.0"



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


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "dice_sides", E.int model.dice_sides )
        , ( "dice_amount", E.int model.dice_amount )
        , ( "dice_explode", E.bool model.dice_explode )
        , ( "table", E.string model.table )
        , ( "icon_amount", E.int model.icon_amount )
        , ( "journal", E.list entryEncoder model.journal )
        ]


decoder : Nav.Key -> D.Decoder Model
decoder key =
    D.map6 (\ds da de t ia j -> Model ds da de t ia [] j key)
        (D.field "dice_sides" D.int)
        (D.field "dice_amount" D.int)
        (D.field "dice_explode" D.bool)
        (D.field "table" D.string)
        (D.field "icon_amount" D.int)
        (D.field "journal" (D.list entryDecoder))


entryEncoder : Entry -> E.Value
entryEncoder entry =
    case entry of
        Txt list ->
            E.object [ ( "Txt", E.list E.string list ) ]

        Img string ->
            E.object [ ( "Img", E.string string ) ]


entryDecoder : D.Decoder Entry
entryDecoder =
    D.oneOf
        [ D.field "Txt" (D.map Txt (D.list D.string))
        , D.field "Img" (D.map Img D.string)
        ]



-- MODEL


type alias Model =
    { dice_sides : Int
    , dice_amount : Int
    , dice_explode : Bool
    , table : String
    , icon_amount : Int
    , icons : List String
    , journal : List Entry
    , key : Nav.Key
    }


type Entry
    = Txt (List String)
    | Img String


init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        -- Try URL first, then localStorage
        modelFromUrl =
            parseStateFromUrl url key

        modelFromStorage =
            case D.decodeValue (decoder key) flags of
                Ok model ->
                    Just model

                Err _ ->
                    Nothing

        defaultModel =
            { dice_sides = 6
            , dice_amount = 1
            , dice_explode = False
            , table = Dict.keys tables |> List.head |> Maybe.withDefault ""
            , icon_amount = 3
            , icons = []
            , journal = []
            , key = key
            }

        initialModel =
            case modelFromUrl of
                Just urlModel ->
                    urlModel

                Nothing ->
                    case modelFromStorage of
                        Just storageModel ->
                            storageModel

                        Nothing ->
                            defaultModel
    in
    ( initialModel
    , Http.get
        { url = "./icons/icons.json"
        , expect = Http.expectJson GotIcons iconsDecoder
        }
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


parseStateFromUrl : Url.Url -> Nav.Key -> Maybe Model
parseStateFromUrl url key =
    url.query
        |> Maybe.andThen
            (\query ->
                String.split "=" query
                    |> List.Extra.getAt 1
                    |> Maybe.andThen (decodeStateFromString key)
            )


decodeStateFromString : Nav.Key -> String -> Maybe Model
decodeStateFromString key encoded =
    encoded
        |> Url.percentDecode
        |> Maybe.andThen
            (\decoded ->
                Base64.decode decoded
                    |> Result.toMaybe
            )
        |> Maybe.andThen (D.decodeString (urlStateDecoder key) >> Result.toMaybe)


encodeStateToString : Model -> String
encodeStateToString model =
    encodeUrlState model
        |> E.encode 0
        |> Base64.encode
        |> Url.percentEncode


urlStateDecoder : Nav.Key -> D.Decoder Model
urlStateDecoder key =
    D.map6 (\ds da de t ia j -> Model ds da de t ia [] j key)
        (D.field "dice_sides" D.int)
        (D.field "dice_amount" D.int)
        (D.field "dice_explode" D.bool)
        (D.field "table" D.string)
        (D.field "icon_amount" D.int)
        (D.field "journal" (D.list urlEntryDecoder))


encodeUrlState : Model -> E.Value
encodeUrlState model =
    E.object
        [ ( "dice_sides", E.int model.dice_sides )
        , ( "dice_amount", E.int model.dice_amount )
        , ( "dice_explode", E.bool model.dice_explode )
        , ( "table", E.string model.table )
        , ( "icon_amount", E.int model.icon_amount )
        , ( "journal", E.list urlEntryEncoder model.journal )
        ]


urlEntryEncoder : Entry -> E.Value
urlEntryEncoder entry =
    case entry of
        Txt list ->
            E.object [ ( "Txt", E.list E.string list ) ]

        Img idxStr ->
            E.object [ ( "Img", E.string idxStr ) ]


urlEntryDecoder : D.Decoder Entry
urlEntryDecoder =
    D.oneOf
        [ D.field "Txt" (D.map Txt (D.list D.string))
        , D.field "Img" (D.map Img D.string)
        ]



-- UPDATE


type Msg
    = GotIcons (Result Http.Error (List String))
    | UpdateDiceSides String
    | UpdateDiceAmount String
    | UpdateDiceExplode
    | Roll
    | NewDice (List Int)
    | UpdateTable String
    | Ask
    | NewAnswer String
    | UpdateIconAmount String
    | Show
    | NewIcon (List Int)
    | Clear
    | ShareUrl
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIcons (Ok iconsList) ->
            -- let
            --     _ =
            --         Debug.log "Icons loaded successfully:" iconsList
            -- in
            ( { model | icons = iconsList }, Cmd.none )

        GotIcons (Err err) ->
            -- let
            --     _ =
            --         Debug.log "Error loading icons:" err
            -- in
            ( model, Cmd.none )

        UpdateDiceSides sides ->
            ( { model | dice_sides = String.toInt sides |> Maybe.withDefault model.dice_sides }, Cmd.none )

        UpdateDiceAmount amount ->
            ( { model | dice_amount = String.toInt amount |> Maybe.withDefault model.dice_amount }, Cmd.none )

        UpdateDiceExplode ->
            ( { model | dice_explode = not model.dice_explode }, Cmd.none )

        Roll ->
            if model.dice_explode then
                ( model, Random.generate NewDice (rollExplodingDice model.dice_amount model.dice_sides) )

            else
                ( model, Random.generate NewDice (Random.list model.dice_amount (Random.int 1 model.dice_sides)) )

        NewDice dice ->
            ( { model
                | journal =
                    Txt
                        [ "Dice ("
                            ++ String.fromInt model.dice_amount
                            ++ "d"
                            ++ String.fromInt model.dice_sides
                            ++ (if model.dice_explode then
                                    "!"

                                else
                                    ""
                               )
                            ++ "): "
                            ++ String.join ", " (List.map String.fromInt dice)
                        ]
                        :: model.journal
              }
            , Cmd.none
            )

        UpdateTable table ->
            ( { model | table = table }, Cmd.none )

        Ask ->
            ( model, Random.generate NewAnswer (Dict.get model.table tables |> Maybe.withDefault (Random.constant "Table not found")) )

        NewAnswer answer ->
            ( { model
                | journal = Txt [ "Table (" ++ model.table ++ "): " ++ answer ] :: model.journal
              }
            , Cmd.none
            )

        UpdateIconAmount amount ->
            ( { model | icon_amount = String.toInt amount |> Maybe.withDefault model.icon_amount }, Cmd.none )

        Show ->
            ( model, Random.generate NewIcon (randomIconIndices model.icons model.icon_amount) )

        NewIcon iconIndices ->
            ( { model
                | journal =
                    Txt [ "Icons: " ] :: List.map (\idx -> Img (String.fromInt idx)) iconIndices ++ model.journal
              }
            , Cmd.none
            )

        Clear ->
            ( { model | journal = [] }, Cmd.batch [ clearStorage (), Cmd.none ] )

        ShareUrl ->
            let
                stateEncoded =
                    encodeStateToString model

                shareableUrl =
                    "?state=" ++ stateEncoded
            in
            ( model, Nav.pushUrl model.key shareableUrl )

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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dice Roller (v" ++ version ++ ")"
    , body =
        [ main_ []
            [ header []
                [ h1 [ class "title" ] [ text ("Dice Roller (v" ++ version ++ ")") ]
                , Html.form []
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
                        , button [ class "my-btn", onClick Roll ] [ text "Roll" ]
                        ]
                    , fieldset [ class "my-inline-container" ]
                        [ legend [] [ text "Tables" ]
                        , div [ class "my-select" ]
                            [ label [ for "tables" ] [ text "Table" ]
                            , select [ id "tables", onInput UpdateTable ]
                                (List.map (\x -> option [ value x, selected (x == model.table) ] [ text x ]) (Dict.keys tables))
                            ]
                        , button [ class "my-btn", onClick Ask ] [ text "Ask" ]
                        ]
                    , fieldset [ class "my-inline-container" ]
                        [ legend [] [ text "Icons" ]
                        , div [ class "my-input" ]
                            [ label [ for "icon_amount" ] [ text "Amount" ]
                            , input [ id "icon_amount", type_ "text", placeholder (String.fromInt model.icon_amount), onInput UpdateIconAmount ] []
                            ]
                        , button [ class "my-btn", onClick Show ] [ text "Show" ]
                        ]
                    , button [ class "my-btn", onClick Clear ] [ text "Clear" ]
                    , button [ class "my-btn", onClick ShareUrl ] [ text "Share" ]
                    ]
                ]
            , article []
                [ h1 [ class "title" ] [ text "Journal" ]
                , div []
                    (List.map
                        (\entry ->
                            case entry of
                                Txt list ->
                                    p [] (List.map text list)

                                Img idxStr ->
                                    let
                                        iconSrc =
                                            String.toInt idxStr
                                                |> Maybe.andThen (\idx -> List.Extra.getAt idx model.icons)
                                                |> Maybe.withDefault ""
                                    in
                                    svg [ SvgAttr.width "100", SvgAttr.height "100" ]
                                        [ image [ SvgAttr.xlinkHref iconSrc, SvgAttr.width "100", SvgAttr.height "100" ] [] ]
                        )
                        model.journal
                    )
                ]
            ]
        ]
    }
