port module Main exposing (..)

--   https://guide.elm-lang.org/effects/random.html

import Array exposing (..)
import Browser
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


version : String
version =
    "0.4.3"



-- MAIN


main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , view = view
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


decoder : D.Decoder Model
decoder =
    D.map7 Model
        (D.field "dice_sides" D.int)
        (D.field "dice_amount" D.int)
        (D.field "dice_explode" D.bool)
        (D.field "table" D.string)
        (D.field "icon_amount" D.int)
        (D.succeed [])
        -- Le asigna una lista vacía a icons
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
    }


type Entry
    = Txt (List String)
    | Img String


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { dice_sides = 6
            , dice_amount = 1
            , dice_explode = False
            , table = Dict.keys tables |> List.head |> Maybe.withDefault ""
            , icon_amount = 3
            , icons = []
            , journal = []
            }
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
    | NewIcon (List String)
    | Clear


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
            ( model, Random.generate NewIcon (randomIcons model.icons model.icon_amount) )

        NewIcon icons ->
            ( { model
                | journal =
                    Txt [ "Icons: " ] :: List.map (\icon -> Img icon) icons ++ model.journal
              }
            , Cmd.none
            )

        Clear ->
            ( { model | journal = [] }, Cmd.batch [ clearStorage (), Cmd.none ] )



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



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
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

                            Img src ->
                                svg [ SvgAttr.width "100", SvgAttr.height "100" ]
                                    [ image [ SvgAttr.xlinkHref src, SvgAttr.width "100", SvgAttr.height "100" ] [] ]
                    )
                    model.journal
                )
            ]
        ]
