port module Main exposing (..)

--   https://guide.elm-lang.org/effects/random.html

import Array exposing (..)
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Platform.Cmd as Cmd
import Random
import Tables exposing (..)

version : String
version = "0.1.1"

-- MAIN


main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


banner : String
banner = "Welcome to the Dice Roller (v" ++ version ++ ")!\n\n"


type alias Model =
    { sides : Int
    , amount : Int
    , table : String
    , journal : String
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { sides = 6, amount = 1, table = Dict.keys tables |> List.head |> Maybe.withDefault "", journal = banner }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateSides String
    | UpdateAmount String
    | Roll
    | NewDice (List Int)
    | UpdateTable String
    | Ask
    | NewAnswer Int
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSides sides ->
            ( { model | sides = String.toInt sides |> Maybe.withDefault model.sides }, Cmd.none )

        UpdateAmount amount ->
            ( { model | amount = String.toInt amount |> Maybe.withDefault model.amount }, Cmd.none )

        Roll ->
            ( model, Random.generate NewDice (Random.list model.amount (Random.int 1 model.sides)) )

        NewDice dice ->
            ( { model
                | journal =
                    String.fromInt model.amount
                        ++ "d"
                        ++ String.fromInt model.sides
                        ++ ": "
                        ++ String.join ", " (List.map String.fromInt dice)
                        ++ "\n"
                        ++ "Min die: "
                        ++ String.fromInt (List.minimum dice |> Maybe.withDefault 0)
                        ++ "\n"
                        ++ "Max die: "
                        ++ String.fromInt (List.maximum dice |> Maybe.withDefault 0)
                        ++ "\n"
                        ++ "Sum: "
                        ++ String.fromInt (List.sum dice)
                        ++ "\n\n"
                        ++ model.journal
              }
            , Cmd.none
            )

        UpdateTable table ->
            ( { model | table = table }, Cmd.none )

        Ask ->
            ( model, Random.generate NewAnswer (Random.int 0 ((Dict.get model.table tables |> Maybe.withDefault Array.empty |> Array.length) - 1)) )

        NewAnswer answer ->
            ( { model
                | journal =
                    "Table: "
                        ++ model.table
                        ++ "\n"
                        ++ "Response: "
                        ++ (Dict.get model.table tables |> Maybe.withDefault Array.empty |> Array.get answer |> Maybe.withDefault "")
                        ++ "\n\n"
                        ++ model.journal
              }
            , Cmd.none
            )

        Clear ->
            ( { model | journal = banner }, Cmd.batch [ clearStorage (), Cmd.none ] )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ section []
                [ h1 [class "title"] [text ("Dice Roller (v" ++ version ++ ")")]
                , Html.form []
                    [ fieldset [class "my-inline-container"]
                        [ legend [] [text "Dices"]
                        , div [class "my-input"]
                            [ label [for "sides"] [ text "Sides" ]    
                            , input [id "sides", type_ "text" , placeholder (String.fromInt model.sides), onInput UpdateSides] []
                            ]
                        , div [class "my-input"]
                            [ label [for "amount"] [ text "Amount" ]
                            , input [id "amount", type_ "text", placeholder (String.fromInt model.amount), onInput UpdateAmount] []
                            ]
                        , button [class "my-btn", onClick Roll] [ text "Roll" ]
                        ]
                    , fieldset [class "my-inline-container"]
                        [ legend [] [text "Tables"]
                        , div [class "my-select"]
                            [ label [for "tables"] [ text "Table" ]
                            , select [id "tables", onInput UpdateTable]
                                (List.map (\x -> option [ value x, selected (x == model.table) ] [ text x ]) (Dict.keys tables))
                            ]
                        , button [class "my-btn", onClick Ask] [ text "Ask" ]
                        ]
                    , button [class "my-btn", onClick Clear] [ text "Clear" ]
                    ]
                ]
            ]
        , main_ []
            [ h1 [class "title"] [ text "Journal" ]
            , pre [] [ text model.journal ]
            ]
        ]



-- PORTS


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
        [ ( "sides", E.int model.sides )
        , ( "amount", E.int model.amount )
        , ( "table", E.string model.table )
        , ( "journal", E.string model.journal )
        ]


decoder : D.Decoder Model
decoder =
    D.map4 Model
        (D.field "sides" D.int)
        (D.field "amount" D.int)
        (D.field "table" D.string)
        (D.field "journal" D.string)
