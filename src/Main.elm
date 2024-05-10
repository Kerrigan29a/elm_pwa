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



-- MAIN


main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


banner =
    """
▓█████▄  ██▓ ▄████▄  ▓█████                       
▒██▀ ██▌▓██▒▒██▀ ▀█  ▓█   ▀                       
░██   █▌▒██▒▒▓█    ▄ ▒███                         
░▓█▄   ▌░██░▒▓▓▄ ▄██▒▒▓█  ▄                       
░▒████▓ ░██░▒ ▓███▀ ░░▒████▒                      
 ▒▒▓  ▒ ░▓  ░ ░▒ ▒  ░░░ ▒░ ░                      
 ░ ▒  ▒  ▒ ░  ░  ▒    ░ ░  ░                      
 ░ ░  ░  ▒ ░░           ░                         
   ░     ░  ░ ░         ░  ░                      
 ░          ░                                     
 ██▀███   ▒█████   ██▓     ██▓    ▓█████  ██▀███  
▓██ ▒ ██▒▒██▒  ██▒▓██▒    ▓██▒    ▓█   ▀ ▓██ ▒ ██▒
▓██ ░▄█ ▒▒██░  ██▒▒██░    ▒██░    ▒███   ▓██ ░▄█ ▒
▒██▀▀█▄  ▒██   ██░▒██░    ▒██░    ▒▓█  ▄ ▒██▀▀█▄  
░██▓ ▒██▒░ ████▓▒░░██████▒░██████▒░▒████▒░██▓ ▒██▒
░ ▒▓ ░▒▓░░ ▒░▒░▒░ ░ ▒░▓  ░░ ▒░▓  ░░░ ▒░ ░░ ▒▓ ░▒▓░
  ░▒ ░ ▒░  ░ ▒ ▒░ ░ ░ ▒  ░░ ░ ▒  ░ ░ ░  ░  ░▒ ░ ▒░
  ░░   ░ ░ ░ ░ ▒    ░ ░     ░ ░      ░     ░░   ░ 
   ░         ░ ░      ░  ░    ░  ░   ░  ░   ░     
"""


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
            [ div []
                [ label [] [ text "Sides: ", input [ placeholder (String.fromInt model.sides), onInput UpdateSides ] [] ]
                , label [] [ text "Amount: ", input [ placeholder (String.fromInt model.amount), onInput UpdateAmount ] [] ]
                , button [ onClick Roll ] [ text "Roll" ]
                ]
            , div []
                [ label []
                    [ text "Table: "
                    , select [ onInput UpdateTable ]
                        (List.map (\x -> option [ value x, selected (x == model.table) ] [ text x ]) (Dict.keys tables))
                    ]
                , button [ onClick Ask ] [ text "Ask" ]
                ]
            , div []
                [ button [ onClick Clear ] [ text "Clear" ]
                ]
            ]
        , main_ []
            [ pre [] [ text model.journal ]
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
