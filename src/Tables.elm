module Tables exposing (tables)

import Array
import Dict exposing (..)


tables = Dict.fromList
    [ ( "oracle", oracle )
    , ( "alignment", alignment )
    ]

oracle = Array.fromList
    [ "No, and"
    , "No"
    , "No, but"
    , "Yes, but"
    , "Yes"
    , "Yes, and"
    ]

alignment = Array.fromList
    [ "Lawful Good"
    , "Neutral Good"
    , "Chaotic Good"
    , "Lawful Neutral"
    , "True Neutral"
    , "Chaotic Neutral"
    , "Lawful Evil"
    , "Neutral Evil"
    , "Chaotic Evil"
    ]
