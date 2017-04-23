module Search.Types
    exposing
        ( RankedDoc
        , LimitedResult
        , emptyResult
        )

import Json.Decode as Decode


-- TYPES


type alias LimitedResult a =
    { offset : Int
    , max : Int
    , count : Int
    , result : List a
    }


emptyResult : LimitedResult a
emptyResult =
    LimitedResult 0 0 0 []


type alias RankedDoc a =
    { uri : String
    , score : Float
    , description : a
    }
