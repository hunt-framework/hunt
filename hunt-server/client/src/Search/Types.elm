module Search.Types
    exposing
        ( RankedDoc
        , LimitedResult
        , Document
        , Location
        , emptyResult
        )

import Json.Decode as Decode
import Date exposing (Date)


-- TYPES


type alias LimitedResult a =
    { offset : Int
    , max : Int
    , count : Int
    , data : List a
    }


emptyResult : LimitedResult a
emptyResult =
    LimitedResult 0 0 0 []


type alias RankedDoc a =
    { uri : String
    , score : Float
    , description : a
    }


type alias Document =
    { subject : String
    , publishDate : Date
    , content : String
    , location : Location
    }


type alias Location =
    { latitude : Float
    , longitude : Float
    }
