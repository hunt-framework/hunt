module Search.Model
    exposing
        ( Model
        , init
        )

import Autocomplete
import Debounce exposing (Debounce)
import Http
import Json.Decode as Json
import Search.Types exposing (LimitedResult, RankedDoc, Document, emptyResult)


-- MODEL


type alias Model =
    { query : String
    , autoState : Autocomplete.State
    , debounce : Debounce String
    , rankedDocs : LimitedResult (RankedDoc Document)
    , completions : List ( String, Float )
    , howManyCompletionsToShow : Int
    , menuVisible : Bool
    , queryError : Maybe Http.Error
    }


init : Model
init =
    { query = ""
    , autoState = Autocomplete.empty
    , debounce = Debounce.init
    , rankedDocs = emptyResult
    , completions = []
    , howManyCompletionsToShow = 5
    , menuVisible = False
    , queryError = Nothing
    }
