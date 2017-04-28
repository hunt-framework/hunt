module Search.Messages
    exposing
        ( Msg(..)
        )

import Autocomplete
import Debounce
import Http
import Search.Types exposing (LimitedResult, RankedDoc, Document)
import Json.Decode as Json


-- MESSAGES


type Msg
    = SetQuery String
    | SetQueryAndClose String
    | Search
    | SetAutocompleteState Autocomplete.Msg
    | SetRankedDocs (Result Http.Error (LimitedResult (RankedDoc Document)))
    | SetCompletions (Result Http.Error (List ( String, Float )))
    | SetCompletionDebounce Debounce.Msg
    | CloseAutocomplete
    | NoOp
