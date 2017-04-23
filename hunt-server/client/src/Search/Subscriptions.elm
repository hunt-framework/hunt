module Search.Subscriptions
    exposing
        ( subscriptions
        )

import Autocomplete
import Search.Model exposing (..)
import Search.Messages exposing (..)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutocompleteState Autocomplete.subscription
