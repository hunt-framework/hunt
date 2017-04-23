module Quickstart.Model
    exposing
        ( Model
        , init
        )

import Http


-- MODEL


type alias Model =
    { contextError : Maybe Http.Error
    , insertError : Maybe Http.Error
    }


init : Model
init =
    Model Nothing Nothing
