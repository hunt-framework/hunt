module Quickstart.Messages
    exposing
        ( Msg(..)
        )

import Http
import Json.Decode as Json


-- MESSAGES


type Msg
    = CreateContexts Json.Value
    | CreateContextsResult (Result Http.Error ())
    | InsertDocs Json.Value
    | InsertDocsResult (Result Http.Error ())
