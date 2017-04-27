module Quickstart.Requests
    exposing
        ( createContexts
        , insertDocs
        )

import Http
import Hunt.Http as Http exposing (BaseUrl)
import Json.Decode as Json


-- REQUESTS


createContexts : BaseUrl -> Json.Value -> Http.Request ()
createContexts baseUrl json =
    Http.postJson baseUrl
        { path = "eval"
        , body = Http.jsonBody json
        , decoder = Json.succeed ()
        }


insertDocs : BaseUrl -> Json.Value -> Http.Request ()
insertDocs baseUrl json =
    Http.postJson baseUrl
        { path = "eval"
        , body = Http.jsonBody json
        , decoder = Json.succeed ()
        }
