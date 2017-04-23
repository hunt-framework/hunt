module Search.Requests exposing (..)

import Http exposing (Request)
import Hunt.Http as Http exposing (BaseUrl)
import Json.Decode as Decode
import Search.Types exposing (LimitedResult, RankedDoc)
import Search.Json.Decode as Decode


-- REQUESTS


{-|
-}
search : BaseUrl -> String -> Request (LimitedResult (RankedDoc Decode.Value))
search baseUrl query =
    Http.getJson baseUrl
        { path = "search/" ++ query
        , queryParams = []
        , decoder = Decode.limitedResult (Decode.rankedDoc Decode.value)
        }


{-|
-}
complete : BaseUrl -> String -> Request (List ( String, Float ))
complete baseUrl query =
    Http.getJson baseUrl
        { path = "completion/" ++ query
        , queryParams = []
        , decoder =
            Decode.list
                (Decode.map2 (,)
                    (Decode.index 0 Decode.string)
                    (Decode.index 1 Decode.float)
                )
        }
