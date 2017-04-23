module Search.Json.Decode
    exposing
        ( limitedResult
        , rankedDoc
        )

import Json.Decode as Json
import Search.Types exposing (..)


-- DECODERS


limitedResult : Json.Decoder a -> Json.Decoder (LimitedResult a)
limitedResult decoder =
    Json.succeed LimitedResult
        |: Json.field "offset" Json.int
        |: Json.field "max" Json.int
        |: Json.field "count" Json.int
        |: Json.field "result" (Json.list decoder)


rankedDoc : Json.Decoder a -> Json.Decoder (RankedDoc a)
rankedDoc decoder =
    Json.succeed RankedDoc
        |: Json.field "uri" Json.string
        |: Json.field "score" Json.float
        |: Json.field "description" decoder



-- HELPERS


(|:) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(|:) a v =
    Json.andThen (\f -> Json.map f v) a
