module Search.Json.Decode
    exposing
        ( limitedResult
        , rankedDoc
        , document
        )

import Json.Decode as Json
import Date exposing (Date)
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


document : Json.Decoder Document
document =
    Json.succeed Document
        |: Json.field "subject" Json.string
        |: Json.field "date" date
        |: Json.field "content" Json.string
        |: Json.field "location" location


location : Json.Decoder Location
location =
    let
        stringToFloats str =
            str
                |> String.split " "
                |> List.map String.toFloat

        dec str =
            case stringToFloats str of
                (Ok latitude) :: (Ok longitude) :: [] ->
                    Json.succeed (Location latitude longitude)

                (Err latitudeErr) :: _ ->
                    Json.fail latitudeErr

                _ :: (Err longitudeErr) :: _ ->
                    Json.fail longitudeErr

                _ ->
                    Json.fail (str ++ " does not correspond to '{latitude} {longitude}' format")
    in
        Json.andThen dec Json.string


date : Json.Decoder Date
date =
    let
        dec str =
            case Date.fromString str of
                Ok result ->
                    Json.succeed result

                Err err ->
                    Json.fail err
    in
        Json.andThen dec Json.string



-- HELPERS


(|:) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(|:) a v =
    Json.andThen (\f -> Json.map f v) a
