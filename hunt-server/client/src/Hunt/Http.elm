module Hunt.Http
    exposing
        ( BaseUrl
        , parseBaseUrl
        , getJson
        )

import Http exposing (Request, encodeUri)
import Json.Decode as Json


-- HTTP


type BaseUrl
    = BaseUrl String


parseBaseUrl : String -> BaseUrl
parseBaseUrl =
    BaseUrl



-- REQUESTS


getJson :
    BaseUrl
    -> { path : String, queryParams : List ( String, String ), decoder : Json.Decoder a }
    -> Request a
getJson (BaseUrl baseUrl) { path, queryParams, decoder } =
    let
        url =
            baseUrl ++ "/" ++ path ++ toQueryParams queryParams
    in
        Http.get url decoder



-- HELPERS


toQueryParams : List ( String, String ) -> String
toQueryParams params =
    case params of
        [] ->
            ""

        _ ->
            params
                |> List.map (\( a, b ) -> encodeUri a ++ "=" ++ encodeUri b)
                |> String.join "&"
                |> String.cons '?'
