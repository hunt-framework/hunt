module Page
    exposing
        ( Page(..)
        , urlParser
        , toPath
        )

import Navigation
import UrlParser as Url exposing ((</>), s, top)


-- PAGE


type Page
    = Index
    | Quickstart


urlParser : Url.Parser (Page -> a) a
urlParser =
    Url.oneOf
        [ Url.map Index top
        , Url.map Quickstart (s "quickstart")
        ]


toPath : Page -> String
toPath page =
    case page of
        Index ->
            ""

        Quickstart ->
            "quickstart"
