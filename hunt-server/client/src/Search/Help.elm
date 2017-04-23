module Help
    exposing
        ( view
        )

import Html exposing (Html, div, section, text, h2, code, h4, h5, span, br)
import Html.Attributes exposing (id, class)


-- VIEW


view : Html msg
view =
    section
        [ class "help" ]
        [ h2 [ class "help__title" ] [ text "Help" ]
        , helpOption "case-sensitive query"
            [ example "!chris"
            , example "!Chris"
            ]
        , helpOption "fuzzy query"
            [ example "~chris"
            , example "~hcris"
            ]
        , helpOption "phrase query"
            [ example "\"this is a phrase\""
            ]
        , helpOption "word with quoting"
            [ example "'a -> m b'"
            ]
        , helpOption "case sensitive word query with quoting"
            [ example "!'a -> m b'"
            ]
        , helpOption "brackets"
            [ example "(...)"
            ]
        , helpOption "context-sensitive"
            [ example "context:query"
            , example "people:chris"
            , example "developer,students,people:chris"
            ]
        , helpOption "combinators"
            [ example "AND"
            , example "OR"
            , example "AND"
            , example "NOT"
            ]
        , helpOption "word sequence search (++)"
            [ example "Charles ++ M ++ Schulz"
            , example "Ginger ++ and ++ Fred"
            ]
        , helpOption "multiple word search (FOLLOW)"
            [ example "Charles FOLLOW 2 Schulz"
            ]
        , helpOption "multiple word search (NEAR)"
            [ example "Fred NEAR 2 Ginger"
            ]
        , helpOption "boosting"
            [ example "toAscList^1.5 OR toList"
            ]
        , helpOption "range query"
            [ example "[ 2014-02-10 TO 2012-02-16 ]"
            ]
        ]



-- HELP OPTIONS


helpOption : String -> List (Html msg) -> Html msg
helpOption title content =
    section
        [ id title
        , class "help__option"
        ]
        [ h5 [ class "help__option__title" ] [ text title ]
        , div [ class "help__option__content" ] content
        ]


example : String -> Html msg
example content =
    div
        [ class "help__option__example" ]
        [ code
            [ class "help__option__example__code" ]
            [ text content
            ]
        ]
