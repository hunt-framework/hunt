module Search.View
    exposing
        ( view
        )

import Html exposing (Html, section, div, h2, h5, code, text, main_, input, a)
import Html.Attributes exposing (id, class, classList, type_, name, value, href)
import Html.Events exposing (onInput)
import Search.Model exposing (..)
import Search.Messages exposing (..)
import Autocomplete


-- VIEW


view : Model -> Html Msg
view model =
    main_
        []
        [ a [ href "#quickstart" ] [ text "Quickstart" ]
        , viewSearch model
        , viewHelp
        ]



-- VIEW SEARCH


viewSearch : Model -> Html Msg
viewSearch model =
    section
        [ class "search" ]
        [ input
            [ type_ "text"
            , name "query"
            , value model.query
            , onInput SetQuery
            ]
            []
        , if model.menuVisible then
            Html.map SetAutocompleteState <|
                Autocomplete.view
                    autocompleteConfig
                    model.howManyCompletionsToShow
                    model.autoState
                    model.completions
          else
            text ""
        ]


autocompleteConfig : Autocomplete.ViewConfig ( String, Float )
autocompleteConfig =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete__list__item", True )
                    , ( "autocomplete__list__item--selected", keySelected )
                    ]
                ]
            , children = [ text (Tuple.first item) ]
            }
    in
        Autocomplete.viewConfig
            { toId = Tuple.first
            , ul = [ class "autocomplete__list" ]
            , li = customizedLi
            }



-- VIEW HELP


viewHelp : Html msg
viewHelp =
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
