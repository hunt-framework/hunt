module Search.View
    exposing
        ( view
        )

import Html exposing (Html, section, div, h2, h3, h5, code, text, main_, input, a, button, table, tbody, tr, td, th, thead, em, span, img)
import Html.Attributes exposing (id, class, classList, type_, name, value, href, placeholder, src, width, height)
import Html.Events exposing (onInput, onClick, onBlur, on, keyCode)
import Date exposing (Date, Month(..))
import Json.Decode as Json
import Search.Model exposing (..)
import Search.Messages exposing (..)
import Search.Types exposing (..)
import Autocomplete


-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ class "search-page" ]
        [ section
            [ class "search" ]
            [ viewSearchField model
            , viewResultTable model.query model.completions model.rankedDocs
            ]
        , viewHelp
        ]



-- VIEW SEARCH


viewSearchField : Model -> Html Msg
viewSearchField model =
    let
        dec code =
            if code == escCode then
                CloseAutocomplete
            else
                NoOp
    in
        section
            [ class "search__field" ]
            [ input
                [ type_ "text"
                , name "query"
                , value model.query
                , onInput SetQuery
                , on "keydown" (Json.map dec keyCode)
                , placeholder "Search"
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
            , button
                [ class "search__field__btn"
                , onClick Search
                ]
                [ img [ src "images/search.svg", width 20, height 20 ] []
                ]
            ]


autocompleteConfig : Autocomplete.ViewConfig ( String, Float )
autocompleteConfig =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "search__field__menu__item", True )
                    , ( "search__field__menu__item--selected", keySelected )
                    ]
                ]
            , children = [ text (Tuple.first item) ]
            }
    in
        Autocomplete.viewConfig
            { toId = Tuple.first
            , ul = [ class "search__field__menu" ]
            , li = customizedLi
            }



-- VIEW RESULT TABLE


viewResultTable : String -> List ( String, Float ) -> LimitedResult (RankedDoc Document) -> Html Msg
viewResultTable query completions result =
    div
        [ class "results" ]
        (viewRankedDocs query completions result)


viewRankedDocs : String -> List ( String, Float ) -> LimitedResult (RankedDoc Document) -> List (Html msg)
viewRankedDocs query completions result =
    case ( query, completions, result.data ) of
        ( "", _, [] ) ->
            [ text "No search yet." ]

        ( _, [], [] ) ->
            [ text "No documents found. Did you run the examples in our "
            , a [ href "#quickstart" ] [ text "quickstart" ]
            , text " guide?"
            ]

        _ ->
            List.map viewRankedDoc result.data


viewRankedDoc : RankedDoc Document -> Html msg
viewRankedDoc doc =
    div
        [ class "ranked-doc" ]
        [ h3 [ class "ranked-doc__title" ] [ text doc.uri ]
        , div [ class "context" ]
            [ span [ class "context__name" ] [ text "Score " ]
            , span [ class "context__value" ] [ text (toString doc.score) ]
            ]
        , div [ class "context" ]
            [ span [ class "context__name" ] [ text "Subject " ]
            , span [ class "context__value" ] [ text doc.description.subject ]
            ]
        , div [ class "context" ]
            [ span [ class "context__name" ] [ text "Published " ]
            , span [ class "context__value" ] [ text (formatDate doc.description.publishDate) ]
            ]
        , div [ class "context" ]
            [ span [ class "context__name" ] [ text "Content " ]
            , span [ class "context__value" ] [ text doc.description.content ]
            ]
        , div [ class "context" ]
            [ span [ class "context__name" ] [ text "Location " ]
            , span [ class "context__value" ]
                [ text (toString doc.description.location.latitude)
                , text ", "
                , text (toString doc.description.location.longitude)
                ]
            ]
        ]


formatDate : Date -> String
formatDate date =
    let
        month =
            case Date.month date of
                Jan ->
                    "January"

                Feb ->
                    "February"

                Mar ->
                    "March"

                Apr ->
                    "April"

                May ->
                    "May"

                Jun ->
                    "June"

                Jul ->
                    "July"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "October"

                Nov ->
                    "November"

                Dec ->
                    "December"

        day =
            Date.day date
                |> toString
                |> String.padLeft 2 '0'
    in
        month ++ " " ++ day ++ ", " ++ (toString (Date.year date))



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



-- HELPERS


escCode : Int
escCode =
    27
