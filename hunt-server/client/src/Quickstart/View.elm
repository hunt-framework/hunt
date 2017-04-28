module Quickstart.View
    exposing
        ( view
        )

import Html exposing (Html, article, section, div, button, pre, code, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (Value)
import Json.Encode as Json
import Markdown
import Quickstart.Model exposing (..)
import Quickstart.Messages exposing (..)


-- VIEW


view : Model -> Html Msg
view model =
    article
        [ class "quickstart" ]
        [ introduction
        , contextCreation
        , insertingDocuments
        , executingQueries
        , conclusion
        ]



-- 1. INTRODUCTION


introduction : Html msg
introduction =
    Markdown.toHtml [] <| """
# Quickstart Guide

The following short guide will provide a quick overview of
[Hunt](https://github.com/hunt-framework/hunt) and some sample data,
to play around with.


## Contexts

A *context* is used to classify and distinguish indexed information.



### Schema

Each context is created with an individual schema.

In an analogy to databases, a context could be seen as a table.
It has a unique name and a configured behaviour. The configuration
is made with the help of the context schema.

Documents are always indexed with respect to at least one context.
It is also possible to create and remove contexts during runtime.

Each context has a corresponding context schema. The schema defines
the contexts behaviour. That includes:

- Definition of how the input should be split up into words.
- Definition of how these words should be preprocessed and normalized.
- Definition of the index type to use (text, date, geolocation, ...).


### Types

Each index implementation is integrated into the search engine as a
context type.

The search engine is started with a list of valid types. Only a context
schema created with one of those valid types is a valid schema.

The context type contains information how the index implementation should
be used.

This includes:

- **Validation:** Each implementation of an index-structure handles data
  differently. A basic validation of the index key needs to be performed
  before insertion. For example, a numeric context type needs to make sure
  each key is a valid number.
- **Splitting Words:** When inserting a big text into a context, some meta
  information is required, how this large text should be split up into words.
  The context type gives a default configuration how this task should be handled.
  The default should fit in most cases, else it could be overwritten by the context schema.


## Example

Consider the maintaining of a blog-like news website. Each blog entry contains
a subject, the date of publication, the content and the location, where
the news happened.

The page has become rather large and it is necessary to provide a way to
search and filter the articles.
"""



-- 2. CONTEXT CREATION


contextCreation : Html Msg
contextCreation =
    section
        [ class "context-creation"
        ]
        [ contextCreationPreface
        , runnableExample CreateContexts createContextCmds
        ]


contextCreationPreface : Html msg
contextCreationPreface =
    Markdown.toHtml [] """
### Context creation

The first step, when building a Hunt search engine application, is to think
about the necessary contexts. There are four fields of information stored for
an article. The publishing date should be a context by itself and is a perfect
match for the date context type. The location could be stored in location
contexts. The obvious choice for the articles’ content is the text type. One
could use a single context for the subject and the content.

In this case it is a more flexible approach, to keep the subject and content
separate. By doing so, it is possible to assign a higher weight to the subject.
This means, that the document in the result set gets a ranking boost, if a search
term occurs in the subject line. A blog entry “Introduction to Functors” would
rank higher in a search for “functor” than another blog entry, where the word
occurs just in the content itself.

First, let us create the four contexts:
"""


createContextCmds : Result String Value
createContextCmds =
    Json.decodeString Json.value """
[
  {
    "cmd": "insert-context",
    "context": "content",
    "schema": {
      "type": "text"
    }
  },
  {
    "cmd": "insert-context",
    "context": "subject",
    "schema": {
      "type": "text",
      "weight": 2
    }
  },
  {
    "cmd": "insert-context",
    "context": "publish_date",
    "schema": {
      "type": "date",
      "default": false
    }
  },
  {
    "cmd": "insert-context",
    "context": "location",
    "schema": {
      "type": "position",
      "default": false
    }
  }
]
"""



-- 3. INSERTING DOCS


insertingDocuments : Html Msg
insertingDocuments =
    section
        [ class "inserting-documents" ]
        [ insertingDocumentsPreface
        , runnableExample InsertDocs insertDocsCmds
        ]


insertingDocumentsPreface : Html msg
insertingDocumentsPreface =
    Markdown.toHtml [] """
### Inserting Documents

Now that all required contexts are created, documents can be inserted.
Each document needs to be inserted with an unique identifier. In this
example, it makes sense to use the URL of an article to identify it, a
database ID might be reasonable as well. While the framework helps with
basic word splitting and normalization, there is still some work to do.

The actual article needs to be transformed into the ApiDocument data
structure, before it can be inserted.

The index object defines the input for each context. Note that index and
document description names do not have to match and not every document has
to insert values to every context.

The description defines, which values should be stored alongside the
document ID. For applications storing the document data separately, for
example in a database, it is a good practice to leave the document
description empty. This avoids redundancy and possible inconsistent data sets.
"""


insertDocsCmds : Result String Value
insertDocsCmds =
    Json.decodeString Json.value """
[
  {
    "cmd": "insert",
    "document": {
      "uri": "Entry1",
      "index": {
        "subject": "Something incredible happened in Paris",
        "publish_date": "2013-07-17",
        "content": "Ad eos audire dissentias. Rebum iusto offendit sit te, sonet accusam ei sed. Ceteros periculis imperdiet vim ex, te illum soluta pro. Vis alterum saperet feugait no, tamquam epicuri vituperatoribus nec ea, ex vel doctus democritum.",
        "location": "48.858503 -2.294809"
      },
      "description": {
        "subject": "Something incredible happened in Paris",
        "content": "Ad eos audire dissentias. Rebum iusto offendit sit te, sonet accusam ei sed. Ceteros periculis imperdiet vim ex, te illum soluta pro. Vis alterum saperet feugait no, tamquam epicuri vituperatoribus nec ea, ex vel doctus democritum.",
        "date": "2013-07-17",
        "location": "48.858503 2.294809"
      }
    }
  },
  {
    "cmd": "insert",
    "document": {
      "uri": "Entry2",
      "index": {
        "subject": "Something incredible happened in New York",
        "publish_date": "2012-07-17",
        "content": "Ridens essent ex eam. No viris facete admodum mei, ut lorem iuvaret duo. Augue appellantur dissentiunt cum ut, te vel cibo semper, quod impedit eam in. Id duo sanctus facilisis, an postea delicatissimi eam, fuisset forensibus pro ne. Id omnis dicam intellegebat quo, ne admodum consulatu aliquando has, impedit comprehensam sit te.",
        "location": "40.722673 -73.995008"
      },
      "description": {
        "subject": "Something incredible happened in New York",
        "content": "Lorem ipsum dolor sit amet, apeirian consequat in pri, an enim falli fabulas usu. Ei mei nonumy eligendi qualisque, feugiat intellegat duo ut. Cu per brute nostro. Odio dicam nec at, everti interpretaris ea duo. Ne cum delicata assueverit. Eu vix malis voluptatibus, no debet numquam his, inimicus adversarium ne vix. Aliquando deterruisset mei cu.",
        "date": "2012-07-17",
        "location": "40.722673 -73.995008"
      }
    }
  },
  {
    "cmd": "insert",
    "document": {
      "uri": "Entry3",
      "index": {
        "subject": "Something incredible happened in Australia",
        "publish_date": "2011-07-17",
        "content": "Lorem ipsum dolor sit amet, apeirian consequat in pri, an enim falli fabulas usu. Ei mei nonumy eligendi qualisque, feugiat intellegat duo ut. Cu per brute nostro. Odio dicam nec at, everti interpretaris ea duo. Ne cum delicata assueverit. Eu vix malis voluptatibus, no debet numquam his, inimicus adversarium ne vix. Aliquando deterruisset mei cu.",
        "location": "-33.890937 -151.085129"
      },
      "description": {
        "subject": "Something incredible happened in Australia",
        "content": "Ridens essent ex eam. No viris facete admodum mei, ut lorem iuvaret duo. Augue appellantur dissentiunt cum ut, te vel cibo semper, quod impedit eam in. Id duo sanctus facilisis, an postea delicatissimi eam, fuisset forensibus pro ne. Id omnis dicam intellegebat quo, ne admodum consulatu aliquando has, impedit comprehensam sit te.",
        "date": "2011-07-17",
        "location": "-33.890937 151.085129"
      }
    }
  }
]
"""



-- 4. EXECUTING QUERIES


executingQueries : Html msg
executingQueries =
    Markdown.toHtml [] <| """
### Executing Queries

After indexing the blog articles, the index is ready for queries. Queries
can be expressed as a Haskell data type, a JSON structure or as plain text.
This section introduces each way with the help of a common query.

The blog search engine stores the publishing date of the articles in a
separate context. A common use of this is to search for articles within a
time frame. This can be done with a range query. It can be written as a plain
text query in the following way:

```
publish_date:[2014-01-10 TO 2014-01-23]
```

Try it out in the search tab!

Range queries generally yield the best results (and impose less performance
overhead), when applied to a specific context. This example query searches for
documents published between 10th and 23rd January 2014.

The same query would look like this in JSON:

```json
[
  {
    "cmd": "search",
    "max": 10,
    "offset": 0,
    "query": {
      "type": "context",
      "contexts": [
        "publish_date"
      ],
      "query": {
        "type": "range",
        "upper": "2014-01-23",
        "lower": "2014-01-10"
      }
    }
  }
]
```
"""



-- 5. CONCLUSION


conclusion : Html msg
conclusion =
    Markdown.toHtml [] """
## Conclusion


"""



-- HELPERS


runnableExample : (Value -> msg) -> Result String Value -> Html msg
runnableExample toMsg value =
    let
        clickHandler =
            case value of
                Err _ ->
                    []

                Ok jsonValue ->
                    [ onClick (toMsg jsonValue) ]
    in
        div
            [ class "executable-json" ]
            [ pre
                [ class "language-json" ]
                [ code
                    []
                    [ text (encode value)
                    ]
                ]
            , button clickHandler
                [ text "Run"
                ]
            ]


encode : Result String Value -> String
encode result =
    case result of
        Err err ->
            "{}"

        Ok value ->
            Json.encode 2 value
