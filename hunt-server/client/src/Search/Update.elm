module Search.Update
    exposing
        ( UpdateConfig
        , updateConfig
        , update
        )

import Time
import Http
import Hunt.Http exposing (BaseUrl)
import Search.Model exposing (..)
import Search.Messages exposing (..)
import Search.Requests as Req
import Autocomplete
import Debounce


-- UPDATE


type UpdateConfig
    = UpdateConfig
        { baseUrl : BaseUrl
        }


updateConfig :
    { baseUrl : BaseUrl
    }
    -> UpdateConfig
updateConfig =
    UpdateConfig


update : UpdateConfig -> Msg -> Model -> ( Model, Cmd Msg )
update (UpdateConfig config) msg model =
    case msg of
        SetQuery query ->
            let
                ( newDebounce, cmd ) =
                    if String.isEmpty query then
                        ( model.debounce, Cmd.none )
                    else
                        Debounce.push debounceConfig query model.debounce
            in
                ( { model
                    | query = query
                    , debounce = newDebounce
                    , menuVisible = not (String.isEmpty query)
                  }
                , cmd
                )

        SetQueryAndClose query ->
            update (UpdateConfig config)
                Search
                { model
                    | query = query
                    , menuVisible = False
                }

        SetAutocompleteState autoMsg ->
            let
                ( newAutoState, maybeMsg ) =
                    Autocomplete.update
                        autocompleteConfig
                        autoMsg
                        model.howManyCompletionsToShow
                        model.autoState
                        model.completions

                newModel =
                    { model | autoState = newAutoState }
            in
                case maybeMsg of
                    Nothing ->
                        ( newModel
                        , Cmd.none
                        )

                    Just otherMsg ->
                        update (UpdateConfig config) otherMsg newModel

        SetRankedDocs (Ok result) ->
            ( { model | rankedDocs = result, queryError = Nothing }
            , Cmd.none
            )

        SetRankedDocs (Err err) ->
            ( { model
                | queryError = Just err
                , menuVisible = False
              }
            , Cmd.none
            )

        SetCompletions (Ok completions) ->
            let
                newAutoState =
                    Autocomplete.resetToFirstItem
                        autocompleteConfig
                        completions
                        model.howManyCompletionsToShow
                        model.autoState
            in
                ( { model
                    | completions = completions
                    , queryError = Nothing
                    , menuVisible = not (List.isEmpty completions)
                    , autoState = newAutoState
                  }
                , Cmd.none
                )

        SetCompletions (Err err) ->
            ( { model | queryError = Just err, menuVisible = False }
            , Cmd.none
            )

        SetCompletionDebounce debounceMsg ->
            let
                complete =
                    Http.send SetCompletions << Req.complete config.baseUrl

                ( newDebounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast complete)
                        debounceMsg
                        model.debounce
            in
                ( { model | debounce = newDebounce }
                , cmd
                )

        CloseAutocomplete ->
            ( { model | menuVisible = False }
            , Cmd.none
            )

        Search ->
            ( { model | menuVisible = False }
            , if String.isEmpty model.query then
                Cmd.none
              else
                Req.search config.baseUrl model.query
                    |> Http.send SetRankedDocs
            )

        NoOp ->
            ( model, Cmd.none )



-- DEBOUNCE


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (400 * Time.millisecond)
    , transform = SetCompletionDebounce
    }



-- AUTOCOMPLETE


escCode : Int
escCode =
    27


enterCode : Int
enterCode =
    13


autocompleteConfig : Autocomplete.UpdateConfig Msg ( String, Float )
autocompleteConfig =
    Autocomplete.updateConfig
        { toId = Tuple.first
        , onKeyDown =
            \code maybeId ->
                if code == enterCode then
                    Maybe.map SetQueryAndClose maybeId
                else if code == escCode then
                    Just CloseAutocomplete
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \value -> Just (SetQueryAndClose value)
        , separateSelections = False
        }
