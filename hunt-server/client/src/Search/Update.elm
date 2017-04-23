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
                    Debounce.push debounceConfig query model.debounce
            in
                ( { model | query = query, debounce = newDebounce }
                , cmd
                )

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
            ( { model | queryError = Just err }
            , Cmd.none
            )

        SetCompletions (Ok completions) ->
            ( { model | completions = completions, queryError = Nothing }
            , Cmd.none
            )

        SetCompletions (Err err) ->
            ( { model | queryError = Just err }
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



-- DEBOUNCE


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (400 * Time.millisecond)
    , transform = SetCompletionDebounce
    }



-- AUTOCOMPLETE


autocompleteConfig : Autocomplete.UpdateConfig Msg ( String, Float )
autocompleteConfig =
    Autocomplete.updateConfig
        { toId = Tuple.first
        , onKeyDown =
            \code maybeId ->
                Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \_ -> Nothing
        , separateSelections = False
        }
