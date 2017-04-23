module Quickstart.Update
    exposing
        ( UpdateConfig
        , updateConfig
        , update
        )

import Hunt.Http exposing (BaseUrl)
import Quickstart.Model exposing (..)
import Quickstart.Messages exposing (..)


-- UPDATE


type UpdateConfig
    = UpdateConfig
        { baseUrl : BaseUrl
        }


updateConfig : { baseUrl : BaseUrl } -> UpdateConfig
updateConfig =
    UpdateConfig


update : UpdateConfig -> Msg -> Model -> ( Model, Cmd Msg )
update (UpdateConfig config) msg model =
    case msg of
        CreateContexts contexts ->
            ( model
            , Cmd.none
            )

        CreateContextsResult (Ok _) ->
            ( { model | contextError = Nothing }
            , Cmd.none
            )

        CreateContextsResult (Err err) ->
            ( { model | contextError = Just err }
            , Cmd.none
            )

        InsertDocs docs ->
            ( model
            , Cmd.none
            )

        InsertDocsResult (Ok _) ->
            ( { model | insertError = Nothing }
            , Cmd.none
            )

        InsertDocsResult (Err err) ->
            ( { model | insertError = Just err }
            , Cmd.none
            )
