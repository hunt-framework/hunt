module Quickstart.Update
    exposing
        ( UpdateConfig
        , updateConfig
        , update
        )

import Http
import Hunt.Http exposing (BaseUrl)
import Quickstart.Model exposing (..)
import Quickstart.Messages exposing (..)
import Quickstart.Requests as Req


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
            , Req.createContexts config.baseUrl contexts
                |> Http.send CreateContextsResult
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
            , Req.insertDocs config.baseUrl docs
                |> Http.send InsertDocsResult
            )

        InsertDocsResult (Ok _) ->
            ( { model | insertError = Nothing }
            , Cmd.none
            )

        InsertDocsResult (Err err) ->
            ( { model | insertError = Just err }
            , Cmd.none
            )
