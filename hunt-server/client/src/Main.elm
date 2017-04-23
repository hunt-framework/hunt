module Main
    exposing
        ( main
        )

import Html exposing (Html, div, text, a)
import Html.Attributes exposing (href)
import Json.Decode as Json
import Navigation
import UrlParser as Url
import Page exposing (Page(..), urlParser, toPath)
import Hunt.Http as Http exposing (BaseUrl)
import Quickstart.Model as Quickstart
import Quickstart.Messages as Quickstart
import Quickstart.Update as Quickstart
import Quickstart.View as Quickstart
import Search.Model as Search
import Search.Update as Search
import Search.Messages as Search
import Search.View as Search
import Search.Subscriptions as Search


-- MAIN


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { page : Page
    , baseUrl : Http.BaseUrl
    , search : Search.Model
    , quickstart : Quickstart.Model
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    update (UrlChange location)
        { page = Index
        , baseUrl = Http.parseBaseUrl "api"
        , search = Search.init
        , quickstart = Quickstart.init
        }



-- UPDATE


type Msg
    = UrlChange Navigation.Location
    | UpdateSearch Search.Msg
    | UpdateQuickstart Quickstart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        UpdateSearch searchMsg ->
            let
                updateConfig =
                    searchUpdateConfig model.baseUrl

                ( newSearch, searchCmd ) =
                    Search.update updateConfig searchMsg model.search
            in
                ( { model | search = newSearch }
                , Cmd.map UpdateSearch searchCmd
                )

        UpdateQuickstart quickstartMsg ->
            let
                updateConfig =
                    quickstartUpdateConfig model.baseUrl

                ( newQuickstart, quickstartCmd ) =
                    Quickstart.update updateConfig quickstartMsg model.quickstart
            in
                ( { model | quickstart = newQuickstart }
                , Cmd.map UpdateQuickstart quickstartCmd
                )


searchUpdateConfig : BaseUrl -> Search.UpdateConfig
searchUpdateConfig baseUrl =
    Search.updateConfig
        { baseUrl = baseUrl
        }


quickstartUpdateConfig : BaseUrl -> Quickstart.UpdateConfig
quickstartUpdateConfig baseUrl =
    Quickstart.updateConfig
        { baseUrl = baseUrl
        }



-- URL UPDATE


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case Url.parseHash urlParser location of
        Just page ->
            ( { model | page = page }
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Index ->
            Html.map UpdateSearch (Search.view model.search)

        Quickstart ->
            Html.map UpdateQuickstart (Quickstart.view model.quickstart)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateSearch (Search.subscriptions model.search)
