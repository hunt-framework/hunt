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
import Quickstart


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
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    update (UrlChange location) (Model Index)



-- UPDATE


type Msg
    = UrlChange Navigation.Location
    | CreateContexts Json.Value
    | InsertDocs Json.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        CreateContexts contexts ->
            ( model
            , Cmd.none
            )

        InsertDocs docs ->
            ( model
            , Cmd.none
            )



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
            div
                []
                [ text "Hello Hunt!"
                , a [ href "#/examples" ] [ text "Examples" ]
                ]

        Examples ->
            Quickstart.view quickstartViewConfig


quickstartViewConfig : Quickstart.ViewConfig Msg
quickstartViewConfig =
    Quickstart.viewConfig
        { createContexts = CreateContexts
        , insertDocs = InsertDocs
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
