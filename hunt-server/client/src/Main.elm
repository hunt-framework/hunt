module Main
    exposing
        ( main
        )

import Html exposing (Html, div, text)
import Navigation
import UrlParser as Url
import Page exposing (Page(..), urlParser, toPath)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model



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
            viewIndex

        Examples ->
            viewExamples


viewIndex : Html Msg
viewIndex =
    div
        []
        [ text "Hello Hunt!"
        ]


viewExamples : Html Msg
viewExamples =
    div
        []
        [ text "Hello Examples"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
