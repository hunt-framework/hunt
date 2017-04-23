module SearchField
    exposing
        ( State
        , Msg
        , subscriptions
        , empty
        , update
        , view
        )

import Html exposing (Html, div, input)
import Html.Attributes exposing (class)
import Autocomplete


-- STATE


type State
    = State
        { query : String
        , autoState : Autocomplete.State
        }


empty : State
empty =
    State "" (Autocomplete.empty)



-- UPDATE


type Msg
    = SetQuery String


update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    case msg of
        SetQuery query ->
            ( State { state | query = query }
            , Cmd.none
            )



-- VIEW


view : State -> Html Msg
view (State state) =
    div
        []
        [ input
            [ type' "text"
            , value state.query
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions (State state) =
    Autocomplete.subscriptions state.autoState
