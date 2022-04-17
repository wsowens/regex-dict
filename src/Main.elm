module Main exposing (main)

import Browser
import Html exposing (h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS
-- MODEL


type alias Model =
    { pattern : String
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pattern = "" }, Cmd.none )



-- UPDATE


type Msg
    = UpdatePattern String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePattern input ->
            ( { model | pattern = input }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "epic"
    , body =
        [ h1 [] [ text "regex-dict" ]
        , input [ placeholder "regular expression to search", value model.pattern, onInput UpdatePattern ] []
        , text model.pattern
        ]
    }
