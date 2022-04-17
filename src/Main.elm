module Main exposing (main)

import Browser
import Html exposing (div, h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (Regex)



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { pattern : String
    , regex : Regex
    , status : Status
    , corpus : List String
    , matches : List String
    }


type Status
    = Ok
    | RegexError



-- INIT


corpus =
    String.split " " "apple banana cherry durian eggplant fig green-bean"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pattern = "", regex = Regex.never, status = Ok, corpus = corpus, matches = [] }, Cmd.none )



-- UPDATE


type Msg
    = UpdatePattern String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePattern input ->
            case Regex.fromString input of
                Just regex ->
                    let
                        new_matches =
                            List.filter (Regex.contains regex) corpus
                    in
                    ( { model | pattern = input, regex = regex, status = Ok, matches = new_matches }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | pattern = input, status = RegexError }, Cmd.none )



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
        , div []
            (case model.status of
                Ok ->
                    [ Html.textarea [] [ text (String.join "\n" model.matches) ] ]

                RegexError ->
                    [ text "Invalid regex" ]
            )
        ]
    }
