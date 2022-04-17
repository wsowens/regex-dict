module Main exposing (main)

import Browser
import Html exposing (div, h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
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
    , regex : Maybe Regex
    , corpus : Corpus
    , matches : List String
    }


type Corpus
    = Loaded (List String)
    | Awaiting
    | Failed



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pattern = ""
      , regex = Just Regex.never
      , corpus = Awaiting
      , matches = []
      }
    , Http.get
        { url = "https://raw.githubusercontent.com/wsowens/regex-dict/master/words.txt"
        , expect = Http.expectString CorpusResponse
        }
    )



-- UPDATE


type Msg
    = UpdatePattern String
    | CorpusResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePattern input ->
            let
                new_model =
                    { model
                        | pattern = input
                        , regex =
                            if input == "" then
                                Nothing

                            else
                                Regex.fromString input
                    }

                regex =
                    Maybe.withDefault Regex.never new_model.regex
            in
            case model.corpus of
                Awaiting ->
                    ( new_model, Cmd.none )

                Failed ->
                    ( new_model, Cmd.none )

                Loaded corpus ->
                    ( { new_model | matches = List.filter (Regex.contains regex) corpus }
                    , Cmd.none
                    )

        CorpusResponse result ->
            case result of
                Ok rawText ->
                    ( { model | corpus = Loaded (String.split "\n" rawText) }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | corpus = Failed }
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
        , Html.span []
            [ text
                (if model.regex == Nothing then
                    "Error: invalid regex"

                 else
                    ""
                )
            ]
        , div []
            (case model.corpus of
                Loaded _ ->
                    [ Html.textarea [] [ text (String.join "\n" model.matches) ] ]

                Failed ->
                    [ text "Corpus failed to load." ]

                Awaiting ->
                    [ text "Loading corpus" ]
            )
        ]
    }
