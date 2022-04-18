module Main exposing (main)

import Browser
import Html exposing (div, h1, input, option, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Http
import Process
import Regex exposing (Regex)
import Task



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
    , caseInsensitive : Bool
    , regex : Maybe Regex
    , corpus : Corpus
    , matches : List String
    , count : Int
    }


type Corpus
    = Loaded (List String)
    | Awaiting
    | Failed



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pattern = ""
      , caseInsensitive = False
      , regex = Just Regex.never
      , corpus = Awaiting
      , matches = []
      , count = 0
      }
    , Http.get
        { url = "https://raw.githubusercontent.com/wsowens/regex-dict/master/words.txt"
        , expect = Http.expectString CorpusResponse
        }
    )



-- Before checking for regex matches, wait this many ms


delay_check_matches : Float
delay_check_matches =
    200.0



-- UPDATE


type Msg
    = UpdatePattern String
    | ChangeCase Bool
    | CorpusResponse (Result Http.Error String)
      -- include the caseInsensitive and pattern field in the Msg so we can check if it's old
    | GetMatches Bool String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePattern input ->
            ( updateModelRegex model model.caseInsensitive input
            , Task.perform (\_ -> GetMatches model.caseInsensitive input) (Process.sleep delay_check_matches)
            )

        ChangeCase input ->
            ( updateModelRegex model input model.pattern
              -- no delay necessary here, usually if someone hits a checkbox they mean it
            , Task.perform (\_ -> GetMatches input model.pattern) (Task.succeed Nothing)
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

        GetMatches withCase withPattern ->
            let
                new_matches =
                    case model.corpus of
                        Awaiting ->
                            model.matches

                        Failed ->
                            model.matches

                        Loaded corpus ->
                            case model.regex of
                                Nothing ->
                                    model.matches

                                Just regex ->
                                    if withPattern /= model.pattern || withCase /= model.caseInsensitive || withPattern == "" then
                                        model.matches

                                    else
                                        List.filter (Regex.contains regex) corpus
            in
            ( { model | matches = new_matches }
            , Cmd.none
            )


updateModelRegex : Model -> Bool -> String -> Model
updateModelRegex model caseInsensitive pattern =
    { model
        | pattern = pattern
        , caseInsensitive = caseInsensitive
        , regex =
            if pattern == "" then
                Nothing

            else
                Regex.fromStringWith { caseInsensitive = caseInsensitive, multiline = False } pattern
    }



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
        , input [ placeholder "enter a JavaScript regular expression here", value model.pattern, onInput UpdatePattern ] []
        , Html.span []
            [ text
                (if model.regex == Nothing && model.pattern /= "" then
                    "Error: invalid regex"

                 else
                    ""
                )
            ]
        , div [] []
        , input [ type_ "checkbox", checked model.caseInsensitive, onCheck ChangeCase ] []
        , Html.span [] [ text "Case insensitive?" ]
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
