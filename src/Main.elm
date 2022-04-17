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
    , regex : Regex
    , status : Status
    , corpus : Maybe (List String)
    , matches : List String
    }


type Status
    = AllClear
    | RegexError
    | CorpusLoadError



-- INIT


corpus =
    String.split " " "apple banana cherry durian eggplant fig green-bean"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pattern = ""
      , regex = Regex.never
      , status = AllClear
      , corpus = Nothing
      , matches = []
      }
    , Http.get
        { url = "https://raw.githubusercontent.com/dwyl/english-words/master/words.txt"
        , expect = Http.expectString maybeLoadCorpus
        }
    )



-- UPDATE


type Msg
    = UpdatePattern String
    | LoadCorpus String
    | LoadingError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePattern input ->
            case Regex.fromString input of
                Just regex ->
                    let
                        new_matches =
                            List.filter (Regex.contains regex) (Maybe.withDefault [] model.corpus)
                    in
                    ( { model | pattern = input, regex = regex, status = AllClear, matches = new_matches }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | pattern = input, status = RegexError }, Cmd.none )

        LoadCorpus rawText ->
            ( { model | corpus = Just (String.split "\n" rawText) }
            , Cmd.none
            )

        LoadingError ->
            ( { model | status = CorpusLoadError }
            , Cmd.none
            )


maybeLoadCorpus : Result Http.Error String -> Msg
maybeLoadCorpus result =
    case result of
        Ok string ->
            LoadCorpus string

        Err _ ->
            LoadingError



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
                AllClear ->
                    [ Html.textarea [] [ text (String.join "\n" model.matches) ] ]

                RegexError ->
                    [ text "Invalid regex" ]

                CorpusLoadError ->
                    [ text "can't load corpus" ]
            )
        ]
    }
