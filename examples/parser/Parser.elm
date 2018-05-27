module Parser exposing (..)

{-
   Sample elm calling program
-}

import Html exposing (Html, div, button, input, text, textarea, h1)
import Html.Events exposing (onClick, on, onInput, targetValue)
import Html.Attributes exposing (..)
import Char exposing (fromCode, toCode)
import Bitwise exposing (and)
import String exposing (..)
import Result exposing (..)
import BinaryFileIO.Ports exposing (..)
import Json.Decode as Json exposing (..)
import Midi.Parse
import Midi.Types exposing (MidiRecording)
import Debug exposing (..)


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { recording : Result String MidiRecording
    , name : Maybe String
    }


{-| initialise the model
-}
init : ( Model, Cmd Msg )
init =
    { recording = Err "not yet loaded"
    , name = Nothing
    }
        ! [ Cmd.none ]



-- UPDATE


type Msg
    = NoOp
    | RequestFileUpload
    | FileLoaded (Maybe Filespec)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RequestFileUpload ->
            ( model, requestLoadFile () )

        FileLoaded maybef ->
            let
                _ =
                    log "elm filespec input" "some contents"
            in
                case maybef of
                    Just f ->
                        ( { model
                            | recording =
                                Midi.Parse.normalise f.contents
                                    |> Midi.Parse.file
                            , name = Just f.name
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


fileLoadedSub : Sub Msg
fileLoadedSub =
    fileLoaded FileLoaded


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileLoadedSub
        ]



-- EFFECTS
-- VIEW


viewLoadResult : Result String MidiRecording -> String
viewLoadResult mr =
    case mr of
        Ok res ->
            "OK: " ++ (toString res)

        Err errs ->
            "Fail: " ++ errs


viewFileName : Model -> String
viewFileName model =
    case model.name of
        Just n ->
            "file name: " ++ n

        Nothing ->
            ""


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "MIDI file parser" ]
        , input
            [ type_ "file"
            , id "fileinput"
            , accept ".midi"
            , on "change" (Json.succeed RequestFileUpload)
            ]
            []
        , div [] [ text (viewFileName model) ]
        , div [] [ text ("load result: " ++ (viewLoadResult model.recording)) ]
        ]
