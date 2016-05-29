module Parser exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result)
import CoMidi exposing (normalise, parse)
import MidiTypes exposing (MidiRecording)


-- MODEL

type alias Model =
    { recording : Result String MidiRecording
    }

init : String -> (Model, Cmd Msg)
init topic =
  ( { recording = Err "not started"  }
  , Cmd.none
  )

-- UPDATE

type Msg
    = NoOp
    | Load String
    | Midi (Result String MidiRecording )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    Midi result ->  ( { recording = result }, Cmd.none ) 

    Load url -> (model, loadMidi url) 
   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x

{- load a MIDI file -}
loadMidi : String -> Cmd Msg
loadMidi url = 
      let settings =  { defaultSettings | desiredResponseType  = Just "text/plain; charset=x-user-defined" }   
        in
          Http.send settings
                          { verb = "GET"
                          , headers = []
                          , url = url
                          , body = empty
                          } 
          |> Task.toResult
          |> Task.map extractResponse
          |> Task.map parseLoadedFile
          |> Task.perform (\_ -> NoOp) Midi 

{- extract the true response, concentrating on 200 statuses - assume other statuses are in error
   (usually 404 not found)
-}
extractResponse : Result RawError Response -> Result String Value
extractResponse result =
  case result of
    Ok response -> 
      case response.status of
        200 -> 
          Ok response.value
        _ -> 
          Err (toString (response.status) ++ ": " ++ response.statusText)
    Err e -> Err "unexpected http error"

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0


parseLoadedFile : Result String Value -> Result String MidiRecording
parseLoadedFile r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s |> normalise |> parse
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

viewParseResult : Result String MidiRecording -> String
viewParseResult mr = 
   case mr of
      Ok res -> 
         "OK: " ++ (toString res)
      Err errs -> 
         "Fail: " ++ (toString errs)


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (Load "midi/lillasystern.midi") ] [ text "lillasystern (type-0)" ]
    , button [ onClick (Load "midi/carolansreceipt.midi") ] [ text "Carolan's Receipt (type-1)" ]
    , button [ onClick (Load "midi/Galway-Hornpipe.midi") ] [ text "Galway Hornpipe (unspecified meta)" ]
    , button [ onClick (Load "midi/frost.midi") ] [ text "Frost is all over (running status)" ]
    , div [  ] [ text ("parse result: " ++ (viewParseResult model.recording)) ]
    ]




