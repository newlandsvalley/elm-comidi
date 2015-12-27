module Parser where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result)
import CoMidi exposing (MidiRecording, normalise, parse)

-- MODEL

type alias Model =
    { recording : Result String MidiRecording
    }

init : String -> (Model, Effects Action)
init topic =
  ( { recording = Err "not started"  }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp
    | Load String
    | Midi (Result String MidiRecording )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    Midi result ->  ( { recording = result }, Effects.none ) 

    Load url -> (model, loadMidi url) 
  


   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load a MIDI file -}
loadMidi : String -> Effects Action
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
          |> Task.map Midi
          |> Effects.task

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


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address (Load "midi/lillasystern.midi") ] [ text "lillasystern (type-0)" ]
    , button [ onClick address (Load "midi/carolansreceipt.midi") ] [ text "Carolan's Receipt (type-1)" ]
    , button [ onClick address (Load "midi/Galway-Hornpipe.midi") ] [ text "Galway Hornpipe (unspecified meta)" ]
    , button [ onClick address (Load "midi/frost.midi") ] [ text "Frost is all over (running status)" ]
    , div [  ] [ text ("parse result: " ++ (viewParseResult model.recording)) ]
    ]

-- INPUTS


signals : List (Signal Action)
signals = []


