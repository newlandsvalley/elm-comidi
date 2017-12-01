module Midi.Generate exposing (event, recording)

{-| Library for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Midi.Types exposing (..)


type alias Byte =
    Int


event : MidiEvent -> List Byte
event event =
    case event of
        NoteOn channel note velocity ->
            [ 128 + 16 + channel, note, velocity ]

        NoteOff channel note velocity ->
            [ 128 + channel, note, velocity ]

        _ ->
            Debug.crash "TODO"


recording : MidiRecording -> List Byte
recording ( header, tracks ) =
    Debug.crash "TODO" ()
