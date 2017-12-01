module Midi.Generate exposing (event, recording)

{-| Library for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Bitwise
import Midi.Types exposing (..)


type alias Byte =
    Int


event : MidiEvent -> List Byte
event event =
    case event of
        NoteOn channel note velocity ->
            [ 144 + channel, note, velocity ]

        NoteOff channel note velocity ->
            [ 128 + channel, note, velocity ]

        NoteAfterTouch channel note velocity ->
            [ 160 + channel, note, velocity ]

        ControlChange channel controllerNumber value ->
            [ 176 + channel, controllerNumber, value ]

        ProgramChange channel value ->
            [ 192 + channel, value ]

        ChannelAfterTouch channel velocity ->
            [ 208 + channel, velocity ]

        PitchBend channel bend ->
            let
                lower =
                    Bitwise.and bend 127

                upper =
                    Bitwise.shiftRightBy 7 bend
            in
                [ 224 + channel, lower, upper ]

        _ ->
            Debug.crash "TODO"


recording : MidiRecording -> List Byte
recording ( header, tracks ) =
    Debug.crash "TODO" ()
