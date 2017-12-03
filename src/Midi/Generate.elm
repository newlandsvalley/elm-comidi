module Midi.Generate exposing (event, events, recording)

{-| Library for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Bitwise
import Char
import Midi.Types exposing (..)


event : MidiEvent -> List Byte
event event =
    case event of
        SysEx bytes ->
            0xF0 :: (bytes ++ [ 0xF7 ])

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



-- TODO(rofer): events should later automatically use Running Status where
-- possible


events : List MidiEvent -> List Byte
events events =
    List.concatMap event events


recording : MidiRecording -> List Byte
recording ( header, tracks ) =
    Debug.crash "TODO" ()
