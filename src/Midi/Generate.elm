module Midi.Generate exposing (event, recording)

{-| Library for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Bitwise exposing (shiftRightBy)
import Char
import Midi.Types exposing (..)


event : MidiEvent -> List Byte
event event =
    case event of
        SysEx F0 bytes ->
            0xF0 :: bytes

        SysEx F7 bytes ->
            bytes

        NoteOn channel note velocity ->
            [ 0x90 + channel, note, velocity ]

        NoteOff channel note velocity ->
            [ 0x80 + channel, note, velocity ]

        NoteAfterTouch channel note velocity ->
            [ 0xA0 + channel, note, velocity ]

        ControlChange channel controllerNumber value ->
            [ 0xB0 + channel, controllerNumber, value ]

        ProgramChange channel value ->
            [ 0xC0 + channel, value ]

        ChannelAfterTouch channel velocity ->
            [ 0xD0 + channel, velocity ]

        PitchBend channel bend ->
            let
                lower =
                    Bitwise.and bend 127

                upper =
                    shiftRightBy 7 bend
            in
                [ 0xE0 + channel, lower, upper ]

        _ ->
            Debug.crash "TODO"


recording : MidiRecording -> List Byte
recording ( h, tracks ) =
    header h ++ (List.concatMap track tracks)



-- Lower level generators


header : Header -> List Byte
header h =
    let
        format =
            h.formatType

        numTracks =
            h.trackCount

        division =
            h.ticksPerBeat
    in
        (strToBytes "MThd") ++ [ 0, 0, 0x06, format, numTracks, division ]


track : Track -> List Byte
track t =
    let
        encodedMsgs =
            List.concatMap midiMessage t

        len =
            List.length encodedMsgs
    in
        (strToBytes "MTrk") ++ uint32 len ++ encodedMsgs


midiMessage : MidiMessage -> List Byte
midiMessage ( ticks, e ) =
    (varInt ticks) ++ fileEvent e


fileEvent : MidiEvent -> List Byte
fileEvent e =
    case e of
        SysEx F0 bytes ->
            0xF0 :: ((varInt (List.length bytes)) ++ bytes)

        SysEx F7 bytes ->
            0xF7 :: ((varInt (List.length bytes)) ++ bytes)

        _ ->
            -- Use the regular event generator for everything else
            event e



-- Helper functions


strToBytes : String -> List Byte
strToBytes =
    (List.map Char.toCode) << String.toList


uint32 : Int -> List Byte
uint32 x =
    let
        b1 =
            Bitwise.and 255 x

        b2 =
            Bitwise.and 255 (shiftRightBy 8 x)

        b3 =
            Bitwise.and 255 (shiftRightBy 16 x)

        b4 =
            Bitwise.and 255 (shiftRightBy 24 x)
    in
        [ b1, b2, b3, b4 ]


varInt : Int -> List Byte
varInt x =
    let
        varIntHelper x bytes =
            varIntHelper
                (shiftRightBy 7 x)
                ((Bitwise.and 255 x) :: bytes)
    in
        if x < 128 then
            [ x ]
        else
            varIntHelper x []
