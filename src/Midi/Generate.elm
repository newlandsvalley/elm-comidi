module Midi.Generate exposing (event, recording)

{-| Module for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Bitwise exposing (shiftRightBy)
import Char
import Midi.Types exposing (..)


{-| Generate a MIDI event
-}
event : MidiEvent -> List Byte
event event =
    case event of
        SysEx F0 bytes ->
            0xF0 :: (bytes ++ [ eox ])

        SysEx F7 bytes ->
            Debug.crash "WebMIDI SysEx events should all be of the 0xF0 flavor."

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
            Debug.crash "Unknown MIDI event."


{-| Generate a MIDI recording
-}
recording : MidiRecording -> List Byte
recording midi =
    case midi of
        SingleTrack ticksPerBeat t ->
            (header 0 1 ticksPerBeat) ++ track t

        MultipleTracks tracksType ticksPerBeat ts ->
            let
                format =
                    case tracksType of
                        Simultaneous ->
                            1

                        Independent ->
                            2
            in
                (header format (List.length ts) ticksPerBeat) ++ (List.concatMap track ts)



-- Lower level generators


header : Int -> Int -> Int -> List Byte
header format numTracks ticksPerBeat =
    List.concat
        [ strToBytes "MThd"
        , uint32 6
        , uint16 format
        , uint16 numTracks
        , uint16 ticksPerBeat
        ]


track : Track -> List Byte
track t =
    let
        endOfTrack =
            [ 0x00, 0xFF, 0x2F, 0x00 ]

        encodedMsgs =
            (List.concatMap midiMessage t) ++ endOfTrack

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


uint16 : Int -> List Byte
uint16 x =
    let
        b1 =
            Bitwise.and 255 (shiftRightBy 8 x)

        b2 =
            Bitwise.and 255 x
    in
        [ b1, b2 ]


uint32 : Int -> List Byte
uint32 x =
    let
        b1 =
            Bitwise.and 255 (shiftRightBy 24 x)

        b2 =
            Bitwise.and 255 (shiftRightBy 16 x)

        b3 =
            Bitwise.and 255 (shiftRightBy 8 x)

        b4 =
            Bitwise.and 255 x
    in
        [ b1, b2, b3, b4 ]


varInt : Int -> List Byte
varInt x =
    let
        varIntHelper x bytes =
            if x < 128 then
                (x + 128) :: bytes
            else
                varIntHelper
                    (shiftRightBy 7 x)
                    ((128 + (Bitwise.and 127 x)) :: bytes)
    in
        if x < 128 then
            [ x ]
        else
            varIntHelper (shiftRightBy 7 x) [ Bitwise.and 127 x ]
