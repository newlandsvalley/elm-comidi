module Midi.Types
    exposing
        ( Track
        , MidiEvent(..)
        , MidiMessage
        , TracksType(..)
        , MidiRecording(..)
        , Byte
        , Channel
        , Note
        , Velocity
        , SysExFlavour(..)
        , Ticks
        , eox
        , validRecording
        )

{-| Type Definition of a MIDI recording


# Data Types

@docs Track, MidiEvent, MidiMessage, TracksType, MidiRecording, Byte, Channel

@docs Note, Velocity, SysExFlavour, Ticks


# Functions

@docs eox, validRecording

-}

import List exposing (..)


{-| Midi Tick

elapsed time

-}
type alias Ticks =
    Int


{-| just a hint that we're really interested in the bytes
in some MidiEvent constructors that return Lists
-}
type alias Byte =
    Int


{-| Midi Channel
-}
type alias Channel =
    Int


{-| Midi Note
-}
type alias Note =
    Int


{-| Midi Velocity (
volume
-}
type alias Velocity =
    Int


{-| SysExFlavour

Discriminate between two different forms of SysExEvrnt as characterised by the
lead-in byte, See <https://www.csie.ntu.edu.tw/~r92092/ref/midi/#sysex_event>.

-}
type SysExFlavour
    = F0 -- normal
    | F7 -- escaped


{-| Midi Event

Note that RunningStatus messages are not included within MidiEvent
because the parser translates them to the underlying channel messages

-}
type MidiEvent
    = -- meta messages
      SequenceNumber Int
    | Text String
    | Copyright String
    | TrackName String
    | InstrumentName String
    | Lyrics String
    | Marker String
    | CuePoint String
    | ChannelPrefix Int
    | Tempo Int
    | SMPTEOffset Int Int Int Int Int
    | TimeSignature Int Int Int Int
    | KeySignature Int Int
    | SequencerSpecific (List Byte)
    | SysEx SysExFlavour (List Byte)
    | Unspecified Int (List Byte)
      -- channel messages
    | NoteOn Channel Note Velocity
    | NoteOff Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControlChange Channel Int Int
    | ProgramChange Channel Int
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Int


{-| Midi Message
-}
type alias MidiMessage =
    ( Ticks, MidiEvent )


{-| Midi Track
-}
type alias Track =
    List MidiMessage


{-| Discriminate between the types of tracks in a recording.
Are they played simultaneously or independently.
-}
type TracksType
    = Simultaneous
    | Independent


{-| Midi Recording
-}
type MidiRecording
    = SingleTrack Int Track
    | MultipleTracks TracksType Int (List Track)


{-| Constants
-}
eox : Int
eox =
    0xF7


{-| Helpers
-}



{- Returns true if a MidiRecording is valid.
   Currently this involves verifying multipart sysex messages are broken up correctly.
-}


validRecording : MidiRecording -> Bool
validRecording r =
    let
        validTrack multipart track =
            case track of
                -- All multipart messages must be finished.
                [] ->
                    not multipart

                t :: ts ->
                    case ( t, multipart ) of
                        ( ( _, SysEx F0 data ), False ) ->
                            case head (reverse data) of
                                Just eox ->
                                    validTrack False ts

                                _ ->
                                    validTrack True ts

                        -- After the first packet all parts of a multipart packet
                        -- start with F7.
                        ( ( _, SysEx F0 _ ), True ) ->
                            False

                        ( ( _, SysEx F7 data ), True ) ->
                            case head (reverse data) of
                                Just eox ->
                                    validTrack False ts

                                _ ->
                                    validTrack True ts

                        ( ( _, SysEx F7 _ ), False ) ->
                            validTrack multipart ts

                        -- There must be no MIDI events in between the packets of a
                        -- multipart sysex message.
                        ( _, True ) ->
                            False

                        _ ->
                            validTrack multipart ts
    in
        case r of
            SingleTrack _ t ->
                validTrack False t

            MultipleTracks _ _ ts ->
                all (validTrack False) ts
