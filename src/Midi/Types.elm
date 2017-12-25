module Midi.Types
    exposing
        ( Track
        , Header
        , MidiEvent(..)
        , MidiMessage
        , MidiRecording
        , Byte
        , Channel
        , Note
        , Velocity
        , SysExFlavour(..)
        , Ticks
        )

{-| Type Definition of a MIDI recording


# Data Types

@docs Header, Track, MidiEvent, MidiMessage, MidiRecording

-}


type alias Ticks =
    Int


{-| just a hint that we're really interested in the bytes
in some MidiEvent constructors that return Lists
-}
type alias Byte =
    Int


type alias Channel =
    Int


type alias Note =
    Int


type alias Velocity =
    Int


{-| Midi Event

Note that RunningStatus messages are not included within MidiEvent
because the parser translates them to the underlying channel messages

-}
type SysExFlavour
    = F0 -- normal
    | F7 -- escaped


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


{-| Midi Header
-}
type alias Header =
    { formatType : Int
    , ticksPerBeat : Int
    }


{-| Midi Recording
-}
type alias MidiRecording =
    ( Header, List Track )
