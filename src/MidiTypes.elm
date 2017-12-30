module MidiTypes
    exposing
        ( Track
        , Header
        , MidiEvent(..)
        , MidiMessage
        , MidiRecording
        , Byte
        )

{-| Type Definition of a MIDI recording


# Data Types

@docs Header, Track, MidiEvent, MidiMessage, MidiRecording

-}


type alias Ticks =
    Int


{-| just a hint that we're really interested in the bytes
in some MidiEvent constructores that return Lists
-}
type alias Byte =
    Int


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
    | SysEx (List Byte)
    | Unspecified Int (List Byte)
      -- channel messages
    | NoteOn Int Int Int
    | NoteOff Int Int Int
    | NoteAfterTouch Int Int Int
    | ControlChange Int Int Int
    | ProgramChange Int Int
    | ChannelAfterTouch Int Int
    | PitchBend Int Int



-- | RunningStatus Int Int


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
    , trackCount : Int
    , ticksPerBeat : Int
    }


{-| Midi Recording
-}
type alias MidiRecording =
    ( Header, List Track )
