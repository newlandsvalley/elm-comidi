module MidiTest exposing (..)

import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (..)
import Midi.Types exposing (..)
import Midi.Parse exposing (..)
import Midi.Generate as Generate


fuzzChannel : Fuzzer Channel
fuzzChannel =
    intRange 0 15


fuzzNote : Fuzzer Note
fuzzNote =
    intRange 0 127


fuzzVelocity : Fuzzer Velocity
fuzzVelocity =
    intRange 0 127


fuzzPositiveVelocity : Fuzzer Velocity
fuzzPositiveVelocity =
    intRange 1 127


fuzzControllerNumber : Fuzzer Int
fuzzControllerNumber =
    intRange 0 119


fuzzNoteOn : Fuzzer MidiEvent
fuzzNoteOn =
    Fuzz.map3 NoteOn fuzzChannel fuzzNote fuzzPositiveVelocity


fuzzNoteOff : Fuzzer MidiEvent
fuzzNoteOff =
    Fuzz.map3 NoteOff fuzzChannel fuzzNote fuzzVelocity


fuzzNoteAfterTouch : Fuzzer MidiEvent
fuzzNoteAfterTouch =
    Fuzz.map3 NoteAfterTouch fuzzChannel fuzzNote fuzzVelocity


fuzzControlChange : Fuzzer MidiEvent
fuzzControlChange =
    Fuzz.map3 ControlChange fuzzChannel fuzzControllerNumber fuzzVelocity


fuzzProgramChange : Fuzzer MidiEvent
fuzzProgramChange =
    Fuzz.map2 ProgramChange fuzzChannel fuzzVelocity


fuzzChannelAfterTouch : Fuzzer MidiEvent
fuzzChannelAfterTouch =
    Fuzz.map2 ChannelAfterTouch fuzzChannel fuzzVelocity


fuzzPitchBend : Fuzzer MidiEvent
fuzzPitchBend =
    Fuzz.map2 PitchBend fuzzChannel (intRange 0 16383)


fuzzSysExByte : Fuzzer Byte
fuzzSysExByte =
    intRange 0 127


listOfLength : Fuzzer a -> Int -> Fuzzer (List a)
listOfLength fuzzer listLen =
    List.foldl
        (Fuzz.map2 (::))
        (Fuzz.constant [])
        (List.repeat listLen fuzzer)


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.andThen
        (listOfLength fuzzer)
        (intRange 1 32)


fuzzSysExEvent : Fuzzer MidiEvent
fuzzSysExEvent =
    Fuzz.map
        (SysEx F0)
        (nonEmptyList fuzzSysExByte)


fuzzMidiEvent : Fuzzer MidiEvent
fuzzMidiEvent =
    Fuzz.oneOf
        [ fuzzNoteOn
        , fuzzNoteOff
        , fuzzNoteAfterTouch
        , fuzzControlChange
        , fuzzProgramChange
        , fuzzPitchBend
        , fuzzSysExEvent
        ]


toByteString : List Int -> String
toByteString list =
    String.fromList (List.map Char.fromCode list)


suite : Test
suite =
    describe "MIDI tests"
        [ fuzz fuzzMidiEvent "Go from Elm type to \"Binary\" and back" <|
            \event ->
                Expect.equal
                    (Ok event)
                    (parseMidiEvent (toByteString (Generate.event event)))
        , fuzz
            (Fuzz.tuple
                ( fuzzChannel, fuzzNote )
            )
            "NoteOn with velocity zero looks like NoteOff with velocity zero."
          <|
            \( channel, note ) ->
                let
                    noteOn =
                        NoteOn channel note 0

                    noteOff =
                        NoteOff channel note 0
                in
                    Expect.equal
                        (parseMidiEvent (toByteString (Generate.event noteOn)))
                        (parseMidiEvent (toByteString (Generate.event noteOff)))
        ]
