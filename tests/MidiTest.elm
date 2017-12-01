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


fuzzMidiEvent : Fuzzer MidiEvent
fuzzMidiEvent =
    Fuzz.oneOf
        [ fuzzNoteOn
        , fuzzNoteOff
        , fuzzNoteAfterTouch
        , fuzzControlChange
        , fuzzProgramChange
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
        ]
