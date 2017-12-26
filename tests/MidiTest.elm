module MidiTest exposing (..)

import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (..)
import Random.Pcg as Random exposing (Generator)
import Shrink exposing (Shrinker)
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


generateChannel : Generator Channel
generateChannel =
    Random.int 0 15


generateNote : Generator Note
generateNote =
    Random.int 0 127


generateVelocity : Generator Velocity
generateVelocity =
    Random.int 0 127


generatePositiveVelocity : Generator Velocity
generatePositiveVelocity =
    Random.int 1 127


generateControllerNumber : Generator Int
generateControllerNumber =
    Random.int 0 119


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


generateNoteOn : Generator MidiEvent
generateNoteOn =
    Random.map3 NoteOn generateChannel generateNote generatePositiveVelocity


generateNoteOff : Generator MidiEvent
generateNoteOff =
    Random.map3 NoteOff generateChannel generateNote generateVelocity


generateNoteAfterTouch : Generator MidiEvent
generateNoteAfterTouch =
    Random.map3 NoteAfterTouch generateChannel generateNote generateVelocity


generateControlChange : Generator MidiEvent
generateControlChange =
    Random.map3 ControlChange generateChannel generateControllerNumber generateVelocity


generateProgramChange : Generator MidiEvent
generateProgramChange =
    Random.map2 ProgramChange generateChannel generateVelocity


generateChannelAfterTouch : Generator MidiEvent
generateChannelAfterTouch =
    Random.map2 ChannelAfterTouch generateChannel generateVelocity


generatePitchBend : Generator MidiEvent
generatePitchBend =
    Random.map2 PitchBend generateChannel (Random.int 0 16383)


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
        (\xs -> SysEx F0 (xs ++ [ 0xF7 ]))
        (Fuzz.list fuzzSysExByte)


commonEvents : List (Fuzzer MidiEvent)
commonEvents =
    [ fuzzNoteOn
    , fuzzNoteOff
    , fuzzNoteAfterTouch
    , fuzzControlChange
    , fuzzProgramChange
    , fuzzPitchBend
    ]


commonEventGenerators : List (Generator MidiEvent)
commonEventGenerators =
    [ generateNoteOn
    , generateNoteOff
    , generateNoteAfterTouch
    , generateControlChange
    , generateProgramChange
    , generatePitchBend
    ]


fuzzMidiEvent : Fuzzer MidiEvent
fuzzMidiEvent =
    Fuzz.oneOf <|
        commonEvents
            ++ [ fuzzSysExEvent ]


generateMidiFileEvent : Generator MidiEvent
generateMidiFileEvent =
    Random.choices commonEventGenerators


generateMidiMessage : Generator MidiMessage
generateMidiMessage =
    Random.pair (Random.int 0 0x0FFFFFFF) generateMidiFileEvent


generateMidiRecording : Generator MidiRecording
generateMidiRecording =
    Random.andThen
        (\format ->
            Random.andThen
                (\numTracks ->
                    Random.map2
                        (\ticks ->
                            \tracks ->
                                ( { formatType = format
                                  , ticksPerBeat = ticks
                                  }
                                , tracks
                                )
                        )
                        (Random.int 1 0x7FFF)
                        (Random.list 1 generateTrack)
                )
                (if format == 0 then
                    Random.constant 1
                 else
                    Random.frequency
                        [ ( 50, Random.constant 1 )
                        , ( 30, Random.int 2 8 )
                        , ( 20, Random.int 9 16 )
                        ]
                )
        )
        (Random.int 0 2)


shrinkHeader : Shrinker Header
shrinkHeader h =
    -- TODO(rofer): Currently we shrink formatType without regard to how many
    -- tracks a recording has leading to the possibility that we could shrink
    -- to an invalid type.
    Shrink.andMap
        (Shrink.int h.ticksPerBeat)
        (Shrink.map Header (Shrink.int h.formatType))


shrinkMidiMessage : Shrinker MidiMessage
shrinkMidiMessage =
    Shrink.noShrink


shrinkMidiTrack : Shrinker Track
shrinkMidiTrack =
    Shrink.list shrinkMidiMessage


shrinkMidiRecording : Shrinker MidiRecording
shrinkMidiRecording =
    -- TODO(rofer): We don't enforce that we always have to have at least one track.
    Shrink.tuple
        ( shrinkHeader, Shrink.list shrinkMidiTrack )


fuzzMidiRecording : Fuzzer MidiRecording
fuzzMidiRecording =
    Fuzz.custom
        generateMidiRecording
        shrinkMidiRecording


generateTrack : Generator Track
generateTrack =
    Random.andThen
        (\numEvents -> Random.list numEvents generateMidiMessage)
        (Random.frequency
            [ ( 25, Random.constant 0 )
            , ( 50, Random.int 1 8 )
            , ( 24, Random.int 128 256 )
            , ( 1, Random.int 1024 2048 )
            ]
        )


toByteString : List Int -> String
toByteString list =
    String.fromList (List.map Char.fromCode list)


suite : Test
suite =
    describe "MIDI tests"
        [ fuzz fuzzMidiEvent "Go from MidiEvent to \"Binary\" and back" <|
            \event ->
                Expect.equal
                    (Ok event)
                    (parseMidiEvent (toByteString (Generate.event event)))
        , fuzz fuzzMidiRecording "Go from MidiRecording to \"Binary\" and back" <|
            \recording ->
                Expect.equal
                    (Ok recording)
                    (parse (toByteString (Generate.recording recording)))
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
