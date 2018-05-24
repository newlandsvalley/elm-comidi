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


generateSysExByte : Generator Byte
generateSysExByte =
    Random.int 0 127


generateByte : Generator Byte
generateByte =
    Random.int 0 255


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
        (\xs -> SysEx F0 xs)
        (Fuzz.list fuzzSysExByte)


generateSysExFileEvent : Generator MidiEvent
generateSysExFileEvent =
    let
        unescaped : Generator MidiEvent
        unescaped =
            Random.map
                (SysEx F0)
                (Random.andThen
                    (\nBytes -> Random.list nBytes generateSysExByte)
                    -- NOTE: The parser blows the stack if this is 2048.
                    (Random.int 0 204)
                )

        escaped : Generator MidiEvent
        escaped =
            Random.map
                (SysEx F7)
                (Random.andThen
                    (\nBytes -> Random.list nBytes generateByte)
                    -- NOTE: The parser blows the stack if this is 2048.
                    (Random.int 0 204)
                )
    in
        Random.choices
            [ unescaped, escaped ]


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



-- Note: The type is identical to a track, but these are regular MIDI events
-- and not MIDI file events (like in generateTrack).


fuzzMidiEventSequence : Fuzzer (List ( Ticks, MidiEvent ))
fuzzMidiEventSequence =
    Fuzz.list (Fuzz.map2 (,) (intRange 0 0x0FFFFFFF) fuzzMidiEvent)


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


generateMidiFileEvent : Generator MidiEvent
generateMidiFileEvent =
    Random.choices (generateSysExFileEvent :: commonEventGenerators)


generateMidiMessage : Generator MidiMessage
generateMidiMessage =
    Random.pair (Random.int 0 0x0FFFFFFF) generateMidiFileEvent


generateMidiRecording : Generator MidiRecording
generateMidiRecording =
    let
        generateSingleTrack =
            Random.map2
                (\ticks track ->
                    SingleTrack ticks track
                )
                (Random.int 1 0x7FFF)
                (generateTrack)

        generateMultipleTracks tracksType =
            Random.map2
                (\ticks tracks ->
                    MultipleTracks tracksType ticks tracks
                )
                (Random.int 1 0x7FFF)
                (Random.andThen
                    (\nTracks -> Random.list nTracks generateTrack)
                    (Random.int 0 16)
                )

        generators =
            [ generateSingleTrack
            , generateMultipleTracks Simultaneous
            , generateMultipleTracks Independent
            ]
    in
        Random.choices generators


shrinkMidiMessage : Shrinker MidiMessage
shrinkMidiMessage =
    Shrink.noShrink


shrinkMidiTrack : Shrinker Track
shrinkMidiTrack =
    Shrink.list shrinkMidiMessage


shrinkMidiRecordingSameFormat : Shrinker MidiRecording
shrinkMidiRecordingSameFormat midi =
    case midi of
        SingleTrack ticksPerBeat track ->
            Shrink.map SingleTrack (Shrink.int ticksPerBeat)
                |> Shrink.andMap (shrinkMidiTrack track)

        MultipleTracks tracksType ticksPerBeat tracks ->
            Shrink.map (MultipleTracks tracksType) (Shrink.int ticksPerBeat)
                |> Shrink.andMap (Shrink.list shrinkMidiTrack tracks)


shrinkMidiRecordingChangeFormat : Shrinker MidiRecording
shrinkMidiRecordingChangeFormat midi =
    case midi of
        MultipleTracks tracksType ticksPerBeat ((track :: []) as tracks) ->
            (shrinkMidiRecording (SingleTrack ticksPerBeat track))

        _ ->
            Shrink.noShrink midi


shrinkMidiRecording =
    Shrink.merge shrinkMidiRecordingChangeFormat shrinkMidiRecordingSameFormat


fuzzMidiRecording : Fuzzer MidiRecording
fuzzMidiRecording =
    Fuzz.custom
        generateMidiRecording
        shrinkMidiRecording


toByteString : List Int -> String
toByteString list =
    String.fromList (List.map Char.fromCode list)


toFileEvent event =
    case event of
        -- Note we need to add in the EOX byte when storing
        -- sysex messages in a MIDI file.
        SysEx F0 bytes ->
            SysEx F0 (bytes ++ [ eox ])

        _ ->
            event


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
        , fuzz fuzzMidiEventSequence "Ensure toFileEvent helper works correctly." <|
            \midiEventSequence ->
                let
                    midiMessages =
                        List.map (\( t, e ) -> ( t, toFileEvent e )) midiEventSequence

                    recording =
                        SingleTrack 1 midiMessages
                in
                    Expect.true
                        "Generated recording is valid."
                        (validRecording recording)
        ]
