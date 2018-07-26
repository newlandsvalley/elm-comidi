elm-comidi
===========
[![Build Status](https://travis-ci.org/newlandsvalley/elm-comidi.svg?branch=master)](https://travis-ci.org/newlandsvalley/elm-comidi)

This is a parser for MIDI written in pure Elm 0.18 and using the [elm-community parser combinators](https://github.com/elm-community/parser-combinators) library.

Unfortunately, Elm has very poor support for binary data.  As from Elm 0.18, you can no longer [load MIDI via http](https://github.com/elm-lang/http/issues/11) - instead you must rely on ports. The native javascript must use the [readAsBinaryString](https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsBinaryString) function to access the MIDI recording which is then loaded into an Elm String. elm-comidi's normalise function is used to make sense of this String.

To parse a MIDI string that represents a MIDI file you can use:

    midiFileData
      |> Midi.Parse.normalise
      |> Midi.Parse.file

This attempts to parse the contents and also translates running status messages to the appropriate underlying channel message (note on, note off, aftertouch etc.).

On the other hand, you may merely need to parse MIDI events (such as note on or note off) that come from a Web MIDI connection. In other words, you are connecting directly to a MIDI device through the browser and need to parse the stream of event messages as the instrument is played.  To do this, use:

    midiEventData
      |> Midi.Parse.event

This will attempt to parse an individual event. Note that Web MIDI does not generate running status messages and this method will not attempt to parse them.

The parser is intended to be a fully conformant with the MIDI standard and accepts Type-0, Type-1 and Type-2 files.

Dependencies
------------

*  elm-lang/core 5.0.0
*  elm-community/parser-combinators 1.1.0
