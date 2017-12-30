elm-comidi
===========
[![Build Status](https://travis-ci.org/newlandsvalley/elm-comidi.svg?branch=master)](https://travis-ci.org/newlandsvalley/elm-comidi)

This is a parser for MIDI written in pure Elm 0.18 and using the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.

Unfortunately, Elm has very poor support for binary data.  As from Elm 0.18, you can no longer [load MIDI via http](https://github.com/elm-lang/http/issues/11) - instead you must rely on ports. The native javascript must use the [readAsBinaryString](https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsBinaryString) function to access the MIDI recording which is then loaded into an Elm String. elm-comidi's normalise function is used to make sense of this String.

To parse a MIDI string that represents a recording you can use:

    midi
      |> normalise
      |> parse

This attempts to parse the contents and also translates running status messages to the appropriate underlying channel message (note on, note off, aftertouch etc.).

On the other hand, you may merely need to parse MIDI events (such as note on or note off) that emanate from a Web MIDI connection. In other words, you are connecting directly to a MIDI device through the browser and need to parse the stream of event messages as the instrument is played.  To do this, use:

    midiEvent
      |> parseMidiEvent

This will attempt to parse an individual event.  It works for all events other than System Exclusive events (these have a different format within a MIDI file from that in a stream). Note that Web MIDI does not generate running status messages.

The parser is intended to be a fully conformant, happy with Type-0, Type-1 and Type-2 files.

Dependencies
------------

*  Bogdanp/elm-combine  3.1.1
*  elm-lang/core 5.0.0
