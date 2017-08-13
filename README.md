elm-comidi
===========

This is a parser for MIDI written in pure Elm 0.18 and using the [elm-community parser combinators](https://github.com/elm-community/parser-combinators) library.

Unfortunately, Elm has very poor support for binary data.  As from Elm 0.18, you can no longer [load MIDI via http](https://github.com/elm-lang/http/issues/11) - instead you must rely on ports. The native javascript must use the [readAsBinaryString](https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsBinaryString) function to access the MIDI recording which is then loaded into an Elm String. elm-comidi's normalise function is used to make sense of this String.

To parse a MIDI string that represents a recording you can use:

    midi
      |> normalise
      |> parse

or, if the MIDI uses running status messages:

    midi
      |> normalise
      |> parse
      |> translateRunningStatus

so that these messages are translated to the underlying channel messages.

On the other hand, you may merely need to parse MIDI events (such as note on or note off). This is more likely if you are connecting directly
to a MIDI device and need to parse the stream of event messages as the instrument is played.  To do this, use:

    midiEvent
      |> parseMidiEvent


This version is intended to be a fully conformant parser which is happy with Type-0, Type-1 and Type-2 files.

Dependencies
------------

*  elm-lang/core 5.0.0
*  elm-community/parser-combinators 1.1.0
