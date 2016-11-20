elm-comidi
===========

This is a parser for MIDI files written in pure elm 0.18 and using the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.

Unfortunately, Elm has very poor support for binary data.  As from Elm 0.18, you can no longer [load MIDI via http](https://github.com/elm-lang/http/issues/11) - instead you must rely on ports. The native javascript must use the [readAsBinaryString](https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsBinaryString) function to access the data which is then loaded into an Elm String. elm-comidi's normalise function is used to make sense of this String.

To parse a MIDI string you can use:

    midi
      |> normalise
      |> parse

or, if the MIDI uses running status messages:

    midi
      |> normalise
      |> parse
      |> translateRunningStatus

so that these messages are translated to the underlying channel messages.


This version is intended to be a fully conformant parser which is happy with Type-0, Type-1 and Type-2 files.

Dependencies
------------

*  Bogdanp/elm-combine  3.1.1
*  elm-lang/core 5.0.0
