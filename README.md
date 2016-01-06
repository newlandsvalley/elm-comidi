elm-comidi
===========

This is a parser for MIDI files written in pure elm and using the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.

To parse a MIDI string you can use:

    midi
      |> normalise 
      |> parse

or:

    midi 
      |> normalise 
      |> parse 
      |> translateRunningStatus

if the input contains running status messages that you would like to translate to the underlying channel messages.


Hopefully, this version is a fully conformant parser which is happy with Type-0, Type-1 and Type-2 files.

Dependencies
------------

*  Bogdanp/elm-combine  2.0.0
*  elm-lang/core 3.0.0

Examples
--------

Examples have been split off into the test directory with an independent elm-package.json in order to cut down dependencies in the library itself.

Limitations
-----------

Type-2 has not yet been tested.

Running Status messages are supported and placed in the parse tree as such.  A function 'translateRunningStatus' can be used after parsing to convert them into the appropriate NoteOn/NoteOff etc. message. It would perhaps be better if they were immediately translated by the parser into the appropriate channel voice message.  
 




