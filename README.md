elm-comidi
===========

This is a parser for MIDI files written in pure elm and using the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.

It is a component of an intended eventual assemblage of modules that will allow me to replace [MIDI.js](https://github.com/mudcube/MIDI.js/) in my [tradtunestore](https://github.com/newlandsvalley/tradtunestore) web application. This is used to play MIDI files that have been automatically generated from tunes in [abc notation](http://www.lesession.co.uk/abc/abc_notation.htm) and that have been submitted by users. Other modules will include a soundfont loader and a web-audio player.  

Hopefully, this version is a fully conformant parser which is happy with Type-0, Type-1 and Type-2 files.

Limitations
-----------

Type-2 has not yet been tested.

Running Status messages are supported and placed in the parse tree as such.  A function 'translateRunningStatus' can be used after parsing to convert them into the appropriate NoteOn/NoteOff etc. message. It would perhaps be better if they were immediately translated by the parser into the appropriate channel voice message.  
 




