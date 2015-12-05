elm-comidi
===========

This is a parser for MIDI files written in pure elm and using the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.

It is a component of an intended eventual assemblage of modules that will allow me to replace [MIDI.js](https://github.com/mudcube/MIDI.js/) in my [tradtunestore](https://github.com/newlandsvalley/tradtunestore) web application. This is used to play MIDI files that have been automatically generated from tunes in [abc notation](http://www.lesession.co.uk/abc/abc_notation.htm) and that have been submitted by users. Other modules will include a soundfont loader and a web-audio player.  

The intention is to provide a fully conformant MIDI parser although the usage described above is relatively simple because the generated files conform to a consistent MIDI type-0 format.

Limitations
-----------

The following aspects have so far not been addressed:

*  Running Status messages
*  Ignoring unexpected chunk types
*  Handling unexpected chunk or event length (e.g. header size greater than 6) 
*  Fully contextual error reporting

In other respects, it appears to be perfectly adequate to parse the abovementioned files.


 




