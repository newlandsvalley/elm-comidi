module CoMidi
    ( Track
    , Header
    , MidiEvent(..)
    , MidiMessage
    , MidiRecording
    , normalise
    , parse
    ) where

{-|  Library for parsing MIDI file contents using parser combinators,

# Definition

# Data Types
@docs Header, Track, MidiEvent, MidiMessage,  MidiRecording

# Functions
@docs normalise, parse

-}

import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Char exposing (..)
import Bitwise exposing (..)
import Char exposing (fromCode, toCode)
import String exposing (fromList, toList)
import Debug exposing (..)

type alias Ticks = Int

{-| Midi Event -}
type MidiEvent = MidiEvent
                -- meta messages
                | SequenceNumber Int
                | Text String
                | Copyright String
                | TrackName String
                | InstrumentName String
                | Lyrics String
                | Marker String
                | CuePoint String
                | ChannelPrefix Int
                | Tempo Int
                | SMPTEOffset Int Int Int Int Int
                | TimeSignature Int Int Int Int
                | KeySignature Int Int
                | SequencerSpecific String
                | SysEx String
                | TrackEnd
                -- channel messages
                | NoteOn Int Int Int
                | NoteOff Int Int Int
                | NoteAfterTouch Int Int Int
                | ControlChange Int Int Int
                | ProgramChange Int Int
                | ChannelAfterTouch Int Int 
                | PitchBend Int Int

{-| Midi Message -}    
type alias MidiMessage = (Ticks, MidiEvent)

{-| Midi Track -}
type alias Track = List MidiMessage

{-| Midi Header -}
type alias Header = 
   { formatType : Int
   , trackCount : Int
   , ticksPerBeat : Int
   }

{-| Midi Recording -}
type alias MidiRecording = (Header, List Track)


-- low level parsers 

{- parse a binary 8 bit integer -}
int8 : Parser Int
int8 = toCode <$> anyChar
-- int8 = log "int8:" <$> (toCode <$> anyChar)

{- parse a signed binary 8 bit integer -}
signedInt8 : Parser Int
signedInt8 = 
   (\i -> if (topBitSet i) then i - 256 else i) <$> int8

{- parse a specific binary 8 bit integer -}
bchar : Int -> Parser Int
bchar val = toCode <$> char (fromCode(val))
-- bchar val = log "bchar:" <$> ( toCode <$> char (fromCode(val)))

{- parse an 8 bit integer lying within a range -}
brange : List Int -> Parser Int
brange xs = choice <| List.map bchar xs

{- parse a choice between a pair of 8 bit integers -}
bchoice : Int -> Int -> Parser Int
bchoice x y = bchar x <|> bchar y

shiftInt8 : Parser (Int -> Int)
shiftInt8 = (\i -> \j -> (shiftLeft i 8) + j) <$> int8

int16 : Parser Int
int16 = shiftInt8 <*> int8
-- int16 = log "int16:" <$> (shiftInt8 <*> int8)

int24 : Parser Int
int24 = shiftInt8 <*> (shiftInt8 <*> int8)

int32 : Parser Int
int32 = shiftInt8 <*> (shiftInt8 <*> (shiftInt8 <*> int8))
-- int32 = log "int32:" <$> (shiftInt8 <*> (shiftInt8 <*> (shiftInt8 <*> int8)))

varInt : Parser Int
varInt = 
  int8 `andThen` 
       (\n -> if (topBitSet n) 
          then
            ( (+)  ((clearTopBit >> shiftLeftSeven) n)) <$> varInt
          else 
             succeed n 
       )
                
{- just for debug purposes - consume the rest of the input -}
rest : Parser (List Char)
rest = many anyChar

-- top level parsers

midi : Parser MidiRecording
midi = (,) <$> midiHeader <*> midiTracks

midiHeader : Parser Header
midiHeader = string "MThd" 
               *> int32 
               *> ( Header <$>  int16 <*> int16 <*> int16 )  
               <?> "header"

midiTracks : Parser (List Track)
midiTracks = many1 midiTrack

midiTrack : Parser Track
midiTrack = string "MTrk" *> int32 *> many1 midiMessage 
                   
midiMessage : Parser MidiMessage
midiMessage = (,) <$> varInt 
                  <*>  choice [ metaEvent
                              , noteOn
                              , noteOff
                              , noteAfterTouch
                              , controlChange
                              , programChange 
                              , channelAfterTouch
                              , pitchBend ]

-- metadata parsers

metaEvent : Parser MidiEvent
metaEvent = bchar 0xFF *> choice [ parseSequenceNumber
                                 , parseText
                                 , parseCopyright
                                 , parseTrackName
                                 , parseInstrumentName
                                 , parseLyrics
                                 , parseMarker
                                 , parseCuePoint
                                 , parseChannelPrefix
                                 , parseTempoChange
                                 , parseSMPTEOffset
                                 , parseTimeSignature
                                 , parseKeySignature
                                 , parseSequencerSpecific
                                 , parseSysEx
                                 , parseTrackEnd ]

parseSequenceNumber : Parser MidiEvent
parseSequenceNumber = SequenceNumber <$> (bchar 0x00 *> varInt *> int16)

{- parse a simple string-valued meta event -}
parseMetaString : Int -> Parser String
parseMetaString target = String.fromList <$> 
                          (bchar target *> varInt `andThen` (\l -> count l anyChar))

parseText : Parser MidiEvent
parseText = log "text:" <$> (Text <$> parseMetaString 0x01)

parseCopyright : Parser MidiEvent
parseCopyright = Copyright <$> parseMetaString 0x02

parseTrackName : Parser MidiEvent
parseTrackName = log "track name:" <$> (TrackName <$> parseMetaString 0x03)

parseInstrumentName : Parser MidiEvent
parseInstrumentName = InstrumentName <$> parseMetaString 0x04

parseLyrics : Parser MidiEvent
parseLyrics = Lyrics <$> parseMetaString 0x05

parseMarker : Parser MidiEvent
parseMarker = Marker <$> parseMetaString 0x06

parseCuePoint : Parser MidiEvent
parseCuePoint = CuePoint <$> parseMetaString 0x07

parseChannelPrefix : Parser MidiEvent
parseChannelPrefix = ChannelPrefix <$> (bchar 0x20 *> varInt *> int8)

parseTempoChange : Parser MidiEvent
parseTempoChange = log "set tempo:" <$> (Tempo <$> (bchar 0x51 *> varInt *> int24 ))

parseSMPTEOffset : Parser MidiEvent
parseSMPTEOffset = bchar 0x54 *> varInt *> (SMPTEOffset <$> int8 <*> int8 <*> int8 <*> int8 <*> int8)

parseTimeSignature : Parser MidiEvent
parseTimeSignature = log "time sig:" <$> (bchar 0x58 *> varInt *> (buildTimeSig <$> int8 <*> int8 <*> int8 <*> int8 ))

parseKeySignature : Parser MidiEvent
parseKeySignature = log "key sig:" <$>  (bchar 0x59 *> varInt *> (KeySignature <$> signedInt8 <*> int8))

parseSequencerSpecific : Parser MidiEvent
parseSequencerSpecific = SequencerSpecific <$> parseMetaString 0x7F

parseSysEx : Parser MidiEvent
parseSysEx = SysEx <$> (String.fromList <$> (bchoice 0xF0 0xF7 *> varInt `andThen` (\l -> count l anyChar)))

parseTrackEnd : Parser MidiEvent
parseTrackEnd =   log "track end:" <$>  (bchar 0x2F *> varInt *> succeed TrackEnd)

-- channel parsers

noteOn : Parser MidiEvent
noteOn = log "note on:" <$> ( buildNote <$> brange [0x90..0x9F] <*> int8 <*> int8 )

noteOff : Parser MidiEvent
noteOff = log "note off:" <$> ( buildNoteOff <$> brange [0x80..0x8F] <*> int8 <*> int8 )

noteAfterTouch : Parser MidiEvent
noteAfterTouch = log "note afterTouch:" <$> ( buildNoteAfterTouch <$> brange [0xA0..0xAF] <*> int8 <*> int8 )

controlChange : Parser MidiEvent
controlChange = log "control change:" <$> ( buildControlChange <$> brange [0xB0..0xBF] <*> int8 <*> int8 )

programChange : Parser MidiEvent
programChange = log "program change:" <$> ( buildProgramChange <$> brange [0xC0..0xCF] <*> int8 )

channelAfterTouch : Parser MidiEvent
channelAfterTouch = log "channel afterTouch:" <$> ( buildChannelAfterTouch <$> brange [0xD0..0xDF] <*> int8 )

pitchBend : Parser MidiEvent
pitchBend = log "pitch bend:" <$> ( buildPitchBend <$> brange [0xE0..0xEF] <*> int8 <*> int8 )


-- result builders

{- build NoteOn (unless the velocity is zero in which case NoteOff) -}
buildNote : Int -> Int -> Int -> MidiEvent
buildNote cmd note velocity = 
     let channel = cmd `and` 0xF
         isOff = (velocity == 0)
     in case isOff of
       True -> NoteOff channel note velocity
       _ -> NoteOn channel note velocity
       
{- abstract builders that construct MidiEvents that all have the same shape -}
channelBuilder3 : (Int -> Int -> Int -> MidiEvent) -> Int -> Int -> Int -> MidiEvent
channelBuilder3 construct cmd x y = 
     let channel = cmd `and` 0xF
     in 
       construct channel x y       
       
channelBuilder2 : (Int -> Int -> MidiEvent) -> Int -> Int -> MidiEvent
channelBuilder2 construct cmd x  = 
     let channel = cmd `and` 0xF
     in 
       construct channel x      
       
{- build NoteOff -}
buildNoteOff : Int -> Int -> Int -> MidiEvent
buildNoteOff cmd note velocity = channelBuilder3 NoteOff cmd note velocity  
       
{- build Note AfterTouch AKA Polyphonic Key Pressure -}
buildNoteAfterTouch : Int -> Int -> Int -> MidiEvent
buildNoteAfterTouch cmd note pressure = channelBuilder3 NoteAfterTouch cmd note pressure       

{- build Control Change -}
buildControlChange : Int -> Int -> Int -> MidiEvent
buildControlChange cmd num value = channelBuilder3 ControlChange cmd num value

{- build Program Change -}
buildProgramChange : Int -> Int -> MidiEvent
buildProgramChange cmd num  = channelBuilder2 ProgramChange cmd num 

{- build Channel AfterTouch AKA Channel Key Pressure -}
buildChannelAfterTouch : Int -> Int -> MidiEvent
buildChannelAfterTouch cmd num  = channelBuilder2 ChannelAfterTouch cmd num 

{- build Pitch Bend -}
buildPitchBend : Int -> Int -> Int -> MidiEvent
buildPitchBend cmd lsb msb  = channelBuilder2 PitchBend cmd <| lsb + shiftLeftSeven msb 
       
{- build a Time Signature -}
buildTimeSig : Int -> Int -> Int -> Int -> MidiEvent
buildTimeSig nn dd cc bb =
   let denom = 2^dd
   in TimeSignature nn denom cc bb
   
-- utility functions

topBitSet : Int -> Bool
topBitSet n = n `and` 0x80 > 0 

clearTopBit : Int -> Int
clearTopBit n = n  `and` 0x7f 

shiftLeftSeven : Int -> Int
shiftLeftSeven n = shiftLeft n 7  


{- Experimental and not working how I want it: expect a value matching the supplied function -}
expect : (a -> Bool) -> Parser a -> Parser a
expect f p = p `andThen` 
          (\b -> case f b of
            True -> succeed b
            _ -> fail [ "unexpected:" ++ toString b ]
          )
          
-- exported functions

{-| entry point - Parse a normalised MIDI file image -}
parse : String -> Result.Result String MidiRecording
parse s =
  case Combine.parse (midi <* end) s of
  -- case parse (midi <* rest <* end) s of
    (Done n, _) ->
      Ok n

    (Fail ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))


{-| normalise the input before we parse by masking off all but the least significant 8 bits -}
normalise : String -> String
normalise = 
   let f = 
     toCode  >> ( (and) 0xFF) >> fromCode
   in
     String.toList >> List.map f >> String.fromList


        
