module CoMidi
    ( Track
    , Header
    , MidiEvent(..)
    , MidiMessage
    , MidiRecording
    , normalise
    , parse
    , translateRunningStatus
    ) where

{-|  Library for parsing MIDI file contents using parser combinators,

# Definition

# Data Types
@docs Header, Track, MidiEvent, MidiMessage,  MidiRecording

# Functions
@docs normalise, parse, translateRunningStatus

-}

import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Char exposing (..)
import Bitwise exposing (..)
import Char exposing (fromCode, toCode)
import String exposing (fromList, toList)
import Debug exposing (..)
import Maybe exposing (withDefault)
-- import List exposing (foldLeft)


type alias Ticks = Int

{-| Midi Event -}
type MidiEvent = -- meta messages
                  SequenceNumber Int
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
                | Unspecified Int (List Int)
                 -- channel messages
                | NoteOn Int Int Int
                | NoteOff Int Int Int
                | NoteAfterTouch Int Int Int
                | ControlChange Int Int Int
                | ProgramChange Int Int
                | ChannelAfterTouch Int Int 
                | PitchBend Int Int
                | RunningStatus Int Int

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
-- int8 = log "int8" <$> (toCode <$> anyChar)

{- parse a signed binary 8 bit integer -}
signedInt8 : Parser Int
signedInt8 = 
   (\i -> if (topBitSet i) then i - 256 else i) <$> int8

{- parse a specific binary 8 bit integer -}
bchar : Int -> Parser Int
bchar val = toCode <$> char (fromCode(val))
-- bchar val = log "bchar" <$> ( toCode <$> char (fromCode(val)))

{- parse an 8 bit integer lying within a range -}
brange : Int -> Int -> Parser Int
brange l r =
  let
    f a = toCode a >= l && toCode a <= r
  in 
    toCode <$> satisfy f 

{- parse a choice between a pair of 8 bit integers -}
bchoice : Int -> Int -> Parser Int
bchoice x y = bchar x <|> bchar y

notTrackEnd : Parser Int
notTrackEnd =
   let
     c = fromCode 0x2F
   in 
     toCode <$> noneOf [c]

-- fixed length integers 

int16 : Parser Int
int16 = 
   let 
     toInt16 a b = shiftLeft a 8 + b
   in
     toInt16 <$> int8 <*> int8

int24 : Parser Int
int24 = 
   let 
     toInt24 a b c = shiftLeft a 16 + shiftLeft b 8 + c 
   in
     toInt24 <$> int8 <*> int8 <*> int8

int32 : Parser Int
int32 = 
   let 
     toInt32 a b c d = shiftLeft a 24 + shiftLeft b 16 + shiftLeft c 8 + d 
   in
     toInt32 <$> int8 <*> int8 <*> int8 <*> int8

-- variable length integers 
-- (need to somehow check this for lengths above 16 bits)
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
midi = midiHeader `andThen` midiTracks

{- this version of the top level parser just parses many tracks
   without checking whether the track count agrees with the header
midi0 : Parser MidiRecording
midi0 = (,) <$> midiHeader <*> midiTracks0

midiTracks0 : Parser (List Track)
midiTracks0 = many1 midiTrack <?> "midi tracks"
-}

{- simple parser for headers which assumes chunk size is 6
midiHeader : Parser Header
midiHeader = string "MThd" 
               *> int32 
               *> ( Header <$>  int16 <*> int16 <*> int16 )  
               <?> "header"
-}

{- parser for headers which quietly eats any extra bytes if we have a non-standard chunk size -} 
midiHeader : Parser Header
midiHeader = string "MThd" 
               *> 
                 let 
                   h = headerChunk <$> int32 <*> int16 <*> int16 <*> int16
                 in 
                   consumeOverspill h 6
                   <?> "header"      

midiTracks : Header -> Parser MidiRecording
midiTracks h = makeTuple h  <$> count h.trackCount midiTrack  <?> "midi tracks"

{- we don't place TrackEnd events into the parse tree - there is no need.
   The end of the track is implied by the end of the event list 
-}
midiTrack : Parser Track
midiTrack = string "MTrk" *> int32 *> many1 midiMessage <* trackEndMessage <?> "midi track"
-- midiTrack = string "MTrk" *> int32 *> manyTill midiMessage trackEndMessage  <?> "midi track"

-- Note - it is important that runningStatus is placed last because of its catch-all definition               
midiMessage : Parser MidiMessage
midiMessage = 
   (,) <$> varInt 
        <*>  choice [ metaEvent
                    , noteOn
                    , noteOff
                    , noteAfterTouch
                    , controlChange
                    , programChange 
                    , channelAfterTouch
                    , pitchBend 
                    , runningStatus ]
        <?> "midi message"

-- metadata parsers

metaEvent : Parser MidiEvent
metaEvent = 
   bchar 0xFF 
     *> choice [ parseSequenceNumber
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
               , parseUnspecified ]
     <?> "meta event"

parseSequenceNumber : Parser MidiEvent
parseSequenceNumber = SequenceNumber <$> (bchar 0x00 *> bchar 0x02 *> int16 <?> "sequence number")

{- parse a simple string-valued meta event -}
parseMetaString : Int -> Parser String
parseMetaString target = String.fromList <$> 
                          (bchar target *> varInt `andThen` (\l -> count l anyChar))

parseText : Parser MidiEvent
parseText = log "text" <$> (Text <$> parseMetaString 0x01 <?> "text" )

parseCopyright : Parser MidiEvent
parseCopyright = Copyright <$> parseMetaString 0x02 <?> "copyright"

parseTrackName : Parser MidiEvent
parseTrackName = log "track name" <$> (TrackName <$> parseMetaString 0x03 <?> "track name")

parseInstrumentName : Parser MidiEvent
parseInstrumentName = InstrumentName <$> parseMetaString 0x04 <?> "instrument name"

parseLyrics : Parser MidiEvent
parseLyrics = Lyrics <$> parseMetaString 0x05 <?> "lyrics"

parseMarker : Parser MidiEvent
parseMarker = log "marker" <$> (Marker <$> parseMetaString 0x06 <?> "marker" )

parseCuePoint : Parser MidiEvent
parseCuePoint = CuePoint <$> parseMetaString 0x07 <?> "cue point"

parseChannelPrefix : Parser MidiEvent
parseChannelPrefix = ChannelPrefix <$> (bchar 0x20 *> bchar 0x01 *> int8 <?> "channel prefix")

parseTempoChange : Parser MidiEvent
parseTempoChange = log "set tempo" <$> (Tempo <$> (bchar 0x51 *> bchar 0x03 *> int24 ) <?> "tempo change")

parseSMPTEOffset : Parser MidiEvent
parseSMPTEOffset = bchar 0x54 *> bchar 0x03 *> (SMPTEOffset <$> int8 <*> int8 <*> int8 <*> int8 <*> int8 <?> "SMTPE offset" )

parseTimeSignature : Parser MidiEvent
parseTimeSignature = log "time sig" <$> (bchar 0x58 *> bchar 0x04 *> (buildTimeSig <$> int8 <*> int8 <*> int8 <*> int8 ) <?> "time signature" )

parseKeySignature : Parser MidiEvent
parseKeySignature = log "key sig" <$>  (bchar 0x59 *> bchar 0x02 *> (KeySignature <$> signedInt8 <*> int8))

parseSequencerSpecific : Parser MidiEvent
parseSequencerSpecific = SequencerSpecific <$> parseMetaString 0x7F <?> "sequencer specific"

parseSysEx : Parser MidiEvent
parseSysEx = SysEx <$> (String.fromList <$> (bchoice 0xF0 0xF7 *> varInt `andThen` (\l -> count l anyChar))) <?> "system exclusive"

{- parse an unspecified meta event
   The possible range for the type is 00-7F. Not all values in this range are defined, but programs must be able 
   to cope with (ie ignore) unexpected values by examining the length and skipping over the data portion.
   We cope by accepting any value here except TrackEnd which is the terminating condition for the list of MidiEvents
   and so must not be recognized here 
-}
parseUnspecified : Parser MidiEvent
parseUnspecified = log "unspecified" <$> (Unspecified <$> notTrackEnd <*> (int8 `andThen` (\l -> count l int8 )))

{- no longer used
parseTrackEnd : Parser MidiEvent
parseTrackEnd =   log "track end" <$>  (bchar 0x2F *> bchar 0x00 *> succeed TrackEnd <?> "track end" )
-}

{- parse an entire Track End message - not simply the event -}
trackEndMessage : Parser ()
trackEndMessage =   log "track end" <$>  (varInt *> bchar 0xFF *> bchar 0x2F *> bchar 0x00 *> succeed () <?> "track end" )

-- channel parsers

noteOn : Parser MidiEvent
noteOn = log "note on" <$> ( buildNote <$> brange 0x90 0x9F <*> int8 <*> int8 <?> "note on" )

noteOff : Parser MidiEvent
noteOff = log "note off" <$> ( buildNoteOff <$> brange 0x80 0x8F <*> int8 <*> int8 <?> "note off" )

noteAfterTouch : Parser MidiEvent
noteAfterTouch = log "note afterTouch" <$> ( buildNoteAfterTouch <$> brange 0xA0 0xAF <*> int8 <*> int8 <?> "note after touch" )

controlChange : Parser MidiEvent
controlChange = log "control change" <$> ( buildControlChange <$> brange 0xB0 0xBF <*> int8 <*> int8 <?> "control change" )

programChange : Parser MidiEvent
programChange = log "program change" <$> ( buildProgramChange <$> brange 0xC0 0xCF <*> int8 <?> "program change" )

channelAfterTouch : Parser MidiEvent
channelAfterTouch = log "channel afterTouch" <$> ( buildChannelAfterTouch <$> brange 0xD0 0xDF <*> int8 <?> "channel after touch")

pitchBend : Parser MidiEvent
pitchBend = log "pitch bend" <$> ( buildPitchBend <$> brange 0xE0 0xEF <*> int8 <*> int8 <?> "pitch bend")

{- running status is somewhat anomalous.  It inherits the 'type' last event parsed, which must be a channel event. 
   This inherited channel event type is not put into the parse tree - this is left to an interpreter -}
runningStatus : Parser MidiEvent
runningStatus = log "running status" <$> ( RunningStatus <$> brange 0x00 0x7F  <*> int8 <?> "running status")

-- result builder

{- build a Header and make the chunk length available so that any overspill bytes can
   later be quietly ignored
-}
headerChunk : Int -> Int -> Int -> Int -> (Int, Header)
headerChunk l a b c =
  (l, Header a b c)

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

{- consume the overspill from a non-standard size chunk 
   actual is the parsed actual chunk size followed by the chunk contents (which are returned)
   expected is the expected size of the chunk
   consume the rest if the difference suggests an overspill of unwanted chunk material
-}
consumeOverspill : Parser (Int, a) -> Int -> Parser a
consumeOverspill actual expected =
  actual `andThen` 
    (\(cnt, rest) -> 
      map (\_ -> rest) 
       <| skip 
       <| count (cnt - expected) int8 )

topBitSet : Int -> Bool
topBitSet n = n `and` 0x80 > 0 

clearTopBit : Int -> Int
clearTopBit n = n  `and` 0x7f 

shiftLeftSeven : Int -> Int
shiftLeftSeven n = shiftLeft n 7  

makeTuple : a -> b -> (a,b)
makeTuple a b = (a,b)


{- Experimental and not working how I want it: expect a value matching the supplied function -}
expect : (a -> Bool) -> Parser a -> Parser a
expect f p = p `andThen` 
          (\b -> case f b of
            True -> succeed b
            _ -> fail [ "unexpected:" ++ toString b ]
          )

-- translation of Running Status messages to NoteOn/NoteOff etc

{- Translate the next message in a track if it has a Running Status event.
   Keep track of state - the event that first kicks of sequence of Running Status events.
   If the incoming event happens in the context of channel voice message state, then translate
   the message to that of the saved state (but using the parameters from the Running Status) 
   Otherwise, if not a Running Status event, just keep hold of the event unchanged.
-}
translateNextEvent : MidiMessage -> (MidiEvent, List MidiMessage) -> (MidiEvent, List MidiMessage)
translateNextEvent nextMessage acc =
  let
    (state, events) = acc
    (ticks, next) = nextMessage
  in case next of
    RunningStatus x y ->
      let 
        translatedStatus = interpretRS state x y
      in case translatedStatus of
        Unspecified _ _ ->            -- couldn't translate the running status so drop it
           (state, events)
        _ ->                          -- could translate the running status so adopt it
        (state, (ticks, translatedStatus) :: events)
    other -> (other, nextMessage :: events)  -- just update the state

{- we can interpret the running status if we have a legitimate last event state which is a channel voice event -}
interpretRS : MidiEvent -> Int -> Int -> MidiEvent
interpretRS last x y =
   case last of 
     NoteOn chan _ _ -> 
       if (y == 0) then
         NoteOff chan x y
       else
         NoteOn chan x y
     NoteOff chan _ _ -> 
       NoteOff chan x y
     NoteAfterTouch chan _ _ -> 
       NoteAfterTouch chan x y
     ControlChange chan _ _ -> 
       ControlChange chan x y
     ProgramChange _ _ -> 
       ProgramChange x y
     ChannelAfterTouch _ _ -> 
       ChannelAfterTouch x y
     PitchBend _ _ -> 
       PitchBend x y
     _ -> 
       Unspecified 0 []

translateAllRunningStatus : Track -> Track
translateAllRunningStatus =
   List.foldl translateNextEvent ( Unspecified 0 [], []) 
   >> snd
   >> List.reverse
   
   
          
-- exported functions

{-| entry point - Parse a normalised MIDI file image -}
parse : String -> Result.Result String MidiRecording
parse s =
  case Combine.parse midi s of 
  -- case Combine.parse (midi <* end) s of
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

{-| translate the Running Status messages in each track to the expanded form (NoteOn/NoteOff etc) -}
translateRunningStatus : Result.Result String MidiRecording -> Result.Result String MidiRecording
translateRunningStatus res =
   case res of
     Ok mr -> 
       let
         header = fst mr
         tracks = snd mr |> List.map translateAllRunningStatus
       in
         Ok (header, tracks) 
     err ->
       err
      

        
