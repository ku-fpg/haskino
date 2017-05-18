-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Parts.Piezo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for piezo speakers.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Haskino.Parts.Piezo(
   -- * Declaring a piezo speaker
     Piezo, speaker
   -- * Notes you can play, and durations
   , Note(..), Duration(..)
   -- * Playing a note, rest, or silencing
   , playNote, rest, silence
   -- * Play a sequence of notes:
   , playNotes
   ) where

import           Data.Maybe              (fromMaybe)
import           Data.Word               (Word16, Word32)

import           System.Hardware.Haskino

-- | A piezo speaker. Note that this type is abstract, use 'speaker' to
-- create an instance.
data Piezo = Piezo { piezoPin :: Pin      -- ^ The internal-pin that controls the speaker
                   , tempo    :: Word32   -- ^ Tempo for the melody
                   }

-- | Create a piezo speaker instance.
speaker :: Word32         -- ^ Tempo. Higher numbers mean faster melodies; in general.
        -> Pin            -- ^ Pin controlling the piezo. Should be a pin that supports PWM mode.
        -> Arduino Piezo
speaker t p = do debug $ "Attaching speaker on pin: " ++ show p
                 setPinMode p OUTPUT
                 return Piezo { piezoPin = p, tempo = t }

-- | Musical notes, notes around middle-C
data Note     = A | B | C | D | E | F | G | R  deriving (Eq, Show)  -- R is for rest

-- | Beat counts
data Duration = Whole | Half | Quarter | Eight deriving (Eq, Show)

-- | Convert a note to its frequency appropriate for Piezo
frequency :: Note -> Word16
frequency n = fromMaybe 0 (n `lookup` fs)
 where fs = [(A, 440), (B, 493), (C, 261), (D, 294), (E, 329), (F, 349), (G, 392), (R, 0)]

-- | Convert a duration to a delay amount
interval :: Piezo -> Duration -> Word32
interval p Whole   = 8 * interval p Eight
interval p Half    = 4 * interval p Eight
interval p Quarter = 2 * interval p Eight
interval p Eight   = tempo p

-- | Turn the speaker off
silence :: Piezo -> Arduino ()
silence (Piezo p _) = analogWrite p 0

-- | Keep playing a given note on the piezo:
setNote :: Piezo -> Note -> Arduino ()
setNote (Piezo p _) n = analogWrite p (fromIntegral $ frequency n)

-- | Play the given note for the duration
playNote :: Piezo -> (Note, Duration) -> Arduino ()
playNote pz (n, d) = do setNote pz n
                        delayMillis (interval pz d)
                        silence pz

-- | Play a sequence of notes with given durations:
playNotes :: Piezo -> [(Note, Duration)] -> Arduino ()
playNotes pz = go
  where go []            = silence pz
        go (nd@(_, d):r) = do playNote pz nd
                              delayMillis (interval pz d `div` 3) -- heuristically found.. :-)
                              go r

-- | Rest for a given duration:
rest :: Piezo -> Duration -> Arduino ()
rest pz d = delayMillis (interval pz d)
