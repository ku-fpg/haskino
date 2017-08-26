-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Strong.JingleBells
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- A (pretty bad!) rendering of Jingle Bells on a piezo speaker
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Strong.JingleBells where

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.Piezo

-- | Notes for jingle-bells. Expecting a nice rendering from this encoding
-- on a piezo speaker would be naive.. However, it's still recognizable!
jingleBells :: [(Note, Duration)]
jingleBells =  m1 ++ m1 ++ m3 ++ m4 ++ wait ++ m5 ++ m6 ++ m7 ++ m8 ++ wait
            ++ m1 ++ m1 ++ m3 ++ m4 ++ wait ++ m5 ++ m6 ++ m15 ++ m16
  where m1   = [(E, Quarter), (E, Quarter), (E, Half)]
        m3   = [(E, Quarter), (G, Quarter), (C, Quarter), (D, Quarter)]
        m4   = [(E, Whole)]
        m5   = replicate 4 (F, Quarter)
        m6   = (F, Quarter) : replicate 3 (E, Quarter)
        m7   = [(E, Quarter), (D, Quarter), (D, Quarter), (E, Quarter)]
        m8   = [(D, Half), (G, Half)]
        m15  = [(G, Quarter), (G, Quarter), (F, Quarter), (D, Quarter)]
        m16  = [(C, Whole)]
        wait = [(R, Half)]

-- | Play the jingle-bells on a PWM line, attached to pin 3. We use a 
-- tempo of @75@; which is fairly fast. For a slower rendring try @150@
-- or higher values.
--
-- The circuit simple has a piezo speaker attached to pin 3.
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/Haskino/SamplePrograms/Schematics/Piezo.png>>
main :: IO ()
main = withArduino False "/dev/cu.usbmodem1421" $ do
                pz <- speaker 75 3
                playNotes pz jingleBells
