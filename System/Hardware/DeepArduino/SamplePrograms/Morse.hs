-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Morse
-- Copyright   :  (c) Antoine R. Dumont, Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Morse code blinker. Original by Antoine R. Dumont, modified to simplify
-- and fit into the existing examples structure.
-------------------------------------------------------------------------------
module System.Hardware.DeepArduino.SamplePrograms.Morse where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Data.Char           (toUpper)
import Data.List           (intercalate)
import Data.Maybe          (fromMaybe)

import System.Hardware.DeepArduino

-- | A dit or a dah is all we need for Morse:
-- A @dit@ is a dot; and a @dah@ is a dash in the Morsian world.
-- We use 'LBreak' and 'WBreak' to indicate a letter and a word break
-- so we can insert some delay between letters and words as we
-- transmit.
data Morse = Dit | Dah | LBreak | WBreak
           deriving Show

-- | Morse code dictionary
dict :: [(Char, [Morse])]
dict = map encode m
  where encode (k, s) = (k, map (\c -> if c == '.' then Dit else Dah) s)
        m = [ ('A', ".-"   ), ('B', "-..." ), ('C', "-.-." ), ('D', "-.."  ), ('E', "."    )
            , ('F', "..-." ), ('G', "--."  ), ('H', "...." ), ('I', ".."   ), ('J', ".---" )
            , ('K', "-.-"  ), ('L', ".-.." ), ('M', "--"   ), ('N', "-."   ), ('O', "---"  )
            , ('P', ".--." ), ('Q', "--.-" ), ('R', ".-."  ), ('S', "..."  ), ('T', "-"    )
            , ('U', "..-"  ), ('V', "...-" ), ('W', ".--"  ), ('X', "-..-" ), ('Y', "-.--" )
            , ('Z', "--.." ), ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--")
            , ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---..")
            , ('9', "----."), ('+', ".-.-."), ('/', "-..-."), ('=', "-...-")
            ]

-- | Given a sentence, decode it. We simply drop any letters that we
-- do not have a mapping for.
decode :: String -> [Morse]
decode = intercalate [WBreak] . map (intercalate [LBreak] . map cvt) . words
 where cvt c = fromMaybe [] $ toUpper c `lookup` dict

-- | Given a morsified sentence, compute the delay times. A 'Left' value means
-- turn the led on that long, a 'Right' value means turn it off that long.
morsify :: [Morse] -> [Either Int Int]
morsify = map t
  where unit     = 300
        t Dit    = Left  $ 1 * unit
        t Dah    = Left  $ 3 * unit
        t LBreak = Right $ 3 * unit
        t WBreak = Right $ 7 * unit

-- | Finally, turn a full sentence into a sequence of blink on/off codes
transmit :: Pin -> String -> Arduino ()
transmit p = sequence_ . concatMap code . morsify . decode
  where code (Left i)  = [digitalWrite p True,  delay i, digitalWrite p False, delay i]
        code (Right i) = [digitalWrite p False, delay i]

-- | A simple demo driver. To run this example, you only need the Arduino connected to your
-- computer, no other hardware is needed. We use the internal led on pin 13. Of course,
-- you can attach a led to pin 13 as well, for artistic effect.
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Blink.png>>
morseDemo :: IO ()
morseDemo = withArduino False "/dev/cu.usbmodemfd131" $ do
                setPinMode led OUTPUT
                forever send
 where  led  = digital 13
        send = do liftIO $ putStr "Message? "
                  m <- liftIO getLine
                  transmit led m
