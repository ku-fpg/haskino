-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.SevenSegment
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Control a single seven-segment display, echoing user's key presses
-- on it verbatim. We use a shift-register to reduce the number of
-- pins we need on the Arduino to control the display.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.SevenSegment  where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Data.Bits           (testBit)
import Data.Word           (Word8)
import System.IO           (hSetBuffering, stdin, BufferMode(NoBuffering))

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.ShiftRegisters
import System.Hardware.DeepArduino.Parts.SevenSegmentCodes

-- | Connections for the Texas Instruments 74HC595 shift-register. Datasheet: <http://www.ti.com/lit/ds/symlink/sn74hc595.pdf>.
-- In our circuit, we merely use pins 8 thru 12 on the Arduino to control the 'serial', 'enable', 'rClock', 'sClock', and 'nClear'
-- lines, respectively. Since we do not need to read the output of the shift-register, we leave the 'bits' field unconnected.
sr :: SR_74HC595
sr = SR_74HC595 { serial  = digital 8
                , nEnable = digital 9
                , rClock  = digital 10
                , sClock  = digital 11
                , nClear  = digital 12
                , mbBits  = Nothing
                }

-- | Seven-segment display demo. For each key-press, we display an equivalent pattern
-- on the connected 7-segment-display. Note that most characters are not-mappable, so
-- we use approximations if available. We use a shift-register to reduce the pin
-- requirements on the Arduino, setting the bits serially.
--
-- Parts:
--
--   * The seven-segment digit we use is a common-cathode single-digit display, such as
--     TDSG5150 (<http://www.vishay.com/docs/83126/83126.pdf>), or Microvity's IS121,
--     but almost any such digit would do. Just pay attention to the line-connections,
--     and do not forget the limiting resistors: 220 ohm's should do nicely.
--
--   * The shift-register is Texas-Instruments 74HC595: <http://www.ti.com/lit/ds/symlink/sn74hc595.pdf>.
--     Make sure to connect the register output lines to the seven-segment displays with the corresponding
--     letters. That is, shift-registers @Q_A@ (Chip-pin 15) should connect to segment @A@; @Q_B@ (Chip-pin 1)
--     to segment @B@, and so on. We do not use the shift-register @Q_H'@ (Chip-pin 9) in this design.
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/DeepArduino/SamplePrograms/Schematics/SevenSegment.png>>
sevenSegment :: IO ()
sevenSegment = withArduino False "/dev/cu.usbmodem1421" $ do
                  initialize sr
                  liftIO $ do hSetBuffering stdin NoBuffering
                              putStrLn "Seven-Segment-Display demo."
                              putStrLn "For each key-press, we will try to display it as a 7-segment character."
                              putStrLn "If there is no good mapping (which is common), we'll just display a dot."
                              putStrLn ""
                              putStrLn "Press-keys to be shown on the display, Ctrl-C to quit.."
                  forever repl
 where pushWord w = do mapM_ (push sr) [w `testBit` i | i <- [0..7]]
                       store sr
       repl = do c <- liftIO getChar
                 case char2SS c of
                   Just w  -> pushWord w
                   Nothing -> pushWord (0x01::Word8) -- the dot, which also nicely covers the '.'
