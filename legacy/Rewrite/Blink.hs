{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written with the expression based version of the commands and procedures
-- introduced in version 0.3 of Haskino
-------------------------------------------------------------------------------

module Main where

import Data.Word
import System.Hardware.Haskino

led :: Word8
led = 13

delay :: Word32
delay = 1000

blink :: Arduino ()
blink = do
    setPinMode led OUTPUT
    blinkLoop
  where
    blinkLoop :: Arduino ()
    blinkLoop = do
        digitalWrite led True
        delayMillis delay
        digitalWrite led False
        delayMillis delay
        blinkLoop

blinkExample :: IO ()
blinkExample = withArduino False "/dev/cu.usbmodem1421" blink

main :: IO ()
main = compileProgram blink "blink.ino"
