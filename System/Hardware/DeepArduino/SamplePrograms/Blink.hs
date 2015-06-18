-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Blink where

import Control.Monad (forever)

import System.Hardware.DeepArduino

blink :: IO ()
blink = withArduino False "/dev/cu.usbmodem1421" $ do
           let led = digital 13
           setPinMode led OUTPUT
           forever $ do digitalWrite led True
                        delay 1000
                        digitalWrite led False
                        delay 1000

