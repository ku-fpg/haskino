-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.SamplePrograms.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.KansasAmber.SamplePrograms.Blink where

import Control.Monad (forever)

import System.Hardware.KansasAmber

blink :: IO ()
blink = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           setPinMode led OUTPUT
           forever $ do digitalWrite led True
                        delayMillis 1000
                        digitalWrite led False
                        delayMillis 1000

