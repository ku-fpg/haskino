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

module Blink where

import Control.Monad (forever)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm


blink :: IO ()
blink = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = digital 13

    send conn (setPinMode led OUTPUT)
    forever $ do 
        send conn $ do 
            digitalPinWrite led True
            delay 1000
            digitalPinWrite led False
            delay 1000

