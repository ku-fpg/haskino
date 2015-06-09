-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Blink
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module Blink where

import Control.Monad (forever)

import Data.Bits (shiftL)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm


main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = DigitalPin 13
    let iled = getInternalPin conn led
    let port = pinPort iled
    let portVal = 1 `shiftL` (fromIntegral $ pinPortIndex iled)

    send conn (setPinMode led OUTPUT)
    forever $ do 
        send conn $ do 
            digitalPortWrite port portVal
            delay 1000
            digitalPortWrite port 0
            delay 1000

