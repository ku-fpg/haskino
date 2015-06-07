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

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm


main :: IO ()
main = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    let led = DigitalPin 13
    let port = pinPort $ getInternalPin led
    send conn (setPinMode led OUTPUT)
    forever $ do 
        send conn $ do 
            digitalPortWrite port 0x20
            hostDelay 1000
            digitalPortWrite port 0x00
            hostDelay 1000

