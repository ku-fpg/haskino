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

import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm


main :: IO ()
main = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    let led = DigitalPin 13
    send conn (setPinMode led OUTPUT)
    forever $ do 
        send conn $ do 
            digitalPinWrite led True
            hostDelay 1000
            digitalPinWrite led False
            hostDelay 1000

