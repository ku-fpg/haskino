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
    conn <- openArduino False "/dev/cu.usbmodem1411"
    let led = DigitalPin 13
    send conn $ do
          setPinMode led OUTPUT
    forever $ do 
        send conn $ do 
            digitalPinWrite led True
        threadDelay (1000 * 1000)
        send conn $ do 
            digitalPinWrite led False
        threadDelay (1000 * 1000)
{-
main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1411"
    let led = DigitalPin 13
    send conn $ do
          setPinMode led OUTPUT
          forever $ do 
            digitalPinWrite led True
            delay 1000
            digitalPinWrite led False
            delay 1000
-}

