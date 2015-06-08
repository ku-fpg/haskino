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

module ScheduledBlink where

import Control.Monad (forever)

import Data.Bits (shiftL)
import Data.Word (Word16)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm

myTask :: Port -> Word16 -> Arduino ()
myTask port portVal = do
        digitalPortWrite port portVal
        delayTask 1000
        digitalPortWrite port 0
        delayTask 1000
        return ()

main :: IO ()
main = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    let led = DigitalPin 13
    let iled = getInternalPin conn led
    let port = pinPort iled
    let portVal = 1 `shiftL` (fromIntegral $ pinPortIndex iled)

    send conn (setPinMode led OUTPUT)
    send conn $ do
        createTask 1 100
        addToTask 1 (myTask port portVal)
        scheduleTask 1 5000
