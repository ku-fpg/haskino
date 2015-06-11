-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Blink
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino.comm (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module Blink where

import Control.Monad (forever)

import Data.Bits (shiftL)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm


blink :: IO ()
blink = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = digital 13
    -- Currently there is no Firmata command to modify just one pin on a 
    -- digital port.  History storage in the connection ala hArduino is not
    -- yet completely reimplementd (plus that is not possible as a Firmata
    -- Scheduled Task), so for now the entire 8 bit port is written.
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

