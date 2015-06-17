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

module Eeprom where

import Control.Monad (forever)
import Data.Bits     ((.&.), shiftR)
import Data.Word     (Word8, Word16)

import System.Hardware.DeepArduino

i2CEnable :: ArduinoConnection -> IO ()
i2CEnable c = send c $ i2cConfig 0

eepromRead :: SlaveAddress -> Word8 -> Word8 -> Arduino [Word8]
eepromRead sa addr count = do
        i2cWrite sa [hi, lo]
        ws <- i2cRead sa Nothing count
        return ws
  where lo =  addr             .&. 0xFF   -- first eight bits
        hi = (addr `shiftR` 8) .&. 0x7F   -- seven extra high-bits

eepromWrite :: SlaveAddress -> Word8 -> [Word8] -> Arduino ()
eepromWrite sa addr ws = do
        i2cWrite sa ([hi, lo] ++ ws)
        delay 10
  where lo =  addr             .&. 0xFF   -- first eight bits
        hi = (addr `shiftR` 8) .&. 0x7F   -- seven extra high-bits
        count = length ws

eeprom :: IO ()
eeprom = do
    conn <- openArduino False "/dev/cu.usbmodem1421"

    i2CEnable conn

    eews <- send conn $ do
        eepromWrite 0x50 4 [0xAA]
        eepromRead 0x50 0 10
    print eews

    closeArduino conn
