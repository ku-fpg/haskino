-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Parts.Eeprom
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for Microchip 24LC256 comaptible I2C EEPROM. 
-- See "System.Hardware.KansasAmber.SamplePrograms.Eeprom" for example uses.
-------------------------------------------------------------------------------

module System.Hardware.KansasAmber.Parts.Eeprom(
   -- * Reading and writing from an eeprom
     eepromClear, eepromEnable, eepromRead, eepromWrite
   ) where

import Data.Bits     ((.&.), shiftR)
import Data.Word     (Word8, Word16)

import System.Hardware.KansasAmber

eepromEnable :: Arduino ()
eepromEnable = i2cConfig

eepromRead :: SlaveAddress -> Word16 -> Word8 -> Arduino [Word8]
eepromRead sa addr count = do
        i2cWrite sa [fromIntegral hi, fromIntegral lo]
        ws <- i2cRead sa count
        return ws
  where lo =  addr             .&. 0xFF   -- first eight bits
        hi = (addr `shiftR` 8) .&. 0x7F   -- seven extra high-bits

eepromWrite :: SlaveAddress -> Word16 -> [Word8] -> Arduino ()
eepromWrite sa addr ws = do
        i2cWrite sa ([fromIntegral hi, fromIntegral lo] ++ ws)
        -- Delay 10ms for write to complete before attempting read
        delayMillis 10
  where lo =  addr             .&. 0xFF   -- first eight bits
        hi = (addr `shiftR` 8) .&. 0x7F   -- seven extra high-bits
        count = length ws

eepromClear :: SlaveAddress -> Word16 -> Int -> Arduino ()
eepromClear sa addr count = eepromWrite sa addr $ take count $ repeat 0
