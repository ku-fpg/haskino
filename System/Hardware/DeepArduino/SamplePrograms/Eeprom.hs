-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Eeprom
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demo of Microchip 24LC256 comaptible I2C EEPROM.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Eeprom where

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.Eeprom

eeprom :: IO ()
eeprom = do
    conn <- openArduino False "/dev/cu.usbmodem1421"

    putStrLn "Clearing 16 bytes at address 0x100"
    eews <- send conn $ do
        eepromEnable
        eepromClear 0x50 0x100 16
        eepromRead 0x50 0x100 16
    print eews
    putStrLn "Writing 16 bytes of [1,2,3,4,5,6,7,8] address 0x104"
    eews <- send conn $ do
        eepromWrite 0x50 0x104 [1,2,3,4,5,6,7,8]
        eepromRead 0x50 0x100 16
    print eews

    closeArduino conn
