-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Shallow.Eeprom
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demo of Microchip 24LC256 comaptible I2C EEPROM.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Shallow.Eeprom where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.Eeprom

eeprom :: IO ()
eeprom = do
    conn <- openArduino True "/dev/cu.usbmodem1421"

    send conn $ do
        liftIO $ putStrLn "Clearing 16 bytes at address 0x100"
        eepromEnable
        eepromClear 0x50 0x100 16
        eews <- eepromRead 0x50 0x100 16
        liftIO $ print eews
        liftIO $ putStrLn "Writing 16 bytes of [1,2,3,4,5,6,7,8] address 0x104"
        eepromWrite 0x50 0x104 [1,2,3,4,5,6,7,8]
        eews <- eepromRead 0x50 0x100 16
        liftIO $ print eews

    closeArduino conn
