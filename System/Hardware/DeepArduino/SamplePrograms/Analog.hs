-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Analog
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino.comm (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Analog where

import Control.Monad (when)
import Data.Bits (shiftL)
import Data.Word (Word16)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm

-- | Read the value of an analog input line. We will print the value
-- on the screen, and also blink a led on the Arduino based on the
-- value. The smaller the value, the faster the blink.
--
-- The circuit simply has a 10K potentiometer between 5V and GND, with
-- the wiper line connected to analog input 3. We also have a led between
-- pin 13 and GND.
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Analog.png>>
analogVal :: IO ()
analogVal = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = digital 13
        pot = analog 3

    first <- send conn $ do 
      setPinMode led OUTPUT
      setPinMode pot ANALOG
      analogReport pot True
      cur <- analogPinRead pot
      return cur
    
    loop conn led first pot
  where
    loop conn pin cur pot = do
      new <- send conn (analogPinRead pot)
      when (cur /= new) $ print new
      send conn $ blinkRate pin new
      loop conn pin new pot 

    blinkRate pin rate = do
      digitalPinWrite pin True
      delay $ fromIntegral rate
      digitalPinWrite pin False
      delay $ fromIntegral rate

