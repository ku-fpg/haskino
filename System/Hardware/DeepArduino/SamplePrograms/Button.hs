-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Blink
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino.comm (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Button where

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
button :: IO ()
button = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    -- Currently there is no Firmata command to modify just one pin on a 
    -- digital port.  History storage in the connection ala hArduino is not
    -- yet completely reimplementd (plus that is not possible as a Firmata
    -- Scheduled Task), so for now the entire 8 bit port is written.
    let but = digital 2
    let ibut = getInternalPin conn but
    let butPort = pinPort ibut

    let led = digital 13
    let iled = getInternalPin conn led
    let ledPort = pinPort iled
    let ledPortVal = 1 `shiftL` (fromIntegral $ pinPortIndex iled)

    first <- send conn $ do 
      setPinMode but INPUT
      setPinMode led OUTPUT
      digitalReport butPort True
      cur <- digitalPinRead but
      return cur
    
    loop conn but first ledPort ledPortVal
  where
    loop conn pin cur ledPort ledPortVal = do
      new <- send conn (digitalPinRead pin)
      when (cur /= new) $ do 
            print new
            send conn $ digitalPortWrite ledPort (if new then ledPortVal else 0)
      loop conn pin new  ledPort ledPortVal
