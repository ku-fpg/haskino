-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Button
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Button where

import Control.Monad (when)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm

-- | Read the value of a push-button (NO - normally open)
-- connected to input pin 2 on the Arduino. We will continuously
-- monitor and print the value as it changes. Also, we'll turn
-- the led on pin 13 on when the switch is pressed.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Button.png>>
button :: IO ()
button = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let but = digital 3
    let led = digital 13

    first <- send conn $ do 
      setPinMode but INPUT
      setPinMode led OUTPUT
      digitalReport but True
      cur <- digitalRead but
      return cur
    
    loop conn but first led
  where
    loop conn pin cur led = do
      new <- send conn (digitalRead pin)
      when (cur /= new) $ do 
            print new
            send conn $ digitalWrite led new
      loop conn pin new led
