-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.ButtonWait
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.Button where

import System.Hardware.DeepArduino

-- | Wait for the value of a push-button (NO - normally open)
-- connected to input pin 2 on the Arduino to change. We will 
-- print "Button Pressed" when it is pressed and "Button Released"
-- when it is released. Also, we'll turn the led on pin 13 on when 
-- the switch is pressed.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Analog.png>>
buttonWait :: IO ()
buttonWait = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let but = digital 3
    let led = digital 13

    send conn $ do 
      setPinMode but INPUT
      setPinMode led OUTPUT
      digitalReport but True
    
    loop conn but led
  where
    loop conn pin led = do
      send conn $ do
        waitAnyHigh [pin]
        digitalWrite led True
      putStrLn "Button Pressed"
      send conn $ do 
        waitAnyLow [pin]
        digitalWrite led False
      putStrLn "Button Released"
      loop conn pin led
