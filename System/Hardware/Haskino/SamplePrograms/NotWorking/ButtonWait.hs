-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ButtonWait
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Reads the value of an analog input, controlled by a 10K potentiometer.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Button where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino

-- | Wait for the value of a push-button (NO - normally open)
-- connected to input pin 2 on the Arduino to change. We will 
-- print "Button Pressed" when it is pressed and "Button Released"
-- when it is released. Also, we'll turn the led on pin 13 on when 
-- the switch is pressed.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/Haskino/SamplePrograms/Schematics/Analog.png>>
buttonWait :: IO ()
buttonWait = withArduino False "/dev/cu.usbmodem1421" $ do
    let but = digital 3
    let led = digital 13
    setPinMode but INPUT
    setPinMode led OUTPUT
    digitalReport but True   
    loop but led
  where
    loop pin led = do
        waitAnyHigh [pin]
        digitalWrite led True
        liftIO $ putStrLn "Button Pressed"
        waitAnyLow [pin]
        digitalWrite led False
        liftIO $ putStrLn "Button Released"
        loop pin led
