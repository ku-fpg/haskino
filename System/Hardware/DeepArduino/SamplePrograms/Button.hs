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
import Control.Monad.Trans (liftIO)

import System.Hardware.DeepArduino

-- | Read the value of a push-button (NO - normally open)
-- connected to input pin 2 on the Arduino. We will continuously
-- monitor and print the value as it changes. Also, we'll turn
-- the led on pin 13 on when the switch is pressed.
--
-- The wiring is straightforward: Simply put a push-button between
-- digital input 2 and +5V, guarded by a 10K resistor:
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/DeepArduino/SamplePrograms/Schematics/Button.png>>

button :: IO ()
button = withArduino False "/dev/cu.usbmodem1421" $ do
            setPinMode led OUTPUT
            setPinMode pb  INPUT
            digitalReport pb True   
            go =<< digitalRead pb
 where pb   = digital 2
       led  = digital 13
       go s = do liftIO $ putStrLn $ "Button is currently " ++ if s then "ON" else "OFF"
                 digitalWrite led s
                 go =<< waitFor pb
