-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Servo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demonstrates basic Stepper motor control
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables      #-}

module System.Hardware.Arduino.SamplePrograms.Stepper where

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.Stepper

-- | Control a stepper
-- | StepDir is either CCW or CW
-- | NumSteps is the number of times to step the motor
-- | Step Speed is speed in 0.01 rad/sec
-- | i.e. stepper CW 80 50
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/Stepper.png>>
stepper :: StepDir -> NumSteps -> StepSpeed -> IO ()
stepper sd ns ss = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    -- Create the Stepper structure and get the Stepper init function
    (s,init) <- attach conn FourWire OneUs 8 (digital 8) (digital 9) (Just $ digital 10) (Just $ digital 11)
    -- Send the stepperinit to the arduino
    send conn init
    -- Execute the steps
    send conn (step s sd ns ss Nothing)
    closeArduino conn
