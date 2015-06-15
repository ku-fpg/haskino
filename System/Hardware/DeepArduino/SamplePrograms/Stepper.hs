-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.Servo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Demonstrates basic Servo motor control
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables      #-}

module System.Hardware.Arduino.SamplePrograms.Stepper where

import Control.Monad       (forever)
import Data.Char           (toLower)

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.Stepper

stepper :: NumSteps -> StepDir -> StepSpeed -> IO ()
stepper ns sd ss = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    -- Create the Stepper structure and get the Stepper init function
    (s,init) <- attach conn FourWire OneUs 80 (digital 9) (digital 10) (Just $ digital 11) (Just $ digital 12)
    -- Send the stepperinit to the arduino
    send conn init
    -- Execute the steps
    send conn (step s sd ns ss Nothing)
    closeArduino conn
