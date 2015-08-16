-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Parts.Stepper
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for stepper motors. 
-- See "System.Hardware.KansasAmber.SamplePrograms.Stepper" for example uses.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.KansasAmber.Parts.Stepper(
   -- * Attaching a stepper motor on a set of pins
     Stepper, attach
   -- * Stepping stepper position
   , step
   ) where

import Control.Concurrent   (putMVar, takeMVar)
import Control.Monad.Trans  (liftIO)

import System.Hardware.KansasAmber
import System.Hardware.KansasAmber.Comm
import System.Hardware.KansasAmber.Data

-- | A stepper motor. Note that this type is abstract, use 'attach' to
-- create an instance.
data Stepper = 
    Stepper { device :: StepDevice
            , p1     :: Pin 
            , p2     :: Pin
            , p3     :: Maybe Pin
            , p4     :: Maybe Pin}

-- | Create and configure a stepper motor instance. 
attach :: ArduinoConnection    -- ^ 
       -> StepType             -- ^ Type of Stepper controller 4-Wire, 2-Wire, Step-Dir
       -> StepDelay            -- ^ Step delay 1us or 2us
       -> StepPerRev           -- ^ Steps per revolution
       -> Pin                  -- ^ Pin 1 for 2 or 4 Wire, Direction Pin for Step-Dir
       -> Pin                  -- ^ Pin 2 for 2 or 4 Wire, Step Pin for Step-Dir
       -> Maybe Pin            -- ^ Pin 3 for 4 Wire
       -> Maybe Pin            -- ^ Pin 3 for 4 Wire
       -> Arduino Stepper
attach c ty d sr p1 p2 p3 p4 = do
    bs <- liftIO $ takeMVar (boardState c)
    let nextStepper = nextStepperDevice bs
    liftIO $ putMVar (boardState c) bs {nextStepperDevice = nextStepper + 1}
    debug $ "Attaching " ++ show ty ++ " stepper on pins: " ++ show p1 ++ "," ++
            show p2 ++ "," ++ show p3 ++ "," ++ show p4 ++ " as device: " ++ 
            show nextStepper ++ " with delay: " ++ show d ++" and Steps/Rev: " ++ 
            show sr
    stepperConfig nextStepper ty d sr p1 p2 p3 p4
    return Stepper { device = nextStepper, p1 = p1, p2 = p2, p3 = p3, p4 = p4 }

-- | Command the Stepper motor to step
-- StepDir is either CCW or CW
-- NumSteps is the number of times to step the motor
-- Step Speed is speed in 0.01 rad/sec
-- i.e. stepper CW 80 50
step :: Stepper -> StepDir -> NumSteps -> StepSpeed -> Maybe StepAccel -> Arduino ()
step Stepper{device, p1, p2, p3, p4} dir steps speed accel
  = do debug $ "Stepping stepper device " ++ show device ++ ": " ++ show steps ++ 
               " Steps " ++ show dir ++ " with speed: " ++ show speed ++ 
               " and acceleration: " ++ show accel
       stepperStep device dir steps speed accel

