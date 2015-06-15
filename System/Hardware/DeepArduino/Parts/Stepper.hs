-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Parts.Stepper
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for stepper motors. 
-- See "System.Hardware.DeepArduino.SamplePrograms.Stepper" for example uses.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.DeepArduino.Parts.Stepper(
   -- * Attaching a stepper motor on a set of pins
     Stepper, attach
   -- * Stepping stepper position
   , step
   ) where

import Control.Concurrent   (putMVar, takeMVar)
import Data.Bits     (shiftR, (.&.))
import Data.Maybe    (fromMaybe)
import Data.Word     (Word16)

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Comm
import System.Hardware.DeepArduino.Data

-- | A stepper motor. Note that this type is abstract, use 'attach' to
-- create an instance.
data Stepper = 
    Stepper { device :: StepDevice
            , p1     :: Pin 
            , p2     :: Pin
            , p3     :: Maybe Pin
            , p4     :: Maybe Pin}

-- | Create a stepper motor instance. The default values for the min/max angle pulse-widths, while typical,
-- may need to be adjusted based on the specs of the actual servo motor. Check the data-sheet for your
-- servo to find the proper values. The default values of @544@ and @2400@ microseconds are typical, so you might
-- want to start by passing 'Nothing' for both parameters and adjusting as necessary.
attach :: ArduinoConnection
       -> StepType
       -> StepDelay
       -> StepPerRev
       -> Pin
       -> Pin
       -> Maybe Pin
       -> Maybe Pin            -- ^ Pin controlling the servo. Should be a pin that supports SERVO mode.
       -> IO (Stepper, Arduino ())
attach c ty d sr p1 p2 p3 p4 = do
    bs <- takeMVar (boardState c)
    let nextStepper = nextStepperDevice bs
    putMVar (boardState c) bs {nextStepperDevice = nextStepper + 1}
    return (Stepper { device = nextStepper, p1 = p1, p2 = p2, p3 = p3, p4 = p4 },
            do debug $ "Attaching " ++ show ty ++ " stepper on pins: " ++ 
                    show p1 ++ "," ++ show p2 ++ "," ++ show p3 ++ "," ++ 
                    show p4 ++ " as device: " ++ show nextStepper ++ 
                    " with delay: " ++ show d ++" and Steps/Rev: " ++ show sr
               stepperConfig nextStepper ty d sr p1 p2 p3 p4)

-- | Set the angle of the servo. The argument should be a number between 0 and 180,
-- indicating the desired angle setting in degrees.
step :: Stepper -> StepDir -> NumSteps -> StepSpeed -> Maybe StepAccel -> Arduino ()
step Stepper{device, p1, p2, p3, p4} dir steps speed accel
  = do debug $ "Stepping stepper device " ++ show device ++ ": " ++ show steps ++ 
               " Steps " ++ show dir ++ " with speed: " ++ show speed ++ 
               " and acceleration: " ++ show accel
       stepperStep device dir steps speed accel

