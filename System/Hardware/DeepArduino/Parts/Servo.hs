-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Parts.Servo
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for servo motors. 
-- See "System.Hardware.DeepArduino.SamplePrograms.Servo" for example uses.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.DeepArduino.Parts.Servo(
   -- * Attaching a servo motor on a pin
     Servo, attach
   -- * Setting servo position
   , setAngle
   ) where

import Data.Bits     (shiftR, (.&.))
import Data.Maybe    (fromMaybe)
import Data.Word     (Word16)

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Comm
import System.Hardware.DeepArduino.Data

-- | A servo motor. Note that this type is abstract, use 'attach' to
-- create an instance.
data Servo = Servo { servoPin :: Pin      -- ^ The pin that controls the servo
                   , minPulse :: Int      -- ^ Pulse-width (microseconds) for the minumum (0-degree) angle.
                   , maxPulse :: Int      -- ^ Pulse-width (microseconds) for the maximum (typically 180-degree) angle.
                   }

-- | Create a servo motor instance. The default values for the min/max angle pulse-widths, while typical,
-- may need to be adjusted based on the specs of the actual servo motor. Check the data-sheet for your
-- servo to find the proper values. The default values of @544@ and @2400@ microseconds are typical, so you might
-- want to start by passing 'Nothing' for both parameters and adjusting as necessary.
attach :: Pin            -- ^ Pin controlling the servo. Should be a pin that supports SERVO mode.
       -> Maybe Word16   -- ^ Pulse-width (in microseconds) for the minumum 0-degree angle. Default: @544@.
       -> Maybe Word16   -- ^ Pulse-width (in microseconds) for the maximum, typically 180-degree, angle. Default: @2400@.
       -> (Servo, Arduino ())
attach p mbMin mbMax = 
    (Servo { servoPin = p , 
             minPulse = fromIntegral $ minPulse, 
             maxPulse = fromIntegral $ maxPulse },
     servoConfig p minPulse maxPulse)
  where
    defaultMin = 544
    defaultMax = 2400
    min = fromMaybe defaultMin mbMin
    max = fromMaybe defaultMax mbMax
    minPulse = if min > max then defaultMin else min
    maxPulse = if min > max then defaultMax else max

-- | Set the angle of the servo. The argument should be a number between 0 and 180,
-- indicating the desired angle setting in degrees.
setAngle :: Servo -> Int -> Arduino ()
setAngle Servo{servoPin, minPulse, maxPulse} angle
  | angle < 0 || angle > 180
  = return ()
  | True
  = do let duration = minPulse + ((maxPulse - minPulse) * angle) `div` 180
       analogPinWrite servoPin (fromIntegral $ minimum [duration,16383])
