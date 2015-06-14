-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Parts.Servo
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Abstractions for servo motors. See "System.Hardware.Arduino.SamplePrograms.Servo" for
-- example uses.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Parts.Servo(
   -- * Attaching a servo motor on a pin
     Servo, attach
   -- * Setting servo position
   , setAngle
   ) where

import Control.Monad (when)
import Data.Bits     (shiftR, (.&.))
import Data.Maybe    (fromMaybe)

import System.Hardware.Arduino
import System.Hardware.Arduino.Comm
import System.Hardware.Arduino.Data

-- | A servo motor. Note that this type is abstract, use 'attach' to
-- create an instance.
data Servo = Servo { servoPin :: IPin  -- ^ The internal-pin that controls the servo
                   , minPulse :: Int   -- ^ Pulse-width (microseconds) for the minumum (0-degree) angle.
                   , maxPulse :: Int   -- ^ Pulse-width (microseconds) for the maximum (typically 180-degree) angle.
                   }

-- | Create a servo motor instance. The default values for the min/max angle pulse-widths, while typical,
-- may need to be adjusted based on the specs of the actual servo motor. Check the data-sheet for your
-- servo to find the proper values. The default values of @544@ and @2400@ microseconds are typical, so you might
-- want to start by passing 'Nothing' for both parameters and adjusting as necessary.
attach :: Pin            -- ^ Pin controlling the servo. Should be a pin that supports SERVO mode.
       -> Maybe Int      -- ^ Pulse-width (in microseconds) for the minumum 0-degree angle. Default: @544@.
       -> Maybe Int      -- ^ Pulse-width (in microseconds) for the maximum, typically 180-degree, angle. Default: @2400@.
       -> Arduino Servo
attach p mbMin mbMax
  | Just m <- mbMin, m < 0
  = die "Servo.attach: minimum pulse width must be positive" ["Received: " ++ show m]
  | Just m <- mbMax, m < 0
  = die "Servo.attach: maximum pulse width must be positive" ["Received: " ++ show m]
  | True
  = do let minPulse = fromMaybe 544  mbMin
           maxPulse = fromMaybe 2400 mbMax
       debug $ "Attaching servo on pin: " ++ show p ++ " with parameters: " ++ show (minPulse, maxPulse)
       when (minPulse >= maxPulse) $ die "Servo.attach: min pulse duration must be less than max pulse duration"
                                         [ "Received min-pulse: " ++ show minPulse
                                         , "Received max-pulse: " ++ show maxPulse
                                         ]
       setPinMode p SERVO
       (ip, _) <- convertAndCheckPin "Servo.attach" p SERVO
       return Servo { servoPin = ip
                    , minPulse = fromMaybe 544  mbMin
                    , maxPulse = fromMaybe 2400 mbMax
                    }

-- | Set the angle of the servo. The argument should be a number between 0 and 180,
-- indicating the desired angle setting in degrees.
setAngle :: Servo -> Int -> Arduino ()
setAngle Servo{servoPin, minPulse, maxPulse} angle
  | angle < 0 || angle > 180
  = die "Servo.setAngle: angle must be between 0 and 180." ["Received: " ++ show angle]
  | True
  = do let duration = minPulse + ((maxPulse - minPulse) * angle) `div` 180
       debug $ "Setting servo on pin: " ++ show servoPin ++ " " ++ show angle ++ " degrees, via a pulse of " ++ show duration ++ " microseconds."
       -- In arduino, the most we can send is 16383; not that a servo should need such a large value, but
       -- just in case
       when (duration >= 16383) $ die "Servo.setAngle angle setting: out-of-range."
                                      [ "Servo pin         : " ++ show servoPin
                                      , "Angle required    : " ++ show angle
                                      , "Min pulse duration: " ++ show minPulse
                                      , "Max pulse duration: " ++ show maxPulse
                                      , "Duration needed   : " ++ show duration
                                      , "Exceeds max value : 16383"
                                      ]
       let msb = fromIntegral $ (duration `shiftR` 7) .&. 0x7f
           lsb = fromIntegral $ duration .&. 0x7f
       send $ AnalogPinWrite servoPin lsb msb
