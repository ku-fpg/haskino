-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- hArduino allows Haskell programs to control Arduino boards (<http://www.arduino.cc>)
-- and peripherals, using the Firmata protocol (<http://firmata.org>).
--
-- For details, see: <http://leventerkok.github.com/hArduino>.
-------------------------------------------------------------------------------
module System.Hardware.DeepArduino (
  -- * Communication functions
  openArduino, closeArduino, send, 
  -- * Deep embeddings
  Arduino(..) , Procedure(..), Query(..), Local(..), TaskProcedure(..)
  -- * Programming the Arduino
  -- ** Pins
  , Pin(..), PinMode(..)--, setPinMode
  -- , analog, digital, pin, Pin, PinMode(..)--, setPinMode
  -- ** Analog input/output (PWM)
  --, analogRead, analogWrite
  -- ** Digital I/O
  --, digitalWrite, digitalRead
  -- ** Programming with triggers
  -- , waitFor, waitAny, waitAnyHigh, waitAnyLow
  -- ** Receiving and sending pulses
  --, pulse, pulseIn_hostTiming, pulseOut_hostTiming
  -- * Misc utilities
  --, setAnalogSamplingInterval, pullUpResistor, delay, time, timeOut
  --, queryFirmware
 )
 where

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm
-- import System.Hardware.DeepArduino.Firmata
