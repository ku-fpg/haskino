-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Blink
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Haskino allows Haskell programs to control Arduino boards 
-- (<http://www.arduino.cc>) and peripherals
--
-- For details, see: <http://kufpg.github.com/Haskino>.
-------------------------------------------------------------------------------
module System.Hardware.Haskino (
  -- * Communication functions
  openArduino, closeArduino, withArduino, send, ArduinoConnection
  -- * Deep embeddings
  , Arduino(..) , Command(..), Procedure(..), Local(..)
  -- * Programming the Arduino
  -- ** Pins
  , Pin, PinMode(..), setPinMode, setPinModeE
  -- ** Gereral utils
  , systemReset, queryFirmware
  -- ** Digital IO
  , digitalWrite, digitalRead, digitalWriteE, digitalReadE  
  -- ** Programming with triggers
  --, waitFor, waitAny, waitAnyHigh, waitAnyLow
  -- ** Analog IO
  , analogWrite, analogRead, analogWriteE, analogReadE
  -- ** I2C
  , SlaveAddress, i2cRead, i2cWrite, i2cConfig
  -- ** Pulse
  --, pulse
  -- ** Servo
  --, MinPulse, MaxPulse, servoConfig
  -- ** Scheduler
  , TaskLength, TaskID, TimeMillis, TimeMicros, TaskPos, queryAllTasks, queryTask
  , createTask, deleteTask, delayMillis, delayMicros, scheduleTask, scheduleReset
  , queryTaskE, deleteTaskE, delayMillisE, delayMicrosE, scheduleTaskE
  -- ** Stepper
  --, StepDevice, StepType(..), NumSteps, StepSpeed, StepAccel, StepPerRev
  --, StepDelay(..), StepDir(..), stepperConfig, stepperStep
  -- ** Control structures
  , loop, while, ifThenElse
  -- ** Expressions
  , Expr(..), lit, newRemoteRef, readRemoteRef, writeRemoteRef
  , writeEffectRemoteRef, modifyRemoteRef
 )
 where

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Comm
import System.Hardware.Haskino.Expr
import Data.Boolean
