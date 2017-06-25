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
  , withArduinoWeak , withArduinoStrong, withArduinoApp
  , sendWeak, sendStrong, sendApp
  -- * Deep embeddings
  , Arduino(..) , ArduinoCommand(..), ArduinoProcedure(..), Processor(..)
  -- * Programming the Arduino
  -- ** Pins
  , Pin, PinMode(..), IntMode(..), setPinMode, setPinModeE
  -- ** Gereral utils
  , systemReset, queryFirmware, queryProcessor, queryFirmwareE, queryProcessorE
  -- ** Digital IO
  , digitalWrite, digitalPortWrite, digitalRead, digitalPortRead
  , digitalWriteE, digitalPortWriteE, digitalReadE, digitalPortReadE
  -- ** Analog IO
  , analogWrite, analogRead, analogWriteE, analogReadE
  -- ** Speaker
  , tone, noTone, toneE, noToneE
  -- ** I2C
  , SlaveAddress, i2cRead, i2cWrite, i2cConfig, i2cReadE, i2cWriteE 
  -- ** Servo
  , servoDetach, servoDetachE, servoWrite, servoWriteE, servoWriteMicros
  , servoWriteMicrosE, servoAttach, servoAttachE, servoAttachMinMax
  , servoAttachMinMaxE, servoRead, servoReadE, servoReadMicros, servoReadMicrosE
  -- ** Time
  , millis, micros, millisE, microsE, delayMillis, delayMicros,delayMillisE, delayMicrosE
  -- ** Scheduler
  , TaskLength, TaskID, TimeMillis, TimeMicros, TaskPos, queryAllTasks, queryTask
  , createTask, createTaskE
  , deleteTask, scheduleTask, scheduleReset, queryTaskE
  , queryAllTasksE, deleteTaskE, scheduleTaskE, bootTaskE
  , takeSem, giveSem, takeSemE, giveSemE, attachInt, attachIntE, detachInt, detachIntE
  , interrupts, noInterrupts
  -- ** Stepper
  --, StepDevice, StepType(..), NumSteps, StepSpeed, StepAccel, StepPerRev
  --, StepDelay(..), StepDir(..), stepperConfig, stepperStep
  , stepper2Pin, stepper2PinE, stepper4Pin, stepper4PinE, stepperSetSpeed
  , stepperSetSpeedE, stepperStep ,stepperStepE
  -- ** Control structures
  -- ToDo: Cleanup IfThenElse's
  --, loop, ifThenElseUnit, ifThenElseUnitE, loopE, forInE, ArduinoConditional(..)
  , loop, loopE, ArduinoConditional(..), ifThenElseUnit, ifThenElseUnitE, forInE
  -- ** Expressions
  , Expr(..), RemoteRef, lit, newRemoteRef, readRemoteRef, writeRemoteRef
  , modifyRemoteRef, (++*), (*:), (!!*), len, pack, litString, showE, showFFloatE
  , ExprB, abs_, rep_, lessE, lesseqE, greatE, greateqE, eqE, neqE
  -- ** Debugging
  , debug, debugE, debugListen, die, deframe
  -- ** Compiler
  , compileProgram
  -- ** Recursion
  , Iter(..), done, step, iterLoop, recurErr, ExprEither(..), ArduinoIterate(..)  
 )
 where

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Comm
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils
import System.Hardware.Haskino.Decode
import System.Hardware.Haskino.Compiler
import System.Hardware.Haskino.Show
import Data.Boolean
