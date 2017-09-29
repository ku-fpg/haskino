-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Data
--                Based on System.Hardware.Arduino.Data
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Underlying data structures
-------------------------------------------------------------------------------
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module System.Hardware.Haskino.Data where

import           Control.Concurrent           (Chan, MVar, ThreadId)
import           Control.Monad.Trans
import           Control.Remote.Monad
import           Data.Int                     (Int8, Int16, Int32)
import           Data.Word                    (Word8, Word16, Word32)
import           System.Hardware.Serialport   (SerialPort)

import           System.Hardware.Haskino.Expr

-----------------------------------------------------------------------------

-- | The Arduino remote monad
newtype Arduino a = Arduino (RemoteMonad ArduinoPrimitive a)
  deriving (Functor, Applicative, Monad)

instance MonadIO Arduino where
  liftIO m = Arduino $ primitive $ LiftIO m

type Pin  = Word8
type PinE = Expr Word8

-- Given a pin number, this function determines which port it belongs to
pinNoPortNo :: Pin -> Word8
pinNoPortNo n = n `quot` 8

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which index it belongs to in its port
pinPortIndex :: Pin -> Word8
pinPortIndex p = p `rem` 8

-- | The mode for a pin.
data PinMode = INPUT
             | OUTPUT
             | INPUT_PULLUP
        deriving (Eq, Show, Enum)

-- | The mode for a triggering an interrupt on a pin.
data IntMode = LOW
             | CHANGE
             | FALLING
             | RISING
        deriving (Eq, Show, Enum)

-- | State of the connection
data ArduinoConnection = ArduinoConnection {
                message       :: String -> IO ()                      -- ^ Current debugging routine
              , bailOut       :: forall a. String -> [String] -> IO a -- ^ Clean-up and quit with a hopefully informative message
              , port          :: SerialPort                           -- ^ Serial port we are communicating on
              , firmwareID    :: String                               -- ^ The ID of the board (as identified by the Board itself)
              , deviceChannel :: Chan Response                        -- ^ Incoming messages from the board
              , processor     :: Processor                            -- ^ Type of processor on board
              , listenerTid   :: MVar ThreadId                        -- ^ ThreadId of the listener
              , refIndex      :: MVar Int                             -- ^ Index used for remote references
              }

type SlaveAddress = Word8
type SlaveAddressE = Expr Word8
type MinPulse = Word16
type MaxPulse = Word16
type TaskLength = Word16
type TaskID = Word8
type TaskIDE = Expr Word8
type TimeMillis = Word32
type TimeMillisE = Expr Word32
type TimeMicros = Word32
type TimeMicrosE = Expr Word32
type TaskPos = Word16
type VarSize = Word8

data ArduinoPrimitive :: * -> * where
     -- Commands
     SystemResetE         ::                                      ArduinoPrimitive (Expr ())
     SetPinModeE          :: PinE -> Expr Word8                -> ArduinoPrimitive (Expr ())
     DigitalPortWriteE    :: PinE -> Expr Word8 -> Expr Word8  -> ArduinoPrimitive (Expr ())
     DigitalWriteE        :: PinE -> Expr Bool                 -> ArduinoPrimitive (Expr ())
     AnalogWriteE         :: PinE -> Expr Word16               -> ArduinoPrimitive (Expr ())
     ToneE                :: PinE -> Expr Word16 -> Maybe (Expr Word32) -> ArduinoPrimitive (Expr ())
     NoToneE              :: PinE                              -> ArduinoPrimitive (Expr ())
     I2CWriteE            :: SlaveAddressE -> Expr [Word8]     -> ArduinoPrimitive (Expr ())
     I2CConfigE           ::                                      ArduinoPrimitive (Expr ())
     SerialBeginE         :: Expr Word8 -> Expr Word32         -> ArduinoPrimitive (Expr ())
     SerialEndE           :: Expr Word8                        -> ArduinoPrimitive (Expr ())
     SerialWriteE         :: Expr Word8 -> Expr Word8          -> ArduinoPrimitive (Expr ())
     SerialWriteListE     :: Expr Word8 -> Expr [Word8]        -> ArduinoPrimitive (Expr ())
     StepperSetSpeedE     :: Expr Word8 -> Expr Int32          -> ArduinoPrimitive (Expr ())
     ServoDetachE         :: Expr Word8                        -> ArduinoPrimitive (Expr ())
     ServoWriteE          :: Expr Word8 -> Expr Int16          -> ArduinoPrimitive (Expr ())
     ServoWriteMicrosE    :: Expr Word8 -> Expr Int16          -> ArduinoPrimitive (Expr ())
     CreateTask           :: TaskID     -> Arduino ()          -> ArduinoPrimitive ()
     CreateTaskE          :: TaskIDE    -> Arduino (Expr ())   -> ArduinoPrimitive (Expr ())
     DeleteTask           :: TaskID                            -> ArduinoPrimitive ()
     DeleteTaskE          :: TaskIDE                           -> ArduinoPrimitive (Expr ())
     ScheduleTask         :: TaskID     -> TimeMillis          -> ArduinoPrimitive ()
     ScheduleTaskE        :: TaskIDE    -> TimeMillisE         -> ArduinoPrimitive (Expr ())
     ScheduleReset        ::                                      ArduinoPrimitive ()
     ScheduleResetE       ::                                      ArduinoPrimitive (Expr ())
     AttachInt            :: Pin  -> TaskID  -> Expr Word8     -> ArduinoPrimitive ()
     AttachIntE           :: PinE -> TaskIDE -> Expr Word8     -> ArduinoPrimitive (Expr ())
     DetachInt            :: Pin                               -> ArduinoPrimitive ()
     DetachIntE           :: PinE                              -> ArduinoPrimitive (Expr ())
     Interrupts           ::                                      ArduinoPrimitive ()
     InterruptsE          ::                                      ArduinoPrimitive (Expr ())
     NoInterrupts         ::                                      ArduinoPrimitive ()
     NoInterruptsE        ::                                      ArduinoPrimitive (Expr ())
     GiveSem              :: Word8                             -> ArduinoPrimitive ()
     GiveSemE             :: Expr Word8                        -> ArduinoPrimitive (Expr ())
     TakeSem              :: Word8                             -> ArduinoPrimitive ()
     TakeSemE             :: Expr Word8                        -> ArduinoPrimitive (Expr ())
     WriteRemoteRefBE     :: RemoteRef Bool    -> Expr Bool    -> ArduinoPrimitive (Expr ())
     WriteRemoteRefW8E    :: RemoteRef Word8   -> Expr Word8   -> ArduinoPrimitive (Expr ())
     WriteRemoteRefW16E   :: RemoteRef Word16  -> Expr Word16  -> ArduinoPrimitive (Expr ())
     WriteRemoteRefW32E   :: RemoteRef Word32  -> Expr Word32  -> ArduinoPrimitive (Expr ())
     WriteRemoteRefI8E    :: RemoteRef Int8    -> Expr Int8    -> ArduinoPrimitive (Expr ())
     WriteRemoteRefI16E   :: RemoteRef Int16   -> Expr Int16   -> ArduinoPrimitive (Expr ())
     WriteRemoteRefI32E   :: RemoteRef Int32   -> Expr Int32   -> ArduinoPrimitive (Expr ())
     WriteRemoteRefIE     :: RemoteRef Int     -> Expr Int     -> ArduinoPrimitive (Expr ())
     WriteRemoteRefL8E    :: RemoteRef [Word8] -> Expr [Word8] -> ArduinoPrimitive (Expr ())
     WriteRemoteRefFloatE :: RemoteRef Float   -> Expr Float   -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefBE    :: RemoteRef Bool    -> Expr Bool    -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefW8E   :: RemoteRef Word8   -> Expr Word8   -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefW16E  :: RemoteRef Word16  -> Expr Word16  -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefW32E  :: RemoteRef Word32  -> Expr Word32  -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefI8E   :: RemoteRef Int8    -> Expr Int8    -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefI16E  :: RemoteRef Int16   -> Expr Int16   -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefI32E  :: RemoteRef Int32   -> Expr Int32   -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefIE    :: RemoteRef Int     -> Expr Int     -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefL8E   :: RemoteRef [Word8] -> Expr [Word8] -> ArduinoPrimitive (Expr ())
     ModifyRemoteRefFloatE :: RemoteRef Float   -> Expr Float   -> ArduinoPrimitive (Expr ())
     Loop                 :: Arduino ()                        -> ArduinoPrimitive ()
     -- ToDo: add SPI commands
     -- Procedures
     QueryFirmware        :: ArduinoPrimitive Word16                   -- ^ Query the Firmware version installed
     QueryFirmwareE       :: ArduinoPrimitive (Expr Word16)                  -- ^ Query the Firmware version installed
     QueryProcessor       :: ArduinoPrimitive Processor                -- ^ Query the type of processor on
     QueryProcessorE      :: ArduinoPrimitive (Expr Word8)
     Micros               :: ArduinoPrimitive Word32
     MicrosE              :: ArduinoPrimitive (Expr Word32)
     Millis               :: ArduinoPrimitive Word32
     MillisE              :: ArduinoPrimitive (Expr Word32)
     DelayMillis          :: TimeMillis -> ArduinoPrimitive ()
     DelayMicros          :: TimeMicros -> ArduinoPrimitive ()
     DelayMillisE         :: TimeMillisE -> ArduinoPrimitive (Expr ())
     DelayMicrosE         :: TimeMicrosE -> ArduinoPrimitive (Expr ())
     DigitalRead          :: Pin -> ArduinoPrimitive Bool            -- ^ Read the avlue ona pin digitally
     DigitalReadE         :: PinE -> ArduinoPrimitive (Expr Bool)         -- ^ Read the avlue ona pin digitally
     DigitalPortRead      :: Pin -> Word8 -> ArduinoPrimitive Word8          -- ^ Read the values on a port digitally
     DigitalPortReadE     :: PinE -> Expr Word8 -> ArduinoPrimitive (Expr Word8)
     AnalogRead           :: Pin -> ArduinoPrimitive Word16          -- ^ Read the analog value on a pin
     AnalogReadE          :: PinE -> ArduinoPrimitive (Expr Word16)
     I2CRead              :: SlaveAddress -> Word8 -> ArduinoPrimitive [Word8]
     I2CReadE             :: SlaveAddressE -> Expr Word8 -> ArduinoPrimitive (Expr [Word8])
     SerialAvailable      :: Word8 -> ArduinoPrimitive Word8
     SerialAvailableE     :: Expr Word8 -> ArduinoPrimitive (Expr Word8)
     SerialRead           :: Word8 -> ArduinoPrimitive Int32
     SerialReadE          :: Expr Word8 -> ArduinoPrimitive (Expr Int32)
     SerialReadList       :: Word8 -> ArduinoPrimitive [Word8]
     SerialReadListE      :: Expr Word8 -> ArduinoPrimitive (Expr [Word8])
     Stepper2Pin          :: Word16 -> Pin -> Pin -> ArduinoPrimitive Word8
     Stepper2PinE         :: Expr Word16 -> PinE -> PinE -> ArduinoPrimitive (Expr Word8)
     Stepper4Pin          :: Word16 -> Pin -> Pin -> Pin -> Pin -> ArduinoPrimitive Word8
     Stepper4PinE         :: Expr Word16 -> PinE -> PinE -> PinE -> PinE -> ArduinoPrimitive (Expr Word8)
     StepperStepE         :: Expr Word8 -> Expr Int16 -> ArduinoPrimitive ()
     ServoAttach          :: Pin -> ArduinoPrimitive Word8
     ServoAttachE         :: PinE -> ArduinoPrimitive (Expr Word8)
     ServoAttachMinMax    :: Pin -> Int16 -> Int16 -> ArduinoPrimitive Word8
     ServoAttachMinMaxE   :: PinE -> Expr Int16 -> Expr Int16 -> ArduinoPrimitive (Expr Word8)
     ServoRead            :: Word8 -> ArduinoPrimitive Int16
     ServoReadE           :: Expr Word8 -> ArduinoPrimitive (Expr Int16)
     ServoReadMicros      :: Word8 -> ArduinoPrimitive Int16
     ServoReadMicrosE     :: Expr Word8 -> ArduinoPrimitive (Expr Int16)
     QueryAllTasks        :: ArduinoPrimitive [TaskID]
     QueryAllTasksE       :: ArduinoPrimitive (Expr [TaskID])
     QueryTask            :: TaskID -> ArduinoPrimitive (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     QueryTaskE           :: TaskIDE -> ArduinoPrimitive (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     BootTaskE            :: Expr [Word8] -> ArduinoPrimitive (Expr Bool)
     ReadRemoteRefBE      :: RemoteRef Bool   -> ArduinoPrimitive (Expr Bool)
     ReadRemoteRefW8E     :: RemoteRef Word8  -> ArduinoPrimitive (Expr Word8)
     ReadRemoteRefW16E    :: RemoteRef Word16 -> ArduinoPrimitive (Expr Word16)
     ReadRemoteRefW32E    :: RemoteRef Word32 -> ArduinoPrimitive (Expr Word32)
     ReadRemoteRefI8E     :: RemoteRef Int8  -> ArduinoPrimitive (Expr Int8)
     ReadRemoteRefI16E    :: RemoteRef Int16 -> ArduinoPrimitive (Expr Int16)
     ReadRemoteRefI32E    :: RemoteRef Int32 -> ArduinoPrimitive (Expr Int32)
     ReadRemoteRefIE      :: RemoteRef Int   -> ArduinoPrimitive (Expr Int)
     ReadRemoteRefL8E     :: RemoteRef [Word8] -> ArduinoPrimitive (Expr [Word8])
     ReadRemoteRefFloatE  :: RemoteRef Float -> ArduinoPrimitive (Expr Float)
     NewRemoteRefBE       :: Expr Bool   -> ArduinoPrimitive (RemoteRef Bool)
     NewRemoteRefW8E      :: Expr Word8  -> ArduinoPrimitive (RemoteRef Word8)
     NewRemoteRefW16E     :: Expr Word16 -> ArduinoPrimitive (RemoteRef Word16)
     NewRemoteRefW32E     :: Expr Word32 -> ArduinoPrimitive (RemoteRef Word32)
     NewRemoteRefI8E      :: Expr Int8  -> ArduinoPrimitive (RemoteRef Int8)
     NewRemoteRefI16E     :: Expr Int16 -> ArduinoPrimitive (RemoteRef Int16)
     NewRemoteRefI32E     :: Expr Int32 -> ArduinoPrimitive (RemoteRef Int32)
     NewRemoteRefIE       :: Expr Int -> ArduinoPrimitive (RemoteRef Int)
     NewRemoteRefL8E      :: Expr [Word8] -> ArduinoPrimitive (RemoteRef [Word8])
     NewRemoteRefFloatE   :: Expr Float -> ArduinoPrimitive (RemoteRef Float)
     IfThenElseUnitE      :: Expr Bool -> Arduino (Expr ()) -> Arduino (Expr ()) -> ArduinoPrimitive (Expr ())
     IfThenElseBoolE      :: Expr Bool -> Arduino (Expr Bool) -> Arduino (Expr Bool) -> ArduinoPrimitive (Expr Bool)
     IfThenElseWord8E     :: Expr Bool -> Arduino (Expr Word8) -> Arduino (Expr Word8) -> ArduinoPrimitive (Expr Word8)
     IfThenElseWord16E    :: Expr Bool -> Arduino (Expr Word16) -> Arduino (Expr Word16) -> ArduinoPrimitive (Expr Word16)
     IfThenElseWord32E    :: Expr Bool -> Arduino (Expr Word32) -> Arduino (Expr Word32) -> ArduinoPrimitive (Expr Word32)
     IfThenElseInt8E      :: Expr Bool -> Arduino (Expr Int8) -> Arduino (Expr Int8) -> ArduinoPrimitive (Expr Int8)
     IfThenElseInt16E     :: Expr Bool -> Arduino (Expr Int16) -> Arduino (Expr Int16) -> ArduinoPrimitive (Expr Int16)
     IfThenElseInt32E     :: Expr Bool -> Arduino (Expr Int32) -> Arduino (Expr Int32) -> ArduinoPrimitive (Expr Int32)
     IfThenElseIntE       :: Expr Bool -> Arduino (Expr Int) -> Arduino (Expr Int) -> ArduinoPrimitive (Expr Int)
     IfThenElseL8E        :: Expr Bool -> Arduino (Expr [Word8]) -> Arduino (Expr [Word8]) -> ArduinoPrimitive (Expr [Word8])
     IfThenElseFloatE     :: Expr Bool -> Arduino (Expr Float) -> Arduino (Expr Float) -> ArduinoPrimitive (Expr Float)
     -- The following IfThenElse* functions generated by toold/GenEitherTypes.hs
     IfThenElseUnitUnit   :: Expr Bool -> Arduino (ExprEither () ()) -> Arduino (ExprEither () ()) -> ArduinoPrimitive (ExprEither () ())
     IfThenElseUnitBool   :: Expr Bool -> Arduino (ExprEither () Bool) -> Arduino (ExprEither () Bool) -> ArduinoPrimitive (ExprEither () Bool)
     IfThenElseUnitW8     :: Expr Bool -> Arduino (ExprEither () Word8) -> Arduino (ExprEither () Word8) -> ArduinoPrimitive (ExprEither () Word8)
     IfThenElseUnitW16    :: Expr Bool -> Arduino (ExprEither () Word16) -> Arduino (ExprEither () Word16) -> ArduinoPrimitive (ExprEither () Word16)
     IfThenElseUnitW32    :: Expr Bool -> Arduino (ExprEither () Word32) -> Arduino (ExprEither () Word32) -> ArduinoPrimitive (ExprEither () Word32)
     IfThenElseUnitI8     :: Expr Bool -> Arduino (ExprEither () Int8) -> Arduino (ExprEither () Int8) -> ArduinoPrimitive (ExprEither () Int8)
     IfThenElseUnitI16    :: Expr Bool -> Arduino (ExprEither () Int16) -> Arduino (ExprEither () Int16) -> ArduinoPrimitive (ExprEither () Int16)
     IfThenElseUnitI32    :: Expr Bool -> Arduino (ExprEither () Int32) -> Arduino (ExprEither () Int32) -> ArduinoPrimitive (ExprEither () Int32)
     IfThenElseUnitI      :: Expr Bool -> Arduino (ExprEither () Int) -> Arduino (ExprEither () Int) -> ArduinoPrimitive (ExprEither () Int)
     IfThenElseUnitL8     :: Expr Bool -> Arduino (ExprEither () [Word8]) -> Arduino (ExprEither () [Word8]) -> ArduinoPrimitive (ExprEither () [Word8])
     IfThenElseUnitFloat  :: Expr Bool -> Arduino (ExprEither () Float) -> Arduino (ExprEither () Float) -> ArduinoPrimitive (ExprEither () Float)
     IfThenElseBoolUnit   :: Expr Bool -> Arduino (ExprEither Bool ()) -> Arduino (ExprEither Bool ()) -> ArduinoPrimitive (ExprEither Bool ())
     IfThenElseBoolBool   :: Expr Bool -> Arduino (ExprEither Bool Bool) -> Arduino (ExprEither Bool Bool) -> ArduinoPrimitive (ExprEither Bool Bool)
     IfThenElseBoolW8     :: Expr Bool -> Arduino (ExprEither Bool Word8) -> Arduino (ExprEither Bool Word8) -> ArduinoPrimitive (ExprEither Bool Word8)
     IfThenElseBoolW16    :: Expr Bool -> Arduino (ExprEither Bool Word16) -> Arduino (ExprEither Bool Word16) -> ArduinoPrimitive (ExprEither Bool Word16)
     IfThenElseBoolW32    :: Expr Bool -> Arduino (ExprEither Bool Word32) -> Arduino (ExprEither Bool Word32) -> ArduinoPrimitive (ExprEither Bool Word32)
     IfThenElseBoolI8     :: Expr Bool -> Arduino (ExprEither Bool Int8) -> Arduino (ExprEither Bool Int8) -> ArduinoPrimitive (ExprEither Bool Int8)
     IfThenElseBoolI16    :: Expr Bool -> Arduino (ExprEither Bool Int16) -> Arduino (ExprEither Bool Int16) -> ArduinoPrimitive (ExprEither Bool Int16)
     IfThenElseBoolI32    :: Expr Bool -> Arduino (ExprEither Bool Int32) -> Arduino (ExprEither Bool Int32) -> ArduinoPrimitive (ExprEither Bool Int32)
     IfThenElseBoolI      :: Expr Bool -> Arduino (ExprEither Bool Int) -> Arduino (ExprEither Bool Int) -> ArduinoPrimitive (ExprEither Bool Int)
     IfThenElseBoolL8     :: Expr Bool -> Arduino (ExprEither Bool [Word8]) -> Arduino (ExprEither Bool [Word8]) -> ArduinoPrimitive (ExprEither Bool [Word8])
     IfThenElseBoolFloat  :: Expr Bool -> Arduino (ExprEither Bool Float) -> Arduino (ExprEither Bool Float) -> ArduinoPrimitive (ExprEither Bool Float)
     IfThenElseW8Unit     :: Expr Bool -> Arduino (ExprEither Word8 ()) -> Arduino (ExprEither Word8 ()) -> ArduinoPrimitive (ExprEither Word8 ())
     IfThenElseW8Bool     :: Expr Bool -> Arduino (ExprEither Word8 Bool) -> Arduino (ExprEither Word8 Bool) -> ArduinoPrimitive (ExprEither Word8 Bool)
     IfThenElseW8W8       :: Expr Bool -> Arduino (ExprEither Word8 Word8) -> Arduino (ExprEither Word8 Word8) -> ArduinoPrimitive (ExprEither Word8 Word8)
     IfThenElseW8W16      :: Expr Bool -> Arduino (ExprEither Word8 Word16) -> Arduino (ExprEither Word8 Word16) -> ArduinoPrimitive (ExprEither Word8 Word16)
     IfThenElseW8W32      :: Expr Bool -> Arduino (ExprEither Word8 Word32) -> Arduino (ExprEither Word8 Word32) -> ArduinoPrimitive (ExprEither Word8 Word32)
     IfThenElseW8I8       :: Expr Bool -> Arduino (ExprEither Word8 Int8) -> Arduino (ExprEither Word8 Int8) -> ArduinoPrimitive (ExprEither Word8 Int8)
     IfThenElseW8I16      :: Expr Bool -> Arduino (ExprEither Word8 Int16) -> Arduino (ExprEither Word8 Int16) -> ArduinoPrimitive (ExprEither Word8 Int16)
     IfThenElseW8I32      :: Expr Bool -> Arduino (ExprEither Word8 Int32) -> Arduino (ExprEither Word8 Int32) -> ArduinoPrimitive (ExprEither Word8 Int32)
     IfThenElseW8I        :: Expr Bool -> Arduino (ExprEither Word8 Int) -> Arduino (ExprEither Word8 Int) -> ArduinoPrimitive (ExprEither Word8 Int)
     IfThenElseW8L8       :: Expr Bool -> Arduino (ExprEither Word8 [Word8]) -> Arduino (ExprEither Word8 [Word8]) -> ArduinoPrimitive (ExprEither Word8 [Word8])
     IfThenElseW8Float    :: Expr Bool -> Arduino (ExprEither Word8 Float) -> Arduino (ExprEither Word8 Float) -> ArduinoPrimitive (ExprEither Word8 Float)
     IfThenElseW16Unit    :: Expr Bool -> Arduino (ExprEither Word16 ()) -> Arduino (ExprEither Word16 ()) -> ArduinoPrimitive (ExprEither Word16 ())
     IfThenElseW16Bool    :: Expr Bool -> Arduino (ExprEither Word16 Bool) -> Arduino (ExprEither Word16 Bool) -> ArduinoPrimitive (ExprEither Word16 Bool)
     IfThenElseW16W8      :: Expr Bool -> Arduino (ExprEither Word16 Word8) -> Arduino (ExprEither Word16 Word8) -> ArduinoPrimitive (ExprEither Word16 Word8)
     IfThenElseW16W16     :: Expr Bool -> Arduino (ExprEither Word16 Word16) -> Arduino (ExprEither Word16 Word16) -> ArduinoPrimitive (ExprEither Word16 Word16)
     IfThenElseW16W32     :: Expr Bool -> Arduino (ExprEither Word16 Word32) -> Arduino (ExprEither Word16 Word32) -> ArduinoPrimitive (ExprEither Word16 Word32)
     IfThenElseW16I8      :: Expr Bool -> Arduino (ExprEither Word16 Int8) -> Arduino (ExprEither Word16 Int8) -> ArduinoPrimitive (ExprEither Word16 Int8)
     IfThenElseW16I16     :: Expr Bool -> Arduino (ExprEither Word16 Int16) -> Arduino (ExprEither Word16 Int16) -> ArduinoPrimitive (ExprEither Word16 Int16)
     IfThenElseW16I32     :: Expr Bool -> Arduino (ExprEither Word16 Int32) -> Arduino (ExprEither Word16 Int32) -> ArduinoPrimitive (ExprEither Word16 Int32)
     IfThenElseW16I       :: Expr Bool -> Arduino (ExprEither Word16 Int) -> Arduino (ExprEither Word16 Int) -> ArduinoPrimitive (ExprEither Word16 Int)
     IfThenElseW16L8      :: Expr Bool -> Arduino (ExprEither Word16 [Word8]) -> Arduino (ExprEither Word16 [Word8]) -> ArduinoPrimitive (ExprEither Word16 [Word8])
     IfThenElseW16Float   :: Expr Bool -> Arduino (ExprEither Word16 Float) -> Arduino (ExprEither Word16 Float) -> ArduinoPrimitive (ExprEither Word16 Float)
     IfThenElseW32Unit    :: Expr Bool -> Arduino (ExprEither Word32 ()) -> Arduino (ExprEither Word32 ()) -> ArduinoPrimitive (ExprEither Word32 ())
     IfThenElseW32Bool    :: Expr Bool -> Arduino (ExprEither Word32 Bool) -> Arduino (ExprEither Word32 Bool) -> ArduinoPrimitive (ExprEither Word32 Bool)
     IfThenElseW32W8      :: Expr Bool -> Arduino (ExprEither Word32 Word8) -> Arduino (ExprEither Word32 Word8) -> ArduinoPrimitive (ExprEither Word32 Word8)
     IfThenElseW32W16     :: Expr Bool -> Arduino (ExprEither Word32 Word16) -> Arduino (ExprEither Word32 Word16) -> ArduinoPrimitive (ExprEither Word32 Word16)
     IfThenElseW32W32     :: Expr Bool -> Arduino (ExprEither Word32 Word32) -> Arduino (ExprEither Word32 Word32) -> ArduinoPrimitive (ExprEither Word32 Word32)
     IfThenElseW32I8      :: Expr Bool -> Arduino (ExprEither Word32 Int8) -> Arduino (ExprEither Word32 Int8) -> ArduinoPrimitive (ExprEither Word32 Int8)
     IfThenElseW32I16     :: Expr Bool -> Arduino (ExprEither Word32 Int16) -> Arduino (ExprEither Word32 Int16) -> ArduinoPrimitive (ExprEither Word32 Int16)
     IfThenElseW32I32     :: Expr Bool -> Arduino (ExprEither Word32 Int32) -> Arduino (ExprEither Word32 Int32) -> ArduinoPrimitive (ExprEither Word32 Int32)
     IfThenElseW32I       :: Expr Bool -> Arduino (ExprEither Word32 Int) -> Arduino (ExprEither Word32 Int) -> ArduinoPrimitive (ExprEither Word32 Int)
     IfThenElseW32L8      :: Expr Bool -> Arduino (ExprEither Word32 [Word8]) -> Arduino (ExprEither Word32 [Word8]) -> ArduinoPrimitive (ExprEither Word32 [Word8])
     IfThenElseW32Float   :: Expr Bool -> Arduino (ExprEither Word32 Float) -> Arduino (ExprEither Word32 Float) -> ArduinoPrimitive (ExprEither Word32 Float)
     IfThenElseI8Unit     :: Expr Bool -> Arduino (ExprEither Int8 ()) -> Arduino (ExprEither Int8 ()) -> ArduinoPrimitive (ExprEither Int8 ())
     IfThenElseI8Bool     :: Expr Bool -> Arduino (ExprEither Int8 Bool) -> Arduino (ExprEither Int8 Bool) -> ArduinoPrimitive (ExprEither Int8 Bool)
     IfThenElseI8W8       :: Expr Bool -> Arduino (ExprEither Int8 Word8) -> Arduino (ExprEither Int8 Word8) -> ArduinoPrimitive (ExprEither Int8 Word8)
     IfThenElseI8W16      :: Expr Bool -> Arduino (ExprEither Int8 Word16) -> Arduino (ExprEither Int8 Word16) -> ArduinoPrimitive (ExprEither Int8 Word16)
     IfThenElseI8W32      :: Expr Bool -> Arduino (ExprEither Int8 Word32) -> Arduino (ExprEither Int8 Word32) -> ArduinoPrimitive (ExprEither Int8 Word32)
     IfThenElseI8I8       :: Expr Bool -> Arduino (ExprEither Int8 Int8) -> Arduino (ExprEither Int8 Int8) -> ArduinoPrimitive (ExprEither Int8 Int8)
     IfThenElseI8I16      :: Expr Bool -> Arduino (ExprEither Int8 Int16) -> Arduino (ExprEither Int8 Int16) -> ArduinoPrimitive (ExprEither Int8 Int16)
     IfThenElseI8I32      :: Expr Bool -> Arduino (ExprEither Int8 Int32) -> Arduino (ExprEither Int8 Int32) -> ArduinoPrimitive (ExprEither Int8 Int32)
     IfThenElseI8I        :: Expr Bool -> Arduino (ExprEither Int8 Int) -> Arduino (ExprEither Int8 Int) -> ArduinoPrimitive (ExprEither Int8 Int)
     IfThenElseI8L8       :: Expr Bool -> Arduino (ExprEither Int8 [Word8]) -> Arduino (ExprEither Int8 [Word8]) -> ArduinoPrimitive (ExprEither Int8 [Word8])
     IfThenElseI8Float    :: Expr Bool -> Arduino (ExprEither Int8 Float) -> Arduino (ExprEither Int8 Float) -> ArduinoPrimitive (ExprEither Int8 Float)
     IfThenElseI16Unit    :: Expr Bool -> Arduino (ExprEither Int16 ()) -> Arduino (ExprEither Int16 ()) -> ArduinoPrimitive (ExprEither Int16 ())
     IfThenElseI16Bool    :: Expr Bool -> Arduino (ExprEither Int16 Bool) -> Arduino (ExprEither Int16 Bool) -> ArduinoPrimitive (ExprEither Int16 Bool)
     IfThenElseI16W8      :: Expr Bool -> Arduino (ExprEither Int16 Word8) -> Arduino (ExprEither Int16 Word8) -> ArduinoPrimitive (ExprEither Int16 Word8)
     IfThenElseI16W16     :: Expr Bool -> Arduino (ExprEither Int16 Word16) -> Arduino (ExprEither Int16 Word16) -> ArduinoPrimitive (ExprEither Int16 Word16)
     IfThenElseI16W32     :: Expr Bool -> Arduino (ExprEither Int16 Word32) -> Arduino (ExprEither Int16 Word32) -> ArduinoPrimitive (ExprEither Int16 Word32)
     IfThenElseI16I8      :: Expr Bool -> Arduino (ExprEither Int16 Int8) -> Arduino (ExprEither Int16 Int8) -> ArduinoPrimitive (ExprEither Int16 Int8)
     IfThenElseI16I16     :: Expr Bool -> Arduino (ExprEither Int16 Int16) -> Arduino (ExprEither Int16 Int16) -> ArduinoPrimitive (ExprEither Int16 Int16)
     IfThenElseI16I32     :: Expr Bool -> Arduino (ExprEither Int16 Int32) -> Arduino (ExprEither Int16 Int32) -> ArduinoPrimitive (ExprEither Int16 Int32)
     IfThenElseI16I       :: Expr Bool -> Arduino (ExprEither Int16 Int) -> Arduino (ExprEither Int16 Int) -> ArduinoPrimitive (ExprEither Int16 Int)
     IfThenElseI16L8      :: Expr Bool -> Arduino (ExprEither Int16 [Word8]) -> Arduino (ExprEither Int16 [Word8]) -> ArduinoPrimitive (ExprEither Int16 [Word8])
     IfThenElseI16Float   :: Expr Bool -> Arduino (ExprEither Int16 Float) -> Arduino (ExprEither Int16 Float) -> ArduinoPrimitive (ExprEither Int16 Float)
     IfThenElseI32Unit    :: Expr Bool -> Arduino (ExprEither Int32 ()) -> Arduino (ExprEither Int32 ()) -> ArduinoPrimitive (ExprEither Int32 ())
     IfThenElseI32Bool    :: Expr Bool -> Arduino (ExprEither Int32 Bool) -> Arduino (ExprEither Int32 Bool) -> ArduinoPrimitive (ExprEither Int32 Bool)
     IfThenElseI32W8      :: Expr Bool -> Arduino (ExprEither Int32 Word8) -> Arduino (ExprEither Int32 Word8) -> ArduinoPrimitive (ExprEither Int32 Word8)
     IfThenElseI32W16     :: Expr Bool -> Arduino (ExprEither Int32 Word16) -> Arduino (ExprEither Int32 Word16) -> ArduinoPrimitive (ExprEither Int32 Word16)
     IfThenElseI32W32     :: Expr Bool -> Arduino (ExprEither Int32 Word32) -> Arduino (ExprEither Int32 Word32) -> ArduinoPrimitive (ExprEither Int32 Word32)
     IfThenElseI32I8      :: Expr Bool -> Arduino (ExprEither Int32 Int8) -> Arduino (ExprEither Int32 Int8) -> ArduinoPrimitive (ExprEither Int32 Int8)
     IfThenElseI32I16     :: Expr Bool -> Arduino (ExprEither Int32 Int16) -> Arduino (ExprEither Int32 Int16) -> ArduinoPrimitive (ExprEither Int32 Int16)
     IfThenElseI32I32     :: Expr Bool -> Arduino (ExprEither Int32 Int32) -> Arduino (ExprEither Int32 Int32) -> ArduinoPrimitive (ExprEither Int32 Int32)
     IfThenElseI32I       :: Expr Bool -> Arduino (ExprEither Int32 Int) -> Arduino (ExprEither Int32 Int) -> ArduinoPrimitive (ExprEither Int32 Int)
     IfThenElseI32L8      :: Expr Bool -> Arduino (ExprEither Int32 [Word8]) -> Arduino (ExprEither Int32 [Word8]) -> ArduinoPrimitive (ExprEither Int32 [Word8])
     IfThenElseI32Float   :: Expr Bool -> Arduino (ExprEither Int32 Float) -> Arduino (ExprEither Int32 Float) -> ArduinoPrimitive (ExprEither Int32 Float)
     IfThenElseIUnit      :: Expr Bool -> Arduino (ExprEither Int ()) -> Arduino (ExprEither Int ()) -> ArduinoPrimitive (ExprEither Int ())
     IfThenElseIBool      :: Expr Bool -> Arduino (ExprEither Int Bool) -> Arduino (ExprEither Int Bool) -> ArduinoPrimitive (ExprEither Int Bool)
     IfThenElseIW8        :: Expr Bool -> Arduino (ExprEither Int Word8) -> Arduino (ExprEither Int Word8) -> ArduinoPrimitive (ExprEither Int Word8)
     IfThenElseIW16       :: Expr Bool -> Arduino (ExprEither Int Word16) -> Arduino (ExprEither Int Word16) -> ArduinoPrimitive (ExprEither Int Word16)
     IfThenElseIW32       :: Expr Bool -> Arduino (ExprEither Int Word32) -> Arduino (ExprEither Int Word32) -> ArduinoPrimitive (ExprEither Int Word32)
     IfThenElseII8        :: Expr Bool -> Arduino (ExprEither Int Int8) -> Arduino (ExprEither Int Int8) -> ArduinoPrimitive (ExprEither Int Int8)
     IfThenElseII16       :: Expr Bool -> Arduino (ExprEither Int Int16) -> Arduino (ExprEither Int Int16) -> ArduinoPrimitive (ExprEither Int Int16)
     IfThenElseII32       :: Expr Bool -> Arduino (ExprEither Int Int32) -> Arduino (ExprEither Int Int32) -> ArduinoPrimitive (ExprEither Int Int32)
     IfThenElseII         :: Expr Bool -> Arduino (ExprEither Int Int) -> Arduino (ExprEither Int Int) -> ArduinoPrimitive (ExprEither Int Int)
     IfThenElseIL8        :: Expr Bool -> Arduino (ExprEither Int [Word8]) -> Arduino (ExprEither Int [Word8]) -> ArduinoPrimitive (ExprEither Int [Word8])
     IfThenElseIFloat     :: Expr Bool -> Arduino (ExprEither Int Float) -> Arduino (ExprEither Int Float) -> ArduinoPrimitive (ExprEither Int Float)
     IfThenElseL8Unit     :: Expr Bool -> Arduino (ExprEither [Word8] ()) -> Arduino (ExprEither [Word8] ()) -> ArduinoPrimitive (ExprEither [Word8] ())
     IfThenElseL8Bool     :: Expr Bool -> Arduino (ExprEither [Word8] Bool) -> Arduino (ExprEither [Word8] Bool) -> ArduinoPrimitive (ExprEither [Word8] Bool)
     IfThenElseL8W8       :: Expr Bool -> Arduino (ExprEither [Word8] Word8) -> Arduino (ExprEither [Word8] Word8) -> ArduinoPrimitive (ExprEither [Word8] Word8)
     IfThenElseL8W16      :: Expr Bool -> Arduino (ExprEither [Word8] Word16) -> Arduino (ExprEither [Word8] Word16) -> ArduinoPrimitive (ExprEither [Word8] Word16)
     IfThenElseL8W32      :: Expr Bool -> Arduino (ExprEither [Word8] Word32) -> Arduino (ExprEither [Word8] Word32) -> ArduinoPrimitive (ExprEither [Word8] Word32)
     IfThenElseL8I8       :: Expr Bool -> Arduino (ExprEither [Word8] Int8) -> Arduino (ExprEither [Word8] Int8) -> ArduinoPrimitive (ExprEither [Word8] Int8)
     IfThenElseL8I16      :: Expr Bool -> Arduino (ExprEither [Word8] Int16) -> Arduino (ExprEither [Word8] Int16) -> ArduinoPrimitive (ExprEither [Word8] Int16)
     IfThenElseL8I32      :: Expr Bool -> Arduino (ExprEither [Word8] Int32) -> Arduino (ExprEither [Word8] Int32) -> ArduinoPrimitive (ExprEither [Word8] Int32)
     IfThenElseL8I        :: Expr Bool -> Arduino (ExprEither [Word8] Int) -> Arduino (ExprEither [Word8] Int) -> ArduinoPrimitive (ExprEither [Word8] Int)
     IfThenElseL8L8       :: Expr Bool -> Arduino (ExprEither [Word8] [Word8]) -> Arduino (ExprEither [Word8] [Word8]) -> ArduinoPrimitive (ExprEither [Word8] [Word8])
     IfThenElseL8Float    :: Expr Bool -> Arduino (ExprEither [Word8] Float) -> Arduino (ExprEither [Word8] Float) -> ArduinoPrimitive (ExprEither [Word8] Float)
     IfThenElseFloatUnit  :: Expr Bool -> Arduino (ExprEither Float ()) -> Arduino (ExprEither Float ()) -> ArduinoPrimitive (ExprEither Float ())
     IfThenElseFloatBool  :: Expr Bool -> Arduino (ExprEither Float Bool) -> Arduino (ExprEither Float Bool) -> ArduinoPrimitive (ExprEither Float Bool)
     IfThenElseFloatW8    :: Expr Bool -> Arduino (ExprEither Float Word8) -> Arduino (ExprEither Float Word8) -> ArduinoPrimitive (ExprEither Float Word8)
     IfThenElseFloatW16   :: Expr Bool -> Arduino (ExprEither Float Word16) -> Arduino (ExprEither Float Word16) -> ArduinoPrimitive (ExprEither Float Word16)
     IfThenElseFloatW32   :: Expr Bool -> Arduino (ExprEither Float Word32) -> Arduino (ExprEither Float Word32) -> ArduinoPrimitive (ExprEither Float Word32)
     IfThenElseFloatI8    :: Expr Bool -> Arduino (ExprEither Float Int8) -> Arduino (ExprEither Float Int8) -> ArduinoPrimitive (ExprEither Float Int8)
     IfThenElseFloatI16   :: Expr Bool -> Arduino (ExprEither Float Int16) -> Arduino (ExprEither Float Int16) -> ArduinoPrimitive (ExprEither Float Int16)
     IfThenElseFloatI32   :: Expr Bool -> Arduino (ExprEither Float Int32) -> Arduino (ExprEither Float Int32) -> ArduinoPrimitive (ExprEither Float Int32)
     IfThenElseFloatI     :: Expr Bool -> Arduino (ExprEither Float Int) -> Arduino (ExprEither Float Int) -> ArduinoPrimitive (ExprEither Float Int)
     IfThenElseFloatL8    :: Expr Bool -> Arduino (ExprEither Float [Word8]) -> Arduino (ExprEither Float [Word8]) -> ArduinoPrimitive (ExprEither Float [Word8])
     IfThenElseFloatFloat :: Expr Bool -> Arduino (ExprEither Float Float) -> Arduino (ExprEither Float Float) -> ArduinoPrimitive (ExprEither Float Float)
     -- The following Iterate*E functions generated by toold/GenEitherTypes.hs
     IterateUnitUnitE     :: Expr () -> (Expr () -> Arduino (ExprEither () ())) -> ArduinoPrimitive (Expr ())
     IterateUnitBoolE     :: Expr () -> (Expr () -> Arduino (ExprEither () Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateUnitW8E       :: Expr () -> (Expr () -> Arduino (ExprEither () Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateUnitW16E      :: Expr () -> (Expr () -> Arduino (ExprEither () Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateUnitW32E      :: Expr () -> (Expr () -> Arduino (ExprEither () Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateUnitI8E       :: Expr () -> (Expr () -> Arduino (ExprEither () Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateUnitI16E      :: Expr () -> (Expr () -> Arduino (ExprEither () Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateUnitI32E      :: Expr () -> (Expr () -> Arduino (ExprEither () Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateUnitIE        :: Expr () -> (Expr () -> Arduino (ExprEither () Int)) -> ArduinoPrimitive (Expr Int)
     IterateUnitL8E       :: Expr () -> (Expr () -> Arduino (ExprEither () [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateUnitFloatE    :: Expr () -> (Expr () -> Arduino (ExprEither () Float)) -> ArduinoPrimitive (Expr Float)
     IterateBoolUnitE     :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool ())) -> ArduinoPrimitive (Expr ())
     IterateBoolBoolE     :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateBoolW8E       :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateBoolW16E      :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateBoolW32E      :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateBoolI8E       :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateBoolI16E      :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateBoolI32E      :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateBoolIE        :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Int)) -> ArduinoPrimitive (Expr Int)
     IterateBoolL8E       :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateBoolFloatE    :: Expr Bool -> (Expr Bool -> Arduino (ExprEither Bool Float)) -> ArduinoPrimitive (Expr Float)
     IterateW8UnitE       :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 ())) -> ArduinoPrimitive (Expr ())
     IterateW8BoolE       :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateW8W8E         :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateW8W16E        :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateW8W32E        :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateW8I8E         :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateW8I16E        :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateW8I32E        :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateW8IE          :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Int)) -> ArduinoPrimitive (Expr Int)
     IterateW8L8E         :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateW8FloatE      :: Expr Word8 -> (Expr Word8 -> Arduino (ExprEither Word8 Float)) -> ArduinoPrimitive (Expr Float)
     IterateW16UnitE      :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 ())) -> ArduinoPrimitive (Expr ())
     IterateW16BoolE      :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateW16W8E        :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateW16W16E       :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateW16W32E       :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateW16I8E        :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateW16I16E       :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateW16I32E       :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateW16IE         :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Int)) -> ArduinoPrimitive (Expr Int)
     IterateW16L8E        :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateW16FloatE     :: Expr Word16 -> (Expr Word16 -> Arduino (ExprEither Word16 Float)) -> ArduinoPrimitive (Expr Float)
     IterateW32UnitE      :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 ())) -> ArduinoPrimitive (Expr ())
     IterateW32BoolE      :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateW32W8E        :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateW32W16E       :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateW32W32E       :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateW32I8E        :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateW32I16E       :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateW32I32E       :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateW32IE         :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Int)) -> ArduinoPrimitive (Expr Int)
     IterateW32L8E        :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateW32FloatE     :: Expr Word32 -> (Expr Word32 -> Arduino (ExprEither Word32 Float)) -> ArduinoPrimitive (Expr Float)
     IterateI8UnitE       :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 ())) -> ArduinoPrimitive (Expr ())
     IterateI8BoolE       :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateI8W8E         :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateI8W16E        :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateI8W32E        :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateI8I8E         :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateI8I16E        :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateI8I32E        :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateI8IE          :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Int)) -> ArduinoPrimitive (Expr Int)
     IterateI8L8E         :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateI8FloatE      :: Expr Int8 -> (Expr Int8 -> Arduino (ExprEither Int8 Float)) -> ArduinoPrimitive (Expr Float)
     IterateI16UnitE      :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 ())) -> ArduinoPrimitive (Expr ())
     IterateI16BoolE      :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateI16W8E        :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateI16W16E       :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateI16W32E       :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateI16I8E        :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateI16I16E       :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateI16I32E       :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateI16IE         :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Int)) -> ArduinoPrimitive (Expr Int)
     IterateI16L8E        :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateI16FloatE     :: Expr Int16 -> (Expr Int16 -> Arduino (ExprEither Int16 Float)) -> ArduinoPrimitive (Expr Float)
     IterateI32UnitE      :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 ())) -> ArduinoPrimitive (Expr ())
     IterateI32BoolE      :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateI32W8E        :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateI32W16E       :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateI32W32E       :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateI32I8E        :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateI32I16E       :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateI32I32E       :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateI32IE         :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Int)) -> ArduinoPrimitive (Expr Int)
     IterateI32L8E        :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateI32FloatE     :: Expr Int32 -> (Expr Int32 -> Arduino (ExprEither Int32 Float)) -> ArduinoPrimitive (Expr Float)
     IterateIUnitE        :: Expr Int -> (Expr Int -> Arduino (ExprEither Int ())) -> ArduinoPrimitive (Expr ())
     IterateIBoolE        :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateIW8E          :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateIW16E         :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateIW32E         :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateII8E          :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateII16E         :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateII32E         :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateIIE           :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Int)) -> ArduinoPrimitive (Expr Int)
     IterateIL8E          :: Expr Int -> (Expr Int -> Arduino (ExprEither Int [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateIFloatE       :: Expr Int -> (Expr Int -> Arduino (ExprEither Int Float)) -> ArduinoPrimitive (Expr Float)
     IterateL8UnitE       :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] ())) -> ArduinoPrimitive (Expr ())
     IterateL8BoolE       :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateL8W8E         :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateL8W16E        :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateL8W32E        :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateL8I8E         :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateL8I16E        :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateL8I32E        :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateL8IE          :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Int)) -> ArduinoPrimitive (Expr Int)
     IterateL8L8E         :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateL8FloatE      :: Expr [Word8] -> (Expr [Word8] -> Arduino (ExprEither [Word8] Float)) -> ArduinoPrimitive (Expr Float)
     IterateFloatUnitE    :: Expr Float -> (Expr Float -> Arduino (ExprEither Float ())) -> ArduinoPrimitive (Expr ())
     IterateFloatBoolE    :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Bool)) -> ArduinoPrimitive (Expr Bool)
     IterateFloatW8E      :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Word8)) -> ArduinoPrimitive (Expr Word8)
     IterateFloatW16E     :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Word16)) -> ArduinoPrimitive (Expr Word16)
     IterateFloatW32E     :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Word32)) -> ArduinoPrimitive (Expr Word32)
     IterateFloatI8E      :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Int8)) -> ArduinoPrimitive (Expr Int8)
     IterateFloatI16E     :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Int16)) -> ArduinoPrimitive (Expr Int16)
     IterateFloatI32E     :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Int32)) -> ArduinoPrimitive (Expr Int32)
     IterateFloatIE       :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Int)) -> ArduinoPrimitive (Expr Int)
     IterateFloatL8E      :: Expr Float -> (Expr Float -> Arduino (ExprEither Float [Word8])) -> ArduinoPrimitive (Expr [Word8])
     IterateFloatFloatE   :: Expr Float -> (Expr Float -> Arduino (ExprEither Float Float)) -> ArduinoPrimitive (Expr Float)
     LiftIO               :: IO a -> ArduinoPrimitive a
     LamExprWord8Unit     :: Expr Word8 -> Arduino (Expr ()) -> ArduinoPrimitive (Expr ())
     AppExprWord8Unit     :: Arduino (Expr ()) -> Expr Word8 -> ArduinoPrimitive (Expr ())
     AppLambdaUnit        :: String -> Arduino (Expr ()) -> ArduinoPrimitive (Expr ())
     Debug                :: [Word8] -> ArduinoPrimitive ()
     DebugE               :: Expr [Word8] -> ArduinoPrimitive ()
     DebugListen          :: ArduinoPrimitive ()
     Die                  :: String -> [String] -> ArduinoPrimitive ()
     -- ToDo: add SPI procedures

instance KnownResult ArduinoPrimitive where
  knownResult (SystemResetE {}         ) = Just LitUnit
  knownResult (SetPinModeE {}          ) = Just LitUnit
  knownResult (DigitalPortWriteE {}    ) = Just LitUnit
  knownResult (DigitalWriteE {}        ) = Just LitUnit
  knownResult (AnalogWriteE {}         ) = Just LitUnit
  knownResult (ToneE {}                ) = Just LitUnit
  knownResult (NoToneE {}              ) = Just LitUnit
  knownResult (SerialBeginE {}         ) = Just LitUnit
  knownResult (SerialEndE {}           ) = Just LitUnit
  knownResult (SerialWriteE {}         ) = Just LitUnit
  knownResult (SerialWriteListE {}     ) = Just LitUnit
  knownResult (StepperSetSpeedE {}     ) = Just LitUnit
  knownResult (ServoDetachE {}         ) = Just LitUnit
  knownResult (ServoWriteE {}          ) = Just LitUnit
  knownResult (ServoWriteMicrosE {}    ) = Just LitUnit
  knownResult (CreateTask {}           ) = Just ()
  knownResult (CreateTaskE {}          ) = Just LitUnit
  knownResult (DeleteTask {}           ) = Just ()
  knownResult (DeleteTaskE {}          ) = Just LitUnit
  knownResult (ScheduleTask  {}        ) = Just ()
  knownResult (ScheduleTaskE {}        ) = Just LitUnit
  knownResult (ScheduleReset {}        ) = Just ()
  knownResult (ScheduleResetE {}       ) = Just LitUnit
  knownResult (AttachInt {}            ) = Just ()
  knownResult (AttachIntE {}           ) = Just LitUnit
  knownResult (DetachInt {}            ) = Just ()
  knownResult (DetachIntE {}           ) = Just LitUnit
  knownResult (Interrupts {}           ) = Just ()
  knownResult (InterruptsE {}          ) = Just LitUnit
  knownResult (NoInterruptsE {}        ) = Just LitUnit
  knownResult (GiveSem {}              ) = Just ()
  knownResult (GiveSemE {}             ) = Just LitUnit
  knownResult (TakeSem {}              ) = Just ()
  knownResult (TakeSemE {}             ) = Just LitUnit
  knownResult (WriteRemoteRefBE {}     ) = Just LitUnit
  knownResult (WriteRemoteRefW8E {}    ) = Just LitUnit
  knownResult (WriteRemoteRefW16E {}   ) = Just LitUnit
  knownResult (WriteRemoteRefW32E  {}  ) = Just LitUnit
  knownResult (WriteRemoteRefI8E {}    ) = Just LitUnit
  knownResult (WriteRemoteRefI16E {}   ) = Just LitUnit
  knownResult (WriteRemoteRefI32E {}   ) = Just LitUnit
  knownResult (WriteRemoteRefIE {}     ) = Just LitUnit
  knownResult (WriteRemoteRefL8E {}    ) = Just LitUnit
  knownResult (WriteRemoteRefFloatE {} ) = Just LitUnit
  knownResult (ModifyRemoteRefBE {}    ) = Just LitUnit
  knownResult (ModifyRemoteRefW8E {}   ) = Just LitUnit
  knownResult (ModifyRemoteRefW16E {}  ) = Just LitUnit
  knownResult (ModifyRemoteRefW32E {}  ) = Just LitUnit
  knownResult (ModifyRemoteRefI8E {}   ) = Just LitUnit
  knownResult (ModifyRemoteRefI16E {}  ) = Just LitUnit
  knownResult (ModifyRemoteRefI32E {}  ) = Just LitUnit
  knownResult (ModifyRemoteRefIE {}    ) = Just LitUnit
  knownResult (ModifyRemoteRefL8E {}   ) = Just LitUnit
  knownResult (ModifyRemoteRefFloatE {} ) = Just LitUnit
  knownResult (Loop {} )                 = Just ()
  knownResult _                          = Nothing

systemReset :: Arduino ()
systemReset = evalExprUnit <$> (Arduino $ primitive SystemResetE)

systemResetE :: Arduino (Expr ())
systemResetE =  Arduino $ primitive SystemResetE

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm = evalExprUnit <$> (Arduino $ primitive $ SetPinModeE (lit p) (lit $ fromIntegral $ fromEnum pm))

setPinModeE :: PinE -> PinMode -> Arduino (Expr ())
setPinModeE p pm =  Arduino $ primitive $ SetPinModeE p (lit $ fromIntegral $ fromEnum pm)

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p b = evalExprUnit <$> (Arduino $ primitive $ DigitalWriteE (lit p) (lit b))

digitalWriteE :: PinE -> Expr Bool -> Arduino (Expr ())
digitalWriteE p b = Arduino $ primitive $ DigitalWriteE p b

digitalPortWrite :: Pin -> Word8 -> Word8 -> Arduino ()
digitalPortWrite p b m = evalExprUnit <$> (Arduino $ primitive $ DigitalPortWriteE (lit p) (lit b) (lit m))

digitalPortWriteE :: PinE -> Expr Word8 -> Expr Word8 -> Arduino (Expr ())
digitalPortWriteE p b m = Arduino $ primitive $ DigitalPortWriteE p b m

analogWrite :: Pin -> Word16 -> Arduino ()
analogWrite p w =  evalExprUnit <$> (Arduino $ primitive $ AnalogWriteE (lit p) (lit w))

analogWriteE :: PinE -> Expr Word16 -> Arduino (Expr ())
analogWriteE p w = Arduino $ primitive $ AnalogWriteE p w

tone :: Pin -> Word16 -> Maybe Word32 -> Arduino ()
tone p f Nothing =  evalExprUnit <$> (Arduino $ primitive $ ToneE (lit p) (lit f) Nothing)
tone p f (Just d) =  evalExprUnit <$> (Arduino $ primitive $ ToneE (lit p) (lit f) (Just $ lit d))

toneE :: PinE -> Expr Word16 -> Maybe (Expr Word32) -> Arduino (Expr ())
toneE p f d = Arduino $ primitive $ ToneE p f d

noTone :: Pin -> Arduino ()
noTone p = evalExprUnit <$> (Arduino $ primitive $ NoToneE (lit p))

noToneE :: PinE -> Arduino (Expr ())
noToneE p = Arduino $ primitive $ NoToneE p

i2cWrite :: SlaveAddress -> [Word8] -> Arduino ()
i2cWrite sa ws = evalExprUnit <$> (Arduino $ primitive $ I2CWriteE (lit sa) (lit ws))

i2cWriteE :: SlaveAddressE -> Expr [Word8] -> Arduino (Expr ())
i2cWriteE sa ws = Arduino $ primitive $ I2CWriteE sa ws

i2cConfig :: Arduino ()
i2cConfig = evalExprUnit <$> (Arduino $ primitive $ I2CConfigE)

i2cConfigE :: Arduino (Expr ())
i2cConfigE =  Arduino $ primitive $ I2CConfigE

serialBegin :: Word8 -> Word32 -> Arduino ()
serialBegin p r = evalExprUnit <$> (Arduino $ primitive $ SerialBeginE (lit p) (lit r))

serialBeginE :: Expr Word8 -> Expr Word32 -> Arduino (Expr ())
serialBeginE p r = Arduino $ primitive $ SerialBeginE p r

serialEnd :: Word8 -> Arduino ()
serialEnd p = evalExprUnit <$> (Arduino $ primitive $ SerialEndE (lit p))

serialEndE :: Expr Word8 -> Arduino (Expr ())
serialEndE p = Arduino $ primitive $ SerialEndE p

serialWrite :: Word8 -> Word8 -> Arduino ()
serialWrite p w = evalExprUnit <$> (Arduino $ primitive $ SerialWriteE (lit p) (lit w))

serialWriteE :: Expr Word8 -> Expr Word8 -> Arduino (Expr ())
serialWriteE p w = Arduino $ primitive $ SerialWriteE p w

serialWriteList :: Word8 -> [Word8] -> Arduino ()
serialWriteList p l = evalExprUnit <$> (Arduino $ primitive $ SerialWriteListE (lit p) (lit l))

serialWriteListE :: Expr Word8 -> Expr [Word8] -> Arduino (Expr ())
serialWriteListE p l = Arduino $ primitive $ SerialWriteListE p l

stepperSetSpeed :: Word8 -> Int32 -> Arduino ()
stepperSetSpeed st sp = evalExprUnit <$> (Arduino $ primitive $ StepperSetSpeedE (lit st) (lit sp))

stepperSetSpeedE :: Expr Word8 -> Expr Int32 -> Arduino (Expr ())
stepperSetSpeedE st sp = Arduino $ primitive $ StepperSetSpeedE st sp

servoDetach :: Word8 -> Arduino ()
servoDetach s = evalExprUnit <$> (Arduino $ primitive $ ServoDetachE (lit s))

servoDetachE :: Expr Word8 -> Arduino (Expr ())
servoDetachE s = Arduino $ primitive $ ServoDetachE s

servoWrite :: Word8 -> Int16 -> Arduino ()
servoWrite s w = evalExprUnit <$> (Arduino $ primitive $ ServoWriteE (lit s) (lit w))

servoWriteE :: Expr Word8 -> Expr Int16 -> Arduino (Expr ())
servoWriteE s w = Arduino $ primitive $ ServoWriteE s w

servoWriteMicros :: Word8 -> Int16 -> Arduino ()
servoWriteMicros s w = evalExprUnit <$> (Arduino $ primitive $ ServoWriteMicrosE (lit s) (lit w))

servoWriteMicrosE :: Expr Word8 -> Expr Int16 -> Arduino (Expr ())
servoWriteMicrosE s w = Arduino $ primitive $ ServoWriteMicrosE s w

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Arduino $ primitive $ CreateTask tid ps

createTaskE :: TaskIDE -> Arduino (Expr ()) -> Arduino (Expr ())
createTaskE tid ps = Arduino $ primitive  $ CreateTaskE tid ps

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Arduino $ primitive $ DeleteTask tid

deleteTaskE :: TaskIDE -> Arduino (Expr ())
deleteTaskE tid = Arduino $ primitive $ DeleteTaskE tid

scheduleTask :: TaskID -> TimeMillis -> Arduino ()
scheduleTask tid tt = Arduino $ primitive $ ScheduleTask tid tt

scheduleTaskE :: TaskIDE -> TimeMillisE -> Arduino (Expr ())
scheduleTaskE tid tt = Arduino $ primitive $ ScheduleTaskE tid tt

attachInt :: Pin -> TaskID -> IntMode -> Arduino ()
attachInt p tid m = Arduino $ primitive $ AttachInt p tid (fromIntegral $ fromEnum m)

attachIntE :: PinE -> TaskIDE -> IntMode -> Arduino (Expr ())
attachIntE p tid m = Arduino $ primitive $ AttachIntE p tid (lit $ fromIntegral $ fromEnum m)

detachInt :: Pin -> Arduino ()
detachInt p = Arduino $ primitive $ DetachInt p

detachIntE :: PinE -> Arduino (Expr ())
detachIntE p = Arduino $ primitive $ DetachIntE p

interrupts :: Arduino ()
interrupts = Arduino $ primitive $ Interrupts

interruptsE :: Arduino (Expr ())
interruptsE = Arduino $ primitive $ InterruptsE

noInterrupts :: Arduino ()
noInterrupts = Arduino $ primitive $ NoInterrupts

noInterruptsE :: Arduino (Expr ())
noInterruptsE =  Arduino $ primitive $ NoInterruptsE

scheduleReset :: Arduino ()
scheduleReset = Arduino $ primitive ScheduleReset

scheduleResetE :: Arduino (Expr ())
scheduleResetE = Arduino $ primitive ScheduleResetE

giveSem :: Word8 -> Arduino ()
giveSem i = Arduino $ primitive $ GiveSem i

giveSemE :: Expr Word8 -> Arduino (Expr ())
giveSemE i = Arduino $ primitive $ GiveSemE i

takeSem :: Word8 -> Arduino ()
takeSem i = Arduino $ primitive $ TakeSem i

takeSemE :: Expr Word8 -> Arduino (Expr ())
takeSemE i = Arduino $ primitive $ TakeSemE i

class ExprB a => RemoteReference a where
    newRemoteRef     :: a -> Arduino (RemoteRef a)
    newRemoteRef v = newRemoteRefE $ lit v
    newRemoteRefE    :: Expr a -> Arduino (RemoteRef a)
    readRemoteRef    :: RemoteRef a -> Arduino a
    readRemoteRef r = eval' <$> (readRemoteRefE r)
    readRemoteRefE   :: RemoteRef a -> Arduino (Expr a)
    writeRemoteRef   :: RemoteRef a -> a -> Arduino ()
    writeRemoteRef r v = eval' <$> (writeRemoteRefE r $ lit v)
    writeRemoteRefE  :: RemoteRef a -> Expr a -> Arduino (Expr ())
    modifyRemoteRef  :: RemoteRef a -> (a -> a) -> Arduino ()
    modifyRemoteRef r f = do
        v <- readRemoteRef r
        writeRemoteRef r (f v)
    modifyRemoteRefE :: RemoteRef a -> (Expr a -> Expr a) ->
                             Arduino (Expr ())

instance RemoteReference Bool where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefBE n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefBE n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefBE r e
    modifyRemoteRefE (RemoteRefB i) f =
        Arduino $ primitive $ ModifyRemoteRefBE (RemoteRefB i) (f $ RefB i)

instance RemoteReference Word8 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefW8E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefW8E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefW8E r e
    modifyRemoteRefE (RemoteRefW8 i) f =
        Arduino $ primitive $ ModifyRemoteRefW8E (RemoteRefW8 i) (f $ RefW8 i)

instance RemoteReference Word16 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefW16E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefW16E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefW16E r e
    modifyRemoteRefE (RemoteRefW16 i) f =
        Arduino $ primitive $ ModifyRemoteRefW16E (RemoteRefW16 i) (f $ RefW16 i)

instance RemoteReference Word32 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefW32E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefW32E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefW32E r e
    modifyRemoteRefE (RemoteRefW32 i) f =
        Arduino $ primitive $ ModifyRemoteRefW32E (RemoteRefW32 i) (f $ RefW32 i)

instance RemoteReference Int8 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefI8E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefI8E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefI8E r e
    modifyRemoteRefE (RemoteRefI8 i) f =
        Arduino $ primitive $ ModifyRemoteRefI8E (RemoteRefI8 i) (f $ RefI8 i)

instance RemoteReference Int16 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefI16E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefI16E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefI16E r e
    modifyRemoteRefE (RemoteRefI16 i) f =
        Arduino $ primitive $ ModifyRemoteRefI16E (RemoteRefI16 i) (f $ RefI16 i)

instance RemoteReference Int32 where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefI32E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefI32E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefI32E r e
    modifyRemoteRefE (RemoteRefI32 i) f =
        Arduino $ primitive $ ModifyRemoteRefI32E (RemoteRefI32 i) (f $ RefI32 i)

instance RemoteReference Int where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefIE n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefIE n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefIE r e
    modifyRemoteRefE (RemoteRefI i) f =
        Arduino $ primitive $ ModifyRemoteRefIE (RemoteRefI i) (f $ RefI i)

instance RemoteReference [Word8] where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefL8E n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefL8E n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefL8E r e
    modifyRemoteRefE (RemoteRefL8 i) f =
        Arduino $ primitive $ ModifyRemoteRefL8E (RemoteRefL8 i) (f $ RefList8 i)

instance RemoteReference Float where
    newRemoteRefE n      = Arduino $ primitive $ NewRemoteRefFloatE n
    readRemoteRefE n     = Arduino $ primitive $ ReadRemoteRefFloatE n
    writeRemoteRefE r e  = Arduino $ primitive $ WriteRemoteRefFloatE r e
    modifyRemoteRefE (RemoteRefFloat i) f =
        Arduino $ primitive $ ModifyRemoteRefFloatE (RemoteRefFloat i) (f $ RefFloat i)

loop :: Arduino () -> Arduino ()
loop m = Arduino $ primitive $ Loop m

queryFirmware :: Arduino Word16
queryFirmware = Arduino $ primitive QueryFirmware

queryFirmwareE :: Arduino (Expr Word16)
queryFirmwareE = Arduino $ primitive QueryFirmwareE

queryProcessor :: Arduino Processor
queryProcessor = Arduino $ primitive QueryProcessor

queryProcessorE :: Arduino (Expr Word8)
queryProcessorE = Arduino $ primitive QueryProcessorE

micros :: Arduino Word32
micros = Arduino $ primitive Micros

microsE :: Arduino (Expr Word32)
microsE = Arduino $ primitive MicrosE

millis :: Arduino Word32
millis = Arduino $ primitive Millis

millisE :: Arduino (Expr Word32)
millisE = Arduino $ primitive MillisE

delayMillis :: TimeMillis -> Arduino ()
delayMillis t = Arduino $ primitive $ DelayMillis t

delayMillisE :: TimeMillisE -> Arduino (Expr ())
delayMillisE t = Arduino $ primitive $ DelayMillisE t

delayMicros :: TimeMicros -> Arduino ()
delayMicros t = Arduino $ primitive $ DelayMicros t

delayMicrosE :: TimeMicrosE -> Arduino (Expr ())
delayMicrosE t = Arduino $ primitive $ DelayMicrosE t

digitalRead :: Pin -> Arduino Bool
digitalRead p = Arduino $ primitive $ DigitalRead p

digitalReadE :: PinE -> Arduino (Expr Bool)
digitalReadE p = Arduino $ primitive $ DigitalReadE p

digitalPortRead :: Pin -> Word8 -> Arduino Word8
digitalPortRead p m = Arduino $ primitive $ DigitalPortRead p m

digitalPortReadE :: PinE -> Expr Word8 -> Arduino (Expr Word8)
digitalPortReadE p m = Arduino $ primitive $ DigitalPortReadE p m

analogRead :: Pin -> Arduino Word16
analogRead p = Arduino $ primitive $ AnalogRead p

analogReadE :: PinE -> Arduino (Expr Word16)
analogReadE p = Arduino $ primitive $ AnalogReadE p

i2cRead :: SlaveAddress -> Word8 -> Arduino [Word8]
i2cRead sa cnt = Arduino $ primitive $ I2CRead sa cnt

i2cReadE :: SlaveAddressE -> Expr Word8 -> Arduino (Expr [Word8])
i2cReadE sa cnt = Arduino $ primitive $ I2CReadE sa cnt

serialAvailable :: Word8 -> Arduino Word8
serialAvailable p = Arduino $ primitive $ SerialAvailable p

serialAvailableE :: Expr Word8 -> Arduino (Expr Word8)
serialAvailableE p = Arduino $ primitive $ SerialAvailableE p

serialRead :: Word8 -> Arduino Int32
serialRead p = Arduino $ primitive $ SerialRead p

serialReadE :: Expr Word8 -> Arduino (Expr Int32)
serialReadE p = Arduino $ primitive $ SerialReadE p

serialReadList :: Word8 -> Arduino [Word8]
serialReadList p = Arduino $ primitive $ SerialReadList p

serialReadListE :: Expr Word8 -> Arduino (Expr [Word8])
serialReadListE p = Arduino $ primitive $ SerialReadListE p

stepper2Pin :: Word16 -> Pin -> Pin -> Arduino Word8
stepper2Pin s p1 p2 = Arduino $ primitive $ Stepper2Pin s p1 p2

stepper2PinE :: Expr Word16 -> PinE -> PinE -> Arduino (Expr Word8)
stepper2PinE s p1 p2 = Arduino $ primitive $ Stepper2PinE s p1 p2

stepper4Pin :: Word16 -> Pin -> Pin -> Pin -> Pin -> Arduino Word8
stepper4Pin s p1 p2 p3 p4 = Arduino $ primitive $ Stepper4Pin s p1 p2 p3 p4

stepper4PinE :: Expr Word16 -> PinE -> PinE -> PinE -> PinE -> Arduino (Expr Word8)
stepper4PinE s p1 p2 p3 p4 = Arduino $ primitive $ Stepper4PinE s p1 p2 p3 p4

stepperStep :: Word8 -> Int16 -> Arduino ()
stepperStep st s = Arduino $ primitive $ StepperStepE (lit st) (lit s)

stepperStepE :: Expr Word8 -> Expr Int16 -> Arduino ()
stepperStepE st s = Arduino $ primitive $ StepperStepE st s

servoAttach :: Pin -> Arduino Word8
servoAttach p = Arduino $ primitive $ ServoAttach p

servoAttachE :: PinE -> Arduino (Expr Word8)
servoAttachE p = Arduino $ primitive $ ServoAttachE p

servoAttachMinMax :: Pin -> Int16 -> Int16 -> Arduino Word8
servoAttachMinMax p mi ma = Arduino $ primitive $ ServoAttachMinMax p mi ma

servoAttachMinMaxE :: PinE -> Expr Int16 -> Expr Int16 -> Arduino (Expr Word8)
servoAttachMinMaxE p mi ma = Arduino $ primitive $ ServoAttachMinMaxE p mi ma

servoRead :: Word8 -> Arduino Int16
servoRead s = Arduino $ primitive $ ServoRead s

servoReadE :: Expr Word8 -> Arduino (Expr Int16)
servoReadE s = Arduino $ primitive $ ServoReadE s

servoReadMicros :: Word8 -> Arduino Int16
servoReadMicros s = Arduino $ primitive $ ServoReadMicros s

servoReadMicrosE :: Expr Word8 -> Arduino (Expr Int16)
servoReadMicrosE s = Arduino $ primitive $ ServoReadMicrosE s

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Arduino $ primitive QueryAllTasks

queryAllTasksE :: Arduino (Expr [TaskID])
queryAllTasksE = Arduino $ primitive QueryAllTasksE

queryTask :: TaskID -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTask tid = Arduino $ primitive $ QueryTask tid

queryTaskE :: TaskIDE -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTaskE tid = Arduino $ primitive $ QueryTaskE tid

bootTaskE :: Expr [Word8] -> Arduino (Expr Bool)
bootTaskE tids = Arduino $ primitive $ BootTaskE tids

debug :: [Word8] -> Arduino ()
debug msg = Arduino $ primitive $ Debug msg

debugE :: Expr [Word8] -> Arduino ()
debugE msg = Arduino $ primitive $ DebugE msg

debugListen :: Arduino ()
debugListen = Arduino $ primitive $ DebugListen

die :: String -> [String] -> Arduino ()
die msg msgs = Arduino $ primitive $ Die msg msgs

class ExprB a => ArduinoConditional a where
    ifThenElseE  :: Expr Bool -> Arduino (Expr a) ->
                        Arduino (Expr a) -> Arduino (Expr a)

instance ArduinoConditional () where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseUnitE be tps eps

instance ArduinoConditional Bool where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseBoolE be tps eps

instance ArduinoConditional Word8 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseWord8E be tps eps

instance ArduinoConditional Word16 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseWord16E be tps eps

instance ArduinoConditional Word32 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseWord32E be tps eps

instance ArduinoConditional Int8 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseInt8E be tps eps

instance ArduinoConditional Int16 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseInt16E be tps eps

instance ArduinoConditional Int32 where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseInt32E be tps eps

instance ArduinoConditional Int where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseIntE be tps eps

instance ArduinoConditional [Word8] where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseL8E be tps eps

instance ArduinoConditional Float where
    ifThenElseE be tps eps = Arduino $ primitive $ IfThenElseFloatE be tps eps

class (ExprB a, ExprB b) => ArduinoIterate a b where
    iterateE  :: Expr a -> (Expr a -> Arduino (ExprEither a b)) -> Arduino (Expr b)
    ifThenElseEither   :: Expr Bool -> Arduino (ExprEither a b) -> Arduino (ExprEither a b) -> Arduino (ExprEither a b)

-- The following ArduinoIterate functions generated by toold/GenEitherTypes.hs
instance ArduinoIterate () () where
    iterateE iv bf = Arduino $ primitive $ IterateUnitUnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitUnit be te ee

instance ArduinoIterate () Bool where
    iterateE iv bf = Arduino $ primitive $ IterateUnitBoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitBool be te ee

instance ArduinoIterate () Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitW8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitW8 be te ee

instance ArduinoIterate () Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitW16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitW16 be te ee

instance ArduinoIterate () Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitW32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitW32 be te ee

instance ArduinoIterate () Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitI8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitI8 be te ee

instance ArduinoIterate () Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitI16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitI16 be te ee

instance ArduinoIterate () Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateUnitI32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitI32 be te ee

instance ArduinoIterate () Int where
    iterateE iv bf = Arduino $ primitive $ IterateUnitIE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitI be te ee

instance ArduinoIterate () [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateUnitL8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitL8 be te ee

instance ArduinoIterate () Float where
    iterateE iv bf = Arduino $ primitive $ IterateUnitFloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseUnitFloat be te ee

instance ArduinoIterate Bool () where
    iterateE iv bf = Arduino $ primitive $ IterateBoolUnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolUnit be te ee

instance ArduinoIterate Bool Bool where
    iterateE iv bf = Arduino $ primitive $ IterateBoolBoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolBool be te ee

instance ArduinoIterate Bool Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolW8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolW8 be te ee

instance ArduinoIterate Bool Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolW16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolW16 be te ee

instance ArduinoIterate Bool Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolW32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolW32 be te ee

instance ArduinoIterate Bool Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolI8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolI8 be te ee

instance ArduinoIterate Bool Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolI16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolI16 be te ee

instance ArduinoIterate Bool Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateBoolI32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolI32 be te ee

instance ArduinoIterate Bool Int where
    iterateE iv bf = Arduino $ primitive $ IterateBoolIE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolI be te ee

instance ArduinoIterate Bool [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateBoolL8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolL8 be te ee

instance ArduinoIterate Bool Float where
    iterateE iv bf = Arduino $ primitive $ IterateBoolFloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseBoolFloat be te ee

instance ArduinoIterate Word8 () where
    iterateE iv bf = Arduino $ primitive $ IterateW8UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8Unit be te ee

instance ArduinoIterate Word8 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateW8BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8Bool be te ee

instance ArduinoIterate Word8 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateW8W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8W8 be te ee

instance ArduinoIterate Word8 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateW8W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8W16 be te ee

instance ArduinoIterate Word8 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateW8W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8W32 be te ee

instance ArduinoIterate Word8 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateW8I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8I8 be te ee

instance ArduinoIterate Word8 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateW8I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8I16 be te ee

instance ArduinoIterate Word8 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateW8I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8I32 be te ee

instance ArduinoIterate Word8 Int where
    iterateE iv bf = Arduino $ primitive $ IterateW8IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8I be te ee

instance ArduinoIterate Word8 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateW8L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8L8 be te ee

instance ArduinoIterate Word8 Float where
    iterateE iv bf = Arduino $ primitive $ IterateW8FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW8Float be te ee

instance ArduinoIterate Word16 () where
    iterateE iv bf = Arduino $ primitive $ IterateW16UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16Unit be te ee

instance ArduinoIterate Word16 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateW16BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16Bool be te ee

instance ArduinoIterate Word16 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateW16W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16W8 be te ee

instance ArduinoIterate Word16 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateW16W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16W16 be te ee

instance ArduinoIterate Word16 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateW16W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16W32 be te ee

instance ArduinoIterate Word16 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateW16I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16I8 be te ee

instance ArduinoIterate Word16 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateW16I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16I16 be te ee

instance ArduinoIterate Word16 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateW16I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16I32 be te ee

instance ArduinoIterate Word16 Int where
    iterateE iv bf = Arduino $ primitive $ IterateW16IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16I be te ee

instance ArduinoIterate Word16 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateW16L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16L8 be te ee

instance ArduinoIterate Word16 Float where
    iterateE iv bf = Arduino $ primitive $ IterateW16FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW16Float be te ee

instance ArduinoIterate Word32 () where
    iterateE iv bf = Arduino $ primitive $ IterateW32UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32Unit be te ee

instance ArduinoIterate Word32 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateW32BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32Bool be te ee

instance ArduinoIterate Word32 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateW32W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32W8 be te ee

instance ArduinoIterate Word32 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateW32W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32W16 be te ee

instance ArduinoIterate Word32 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateW32W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32W32 be te ee

instance ArduinoIterate Word32 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateW32I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32I8 be te ee

instance ArduinoIterate Word32 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateW32I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32I16 be te ee

instance ArduinoIterate Word32 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateW32I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32I32 be te ee

instance ArduinoIterate Word32 Int where
    iterateE iv bf = Arduino $ primitive $ IterateW32IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32I be te ee

instance ArduinoIterate Word32 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateW32L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32L8 be te ee

instance ArduinoIterate Word32 Float where
    iterateE iv bf = Arduino $ primitive $ IterateW32FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseW32Float be te ee

instance ArduinoIterate Int8 () where
    iterateE iv bf = Arduino $ primitive $ IterateI8UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8Unit be te ee

instance ArduinoIterate Int8 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateI8BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8Bool be te ee

instance ArduinoIterate Int8 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateI8W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8W8 be te ee

instance ArduinoIterate Int8 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateI8W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8W16 be te ee

instance ArduinoIterate Int8 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateI8W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8W32 be te ee

instance ArduinoIterate Int8 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateI8I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8I8 be te ee

instance ArduinoIterate Int8 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateI8I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8I16 be te ee

instance ArduinoIterate Int8 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateI8I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8I32 be te ee

instance ArduinoIterate Int8 Int where
    iterateE iv bf = Arduino $ primitive $ IterateI8IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8I be te ee

instance ArduinoIterate Int8 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateI8L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8L8 be te ee

instance ArduinoIterate Int8 Float where
    iterateE iv bf = Arduino $ primitive $ IterateI8FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI8Float be te ee

instance ArduinoIterate Int16 () where
    iterateE iv bf = Arduino $ primitive $ IterateI16UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16Unit be te ee

instance ArduinoIterate Int16 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateI16BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16Bool be te ee

instance ArduinoIterate Int16 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateI16W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16W8 be te ee

instance ArduinoIterate Int16 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateI16W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16W16 be te ee

instance ArduinoIterate Int16 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateI16W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16W32 be te ee

instance ArduinoIterate Int16 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateI16I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16I8 be te ee

instance ArduinoIterate Int16 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateI16I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16I16 be te ee

instance ArduinoIterate Int16 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateI16I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16I32 be te ee

instance ArduinoIterate Int16 Int where
    iterateE iv bf = Arduino $ primitive $ IterateI16IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16I be te ee

instance ArduinoIterate Int16 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateI16L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16L8 be te ee

instance ArduinoIterate Int16 Float where
    iterateE iv bf = Arduino $ primitive $ IterateI16FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI16Float be te ee

instance ArduinoIterate Int32 () where
    iterateE iv bf = Arduino $ primitive $ IterateI32UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32Unit be te ee

instance ArduinoIterate Int32 Bool where
    iterateE iv bf = Arduino $ primitive $ IterateI32BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32Bool be te ee

instance ArduinoIterate Int32 Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateI32W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32W8 be te ee

instance ArduinoIterate Int32 Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateI32W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32W16 be te ee

instance ArduinoIterate Int32 Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateI32W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32W32 be te ee

instance ArduinoIterate Int32 Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateI32I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32I8 be te ee

instance ArduinoIterate Int32 Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateI32I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32I16 be te ee

instance ArduinoIterate Int32 Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateI32I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32I32 be te ee

instance ArduinoIterate Int32 Int where
    iterateE iv bf = Arduino $ primitive $ IterateI32IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32I be te ee

instance ArduinoIterate Int32 [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateI32L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32L8 be te ee

instance ArduinoIterate Int32 Float where
    iterateE iv bf = Arduino $ primitive $ IterateI32FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseI32Float be te ee

instance ArduinoIterate Int () where
    iterateE iv bf = Arduino $ primitive $ IterateIUnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIUnit be te ee

instance ArduinoIterate Int Bool where
    iterateE iv bf = Arduino $ primitive $ IterateIBoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIBool be te ee

instance ArduinoIterate Int Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateIW8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIW8 be te ee

instance ArduinoIterate Int Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateIW16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIW16 be te ee

instance ArduinoIterate Int Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateIW32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIW32 be te ee

instance ArduinoIterate Int Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateII8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseII8 be te ee

instance ArduinoIterate Int Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateII16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseII16 be te ee

instance ArduinoIterate Int Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateII32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseII32 be te ee

instance ArduinoIterate Int Int where
    iterateE iv bf = Arduino $ primitive $ IterateIIE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseII be te ee

instance ArduinoIterate Int [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateIL8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIL8 be te ee

instance ArduinoIterate Int Float where
    iterateE iv bf = Arduino $ primitive $ IterateIFloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseIFloat be te ee

instance ArduinoIterate [Word8] () where
    iterateE iv bf = Arduino $ primitive $ IterateL8UnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8Unit be te ee

instance ArduinoIterate [Word8] Bool where
    iterateE iv bf = Arduino $ primitive $ IterateL8BoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8Bool be te ee

instance ArduinoIterate [Word8] Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateL8W8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8W8 be te ee

instance ArduinoIterate [Word8] Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateL8W16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8W16 be te ee

instance ArduinoIterate [Word8] Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateL8W32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8W32 be te ee

instance ArduinoIterate [Word8] Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateL8I8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8I8 be te ee

instance ArduinoIterate [Word8] Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateL8I16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8I16 be te ee

instance ArduinoIterate [Word8] Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateL8I32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8I32 be te ee

instance ArduinoIterate [Word8] Int where
    iterateE iv bf = Arduino $ primitive $ IterateL8IE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8I be te ee

instance ArduinoIterate [Word8] [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateL8L8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8L8 be te ee

instance ArduinoIterate [Word8] Float where
    iterateE iv bf = Arduino $ primitive $ IterateL8FloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseL8Float be te ee

instance ArduinoIterate Float () where
    iterateE iv bf = Arduino $ primitive $ IterateFloatUnitE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatUnit be te ee

instance ArduinoIterate Float Bool where
    iterateE iv bf = Arduino $ primitive $ IterateFloatBoolE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatBool be te ee

instance ArduinoIterate Float Word8 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatW8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatW8 be te ee

instance ArduinoIterate Float Word16 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatW16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatW16 be te ee

instance ArduinoIterate Float Word32 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatW32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatW32 be te ee

instance ArduinoIterate Float Int8 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatI8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatI8 be te ee

instance ArduinoIterate Float Int16 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatI16E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatI16 be te ee

instance ArduinoIterate Float Int32 where
    iterateE iv bf = Arduino $ primitive $ IterateFloatI32E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatI32 be te ee

instance ArduinoIterate Float Int where
    iterateE iv bf = Arduino $ primitive $ IterateFloatIE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatI be te ee

instance ArduinoIterate Float [Word8] where
    iterateE iv bf = Arduino $ primitive $ IterateFloatL8E iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatL8 be te ee

instance ArduinoIterate Float Float where
    iterateE iv bf = Arduino $ primitive $ IterateFloatFloatE iv bf
    ifThenElseEither be te ee = Arduino $ primitive $ IfThenElseFloatFloat be te ee

whileE :: ArduinoIterate a a => Expr a -> (Expr a -> Expr Bool) ->
                     (Expr a -> Arduino (Expr a)) -> Arduino (Expr a)
whileE i tf bf = iterateE i ibf
  where
    ibf i' = do
        ifThenElseEither (tf i') (do
                                    res <- bf i'
                                    return $ ExprLeft res) (return $ ExprRight i')

repeatUntilE :: ArduinoIterate a a => Expr a -> (Expr a -> Expr Bool) ->
                     (Expr a -> Arduino (Expr a)) -> Arduino (Expr a)
repeatUntilE i tf bf = iterateE i ibf
  where
    ibf i' = do
        res <- bf i'
        ifThenElseEither (tf i') (return $ ExprLeft res) (return $ ExprRight i')

loopE :: Arduino (Expr ()) -> Arduino (Expr ())
loopE bf = iterateE LitUnit (\_ -> bf >> (return $ ExprLeft LitUnit))

forInE :: Expr [Word8] -> (Expr Word8 -> Arduino (Expr ())) -> Arduino (Expr ())
forInE ws bf = iterateE 0 ibf
  where
    ibf i = do
        _ <- bf (ws !!* i)
        ifThenElseEither (i `lessE` (len ws)) (return $ ExprLeft (i+1)) (return $ ExprRight LitUnit)

class (ExprB a, ExprB b) => ArduinoLambdaExpr a b where
    lamExpr :: Expr a -> Arduino (Expr b) -> Arduino (Expr b)
    appExpr :: Arduino (Expr b) -> Expr a -> Arduino (Expr b)

class ExprB a => ArduinoLambdaApp a where
    appLambda :: String -> Arduino (Expr a) -> Arduino (Expr a)

instance ArduinoLambdaExpr Word8 () where
    lamExpr arg bod = Arduino $ primitive $ LamExprWord8Unit arg bod
    appExpr f arg = Arduino $ primitive $ AppExprWord8Unit f arg

instance ArduinoLambdaApp () where
    appLambda n f = Arduino $ primitive $ AppLambdaUnit n f

-- | A response, as returned from the Arduino
data Response = DelayResp
              | Firmware Word16                      -- ^ Firmware version (maj/min)
              | ProcessorType Word8                  -- ^ Processor report
              | MicrosReply Word32                   -- ^ Elapsed Microseconds
              | MillisReply Word32                   -- ^ Elapsed Milliseconds
              | DigitalReply Word8                   -- ^ Status of a pin
              | DigitalPortReply Word8               -- ^ Status of a port
              | AnalogReply Word16                   -- ^ Status of an analog pin
              | StringMessage  String                -- ^ String message from Firmware
              | I2CReply [Word8]                     -- ^ Response to a I2C Read
              | SerialAvailableReply Word8
              | SerialReadReply Int32
              | SerialReadListReply [Word8]
              | Stepper2PinReply Word8
              | Stepper4PinReply Word8
              | StepperStepReply
              | ServoAttachReply Word8
              | ServoReadReply Int16
              | ServoReadMicrosReply Int16
              | QueryAllTasksReply [Word8]           -- ^ Response to Query All Tasks
              | QueryTaskReply (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
              | BootTaskResp Word8
              | NewReply Word8
              | ReadRefBReply Bool
              | ReadRefW8Reply Word8
              | ReadRefW16Reply Word16
              | ReadRefW32Reply Word32
              | ReadRefI8Reply Int8
              | ReadRefI16Reply Int16
              | ReadRefI32Reply Int32
              | ReadRefL8Reply [Word8]
              | ReadRefFloatReply Float
              | IfThenElseUnitReply ()
              | IfThenElseBoolReply Bool
              | IfThenElseW8Reply Word8
              | IfThenElseW16Reply Word16
              | IfThenElseW32Reply Word32
              | IfThenElseI8Reply Int8
              | IfThenElseI16Reply Int16
              | IfThenElseI32Reply Int32
              | IfThenElseL8Reply [Word8]
              | IfThenElseFloatReply Float
              | IfThenElseUnitLeftReply ()
              | IfThenElseBoolLeftReply Bool
              | IfThenElseW8LeftReply Word8
              | IfThenElseW16LeftReply Word16
              | IfThenElseW32LeftReply Word32
              | IfThenElseI8LeftReply Int8
              | IfThenElseI16LeftReply Int16
              | IfThenElseI32LeftReply Int32
              | IfThenElseL8LeftReply [Word8]
              | IfThenElseFloatLeftReply Float
              | IterateUnitReply
              | IterateBoolReply Bool
              | IterateW8Reply Word8
              | IterateW16Reply Word16
              | IterateW32Reply Word32
              | IterateI8Reply Int8
              | IterateI16Reply Int16
              | IterateI32Reply Int32
              | IterateL8Reply [Word8]
              | IterateFloatReply Float
              | DebugResp
              | FailedNewRef
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
              | EmptyFrame
              | InvalidChecksumFrame [Word8]
    deriving Show

-- | Haskino Firmware commands, see:
-- | https://github.com/ku-fpg/haskino/wiki/Haskino-Firmware-Protocol-Definition
data FirmwareCmd = BC_CMD_SYSTEM_RESET
                 | BC_CMD_SET_PIN_MODE
                 | BC_CMD_DELAY_MILLIS
                 | BC_CMD_DELAY_MICROS
                 | BC_CMD_ITERATE
                 | BC_CMD_IF_THEN_ELSE
                 | BS_CMD_REQUEST_VERSION
                 | BS_CMD_REQUEST_TYPE
                 | BS_CMD_REQUEST_MICROS
                 | BS_CMD_REQUEST_MILLIS
                 | BS_CMD_DEBUG
                 | DIG_CMD_READ_PIN
                 | DIG_CMD_WRITE_PIN
                 | DIG_CMD_READ_PORT
                 | DIG_CMD_WRITE_PORT
                 | ALG_CMD_READ_PIN
                 | ALG_CMD_WRITE_PIN
                 | ALG_CMD_TONE_PIN
                 | ALG_CMD_NOTONE_PIN
                 | I2C_CMD_CONFIG
                 | I2C_CMD_READ
                 | I2C_CMD_WRITE
                 | SER_CMD_BEGIN
                 | SER_CMD_END
                 | SER_CMD_AVAIL
                 | SER_CMD_READ
                 | SER_CMD_READ_LIST
                 | SER_CMD_WRITE
                 | SER_CMD_WRITE_LIST
                 | STEP_CMD_2PIN
                 | STEP_CMD_4PIN
                 | STEP_CMD_SET_SPEED
                 | STEP_CMD_STEP
                 | SRVO_CMD_ATTACH
                 | SRVO_CMD_DETACH
                 | SRVO_CMD_WRITE
                 | SRVO_CMD_WRITE_MICROS
                 | SRVO_CMD_READ
                 | SRVO_CMD_READ_MICROS
                 | SCHED_CMD_CREATE_TASK
                 | SCHED_CMD_DELETE_TASK
                 | SCHED_CMD_ADD_TO_TASK
                 | SCHED_CMD_SCHED_TASK
                 | SCHED_CMD_QUERY_ALL
                 | SCHED_CMD_QUERY
                 | SCHED_CMD_RESET
                 | SCHED_CMD_BOOT_TASK
                 | SCHED_CMD_GIVE_SEM
                 | SCHED_CMD_TAKE_SEM
                 | SCHED_CMD_ATTACH_INT
                 | SCHED_CMD_DETACH_INT
                 | SCHED_CMD_INTERRUPTS
                 | SCHED_CMD_NOINTERRUPTS
                 | REF_CMD_NEW
                 | REF_CMD_READ
                 | REF_CMD_WRITE
                 | EXPR_CMD_RET
                 | UNKNOWN_COMMAND
                deriving Show

-- | Compute the numeric value of a command
firmwareCmdVal :: FirmwareCmd -> Word8
firmwareCmdVal BC_CMD_SYSTEM_RESET      = 0x10
firmwareCmdVal BC_CMD_SET_PIN_MODE      = 0x11
firmwareCmdVal BC_CMD_DELAY_MILLIS      = 0x12
firmwareCmdVal BC_CMD_DELAY_MICROS      = 0x13
firmwareCmdVal BC_CMD_ITERATE           = 0x14
firmwareCmdVal BC_CMD_IF_THEN_ELSE      = 0x15
firmwareCmdVal BS_CMD_REQUEST_VERSION   = 0x20
firmwareCmdVal BS_CMD_REQUEST_TYPE      = 0x21
firmwareCmdVal BS_CMD_REQUEST_MICROS    = 0x22
firmwareCmdVal BS_CMD_REQUEST_MILLIS    = 0x23
firmwareCmdVal BS_CMD_DEBUG             = 0x24
firmwareCmdVal DIG_CMD_READ_PIN         = 0x30
firmwareCmdVal DIG_CMD_WRITE_PIN        = 0x31
firmwareCmdVal DIG_CMD_READ_PORT        = 0x32
firmwareCmdVal DIG_CMD_WRITE_PORT       = 0x33
firmwareCmdVal ALG_CMD_READ_PIN         = 0x40
firmwareCmdVal ALG_CMD_WRITE_PIN        = 0x41
firmwareCmdVal ALG_CMD_TONE_PIN         = 0x42
firmwareCmdVal ALG_CMD_NOTONE_PIN       = 0x43
firmwareCmdVal I2C_CMD_CONFIG           = 0x50
firmwareCmdVal I2C_CMD_READ             = 0x51
firmwareCmdVal I2C_CMD_WRITE            = 0x52
firmwareCmdVal STEP_CMD_2PIN            = 0x60
firmwareCmdVal STEP_CMD_4PIN            = 0x61
firmwareCmdVal STEP_CMD_SET_SPEED       = 0x62
firmwareCmdVal STEP_CMD_STEP            = 0x63
firmwareCmdVal SRVO_CMD_ATTACH          = 0x80
firmwareCmdVal SRVO_CMD_DETACH          = 0x81
firmwareCmdVal SRVO_CMD_WRITE           = 0x82
firmwareCmdVal SRVO_CMD_WRITE_MICROS    = 0x83
firmwareCmdVal SRVO_CMD_READ            = 0x84
firmwareCmdVal SRVO_CMD_READ_MICROS     = 0x85
firmwareCmdVal SCHED_CMD_CREATE_TASK    = 0xA0
firmwareCmdVal SCHED_CMD_DELETE_TASK    = 0xA1
firmwareCmdVal SCHED_CMD_ADD_TO_TASK    = 0xA2
firmwareCmdVal SCHED_CMD_SCHED_TASK     = 0xA3
firmwareCmdVal SCHED_CMD_QUERY          = 0xA4
firmwareCmdVal SCHED_CMD_QUERY_ALL      = 0xA5
firmwareCmdVal SCHED_CMD_RESET          = 0xA6
firmwareCmdVal SCHED_CMD_BOOT_TASK      = 0xA7
firmwareCmdVal SCHED_CMD_TAKE_SEM       = 0xA8
firmwareCmdVal SCHED_CMD_GIVE_SEM       = 0xA9
firmwareCmdVal SCHED_CMD_ATTACH_INT     = 0xAA
firmwareCmdVal SCHED_CMD_DETACH_INT     = 0xAB
firmwareCmdVal SCHED_CMD_INTERRUPTS     = 0xAC
firmwareCmdVal SCHED_CMD_NOINTERRUPTS   = 0xAD
firmwareCmdVal REF_CMD_NEW              = 0xC0
firmwareCmdVal REF_CMD_READ             = 0xC1
firmwareCmdVal REF_CMD_WRITE            = 0xC2
firmwareCmdVal SER_CMD_BEGIN            = 0xE0
firmwareCmdVal SER_CMD_END              = 0xE1
firmwareCmdVal SER_CMD_AVAIL            = 0xE2
firmwareCmdVal SER_CMD_READ             = 0xE3
firmwareCmdVal SER_CMD_READ_LIST        = 0xE4
firmwareCmdVal SER_CMD_WRITE            = 0xE5
firmwareCmdVal SER_CMD_WRITE_LIST       = 0xE6
firmwareCmdVal _                        = 0x00

-- | Compute the numeric value of a command
firmwareValCmd :: Word8 -> FirmwareCmd
firmwareValCmd 0x10 = BC_CMD_SYSTEM_RESET
firmwareValCmd 0x11 = BC_CMD_SET_PIN_MODE
firmwareValCmd 0x12 = BC_CMD_DELAY_MILLIS
firmwareValCmd 0x13 = BC_CMD_DELAY_MICROS
firmwareValCmd 0x14 = BC_CMD_ITERATE
firmwareValCmd 0x15 = BC_CMD_IF_THEN_ELSE
firmwareValCmd 0x20 = BS_CMD_REQUEST_VERSION
firmwareValCmd 0x21 = BS_CMD_REQUEST_TYPE
firmwareValCmd 0x22 = BS_CMD_REQUEST_MICROS
firmwareValCmd 0x23 = BS_CMD_REQUEST_MILLIS
firmwareValCmd 0x24 = BS_CMD_DEBUG
firmwareValCmd 0x30 = DIG_CMD_READ_PIN
firmwareValCmd 0x31 = DIG_CMD_WRITE_PIN
firmwareValCmd 0x32 = DIG_CMD_READ_PORT
firmwareValCmd 0x33 = DIG_CMD_WRITE_PORT
firmwareValCmd 0x40 = ALG_CMD_READ_PIN
firmwareValCmd 0x41 = ALG_CMD_WRITE_PIN
firmwareValCmd 0x42 = ALG_CMD_TONE_PIN
firmwareValCmd 0x43 = ALG_CMD_NOTONE_PIN
firmwareValCmd 0x50 = I2C_CMD_CONFIG
firmwareValCmd 0x51 = I2C_CMD_READ
firmwareValCmd 0x52 = I2C_CMD_WRITE
firmwareValCmd 0x60 = STEP_CMD_2PIN
firmwareValCmd 0x61 = STEP_CMD_4PIN
firmwareValCmd 0x62 = STEP_CMD_SET_SPEED
firmwareValCmd 0x63 = STEP_CMD_STEP
firmwareValCmd 0x80 = SRVO_CMD_ATTACH
firmwareValCmd 0x81 = SRVO_CMD_DETACH
firmwareValCmd 0x82 = SRVO_CMD_WRITE
firmwareValCmd 0x83 = SRVO_CMD_WRITE_MICROS
firmwareValCmd 0x84 = SRVO_CMD_READ
firmwareValCmd 0x85 = SRVO_CMD_READ_MICROS
firmwareValCmd 0xA0 = SCHED_CMD_CREATE_TASK
firmwareValCmd 0xA1 = SCHED_CMD_DELETE_TASK
firmwareValCmd 0xA2 = SCHED_CMD_ADD_TO_TASK
firmwareValCmd 0xA3 = SCHED_CMD_SCHED_TASK
firmwareValCmd 0xA4 = SCHED_CMD_QUERY
firmwareValCmd 0xA5 = SCHED_CMD_QUERY_ALL
firmwareValCmd 0xA6 = SCHED_CMD_RESET
firmwareValCmd 0xA7 = SCHED_CMD_BOOT_TASK
firmwareValCmd 0xA8 = SCHED_CMD_TAKE_SEM
firmwareValCmd 0xA9 = SCHED_CMD_GIVE_SEM
firmwareValCmd 0xAA = SCHED_CMD_ATTACH_INT
firmwareValCmd 0xAB = SCHED_CMD_DETACH_INT
firmwareValCmd 0xAC = SCHED_CMD_INTERRUPTS
firmwareValCmd 0xAD = SCHED_CMD_NOINTERRUPTS
firmwareValCmd 0xC0 = REF_CMD_NEW
firmwareValCmd 0xC1 = REF_CMD_READ
firmwareValCmd 0xC2 = REF_CMD_WRITE
firmwareValCmd 0xD0 = EXPR_CMD_RET
firmwareValCmd 0xE0 = SER_CMD_BEGIN
firmwareValCmd 0xE1 = SER_CMD_END
firmwareValCmd 0xE2 = SER_CMD_AVAIL
firmwareValCmd 0xE3 = SER_CMD_READ
firmwareValCmd 0xE4 = SER_CMD_READ_LIST
firmwareValCmd 0xE5 = SER_CMD_WRITE
firmwareValCmd 0xE6 = SER_CMD_WRITE_LIST
firmwareValCmd _    = UNKNOWN_COMMAND

-- | Firmware replies, see:
-- | https://github.com/ku-fpg/haskino/wiki/Haskino-Firmware-Protocol-Definition
data FirmwareReply =  BC_RESP_DELAY
                   |  BC_RESP_IF_THEN_ELSE
                   |  BC_RESP_ITERATE
                   |  BS_RESP_VERSION
                   |  BS_RESP_TYPE
                   |  BS_RESP_MICROS
                   |  BS_RESP_MILLIS
                   |  BS_RESP_STRING
                   |  BS_RESP_DEBUG
                   |  DIG_RESP_READ_PIN
                   |  DIG_RESP_READ_PORT
                   |  ALG_RESP_READ_PIN
                   |  I2C_RESP_READ
                   |  SER_RESP_AVAIL
                   |  SER_RESP_READ
                   |  SER_RESP_READ_LIST
                   |  STEP_RESP_2PIN
                   |  STEP_RESP_4PIN
                   |  STEP_RESP_STEP
                   |  SRVO_RESP_ATTACH
                   |  SRVO_RESP_READ
                   |  SRVO_RESP_READ_MICROS
                   |  SCHED_RESP_QUERY
                   |  SCHED_RESP_QUERY_ALL
                   |  SCHED_RESP_BOOT
                   |  REF_RESP_NEW
                   |  REF_RESP_READ
                   |  EXPR_RESP_RET
                deriving Show

getFirmwareReply :: Word8 -> Either Word8 FirmwareReply
getFirmwareReply 0x18 = Right BC_RESP_DELAY
getFirmwareReply 0x19 = Right BC_RESP_IF_THEN_ELSE
getFirmwareReply 0x1A = Right BC_RESP_ITERATE
getFirmwareReply 0x28 = Right BS_RESP_VERSION
getFirmwareReply 0x29 = Right BS_RESP_TYPE
getFirmwareReply 0x2A = Right BS_RESP_MICROS
getFirmwareReply 0x2B = Right BS_RESP_MILLIS
getFirmwareReply 0x2C = Right BS_RESP_STRING
getFirmwareReply 0x2D = Right BS_RESP_DEBUG
getFirmwareReply 0x38 = Right DIG_RESP_READ_PIN
getFirmwareReply 0x39 = Right DIG_RESP_READ_PORT
getFirmwareReply 0x48 = Right ALG_RESP_READ_PIN
getFirmwareReply 0x58 = Right I2C_RESP_READ
getFirmwareReply 0x68 = Right STEP_RESP_2PIN
getFirmwareReply 0x69 = Right STEP_RESP_4PIN
getFirmwareReply 0x6A = Right STEP_RESP_STEP
getFirmwareReply 0x88 = Right SRVO_RESP_ATTACH
getFirmwareReply 0x89 = Right SRVO_RESP_READ
getFirmwareReply 0x8A = Right SRVO_RESP_READ_MICROS
getFirmwareReply 0xB0 = Right SCHED_RESP_QUERY
getFirmwareReply 0xB1 = Right SCHED_RESP_QUERY_ALL
getFirmwareReply 0xB2 = Right SCHED_RESP_BOOT
getFirmwareReply 0xC8 = Right REF_RESP_NEW
getFirmwareReply 0xC9 = Right REF_RESP_READ
getFirmwareReply 0xD8 = Right EXPR_RESP_RET
getFirmwareReply 0xE8 = Right SER_RESP_AVAIL
getFirmwareReply 0xE9 = Right SER_RESP_READ
getFirmwareReply 0xEA = Right SER_RESP_READ_LIST
getFirmwareReply n    = Left n

data Processor = ATMEGA8
               | ATMEGA168
               | ATMEGA328P
               | ATMEGA1280
               | ATMEGA256
               | ATMEGA32U4
               | ATMEGA644P
               | ATMEGA644
               | ATMEGA645
               | SAM3X8E
               | X86
               | QUARK
               | UNKNOWN_PROCESSOR
    deriving (Eq, Show, Enum)
