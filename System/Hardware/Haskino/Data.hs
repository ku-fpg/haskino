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
{-# LANGUAGE FlexibleInstances, GADTs, KindSignatures, RankNTypes,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             GeneralizedNewtypeDeriving, NamedFieldPuns, DataKinds #-}

module System.Hardware.Haskino.Data where

import           Control.Applicative
import           Control.Concurrent (Chan, MVar, ThreadId, withMVar, modifyMVar, 
                                     modifyMVar_, putMVar, takeMVar, readMVar,
                                     newEmptyMVar)
import           Control.Monad (ap, liftM2, forever)
import           Control.Monad.Trans
import           Control.Remote.Monad

import           Data.Bits ((.|.), (.&.), setBit)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           Data.Monoid
import           Data.Word (Word8, Word16, Word32)
import           Data.Int (Int8, Int16, Int32)

import           System.Hardware.Serialport (SerialPort)

import           System.Hardware.Haskino.Expr
import           System.Hardware.Haskino.Utils

-----------------------------------------------------------------------------

-- | The Arduino remote monad
newtype Arduino a = Arduino (RemoteMonad ArduinoCommand ArduinoProcedure a)
  deriving (Functor, Applicative, Monad)
  
instance MonadIO Arduino where
  liftIO m = Arduino $ procedure $ LiftIO m

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

-- | LCD's connected to the board
data LCD = LCD {
                 lcdController     :: LCDController -- ^ Actual controller
               , lcdState          :: MVar LCDData  -- ^ State information    
               }

data LCDE = LCDE {
                  lcdControllerE   :: LCDController  -- ^ Actual controller
                , lcdStateE        :: LCDDataE  -- ^ State information    
                }

-- | Hitachi LCD controller: See: <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller>.
-- We model only the 4-bit variant, with RS and EN lines only. (The most common Arduino usage.)
-- The data sheet can be seen at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>.
data LCDController = 
    Hitachi44780 {
                       lcdRS       :: Pin  -- ^ Hitachi pin @ 4@: Register-select
                     , lcdEN       :: Pin  -- ^ Hitachi pin @ 6@: Enable
                     , lcdD4       :: Pin  -- ^ Hitachi pin @11@: Data line @4@
                     , lcdD5       :: Pin  -- ^ Hitachi pin @12@: Data line @5@
                     , lcdD6       :: Pin  -- ^ Hitachi pin @13@: Data line @6@
                     , lcdD7       :: Pin  -- ^ Hitachi pin @14@: Data line @7@
                     , lcdBL       :: Maybe Pin -- ^ Backlight control pin (if present)
                     , lcdRows     :: Word8  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Word8  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
    | I2CHitachi44780 {
                       address     :: Word8 -- ^ I2C Slave Address of LCD
                     , lcdRows     :: Word8  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Word8  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
                     deriving Show

-- | State of the LCD, a mere 8-bit word for the Hitachi
data LCDData = LCDData {
                  lcdDisplayMode    :: Word8         -- ^ Display mode (left/right/scrolling etc.)
                , lcdDisplayControl :: Word8         -- ^ Display control (blink on/off, display on/off etc.)
                , lcdGlyphCount     :: Word8         -- ^ Count of custom created glyphs (typically at most 8)
                , lcdBacklightState :: Bool
                }

data LCDDataE = LCDDataE {
                  lcdDisplayModeE    :: RemoteRef Word8         -- ^ Display mode (left/right/scrolling etc.)
                , lcdDisplayControlE :: RemoteRef Word8         -- ^ Display control (blink on/off, display on/off etc.)
                , lcdGlyphCountE     :: RemoteRef Word8         -- ^ Count of custom created glyphs (typically at most 8)
                , lcdBacklightStateE :: RemoteRef Bool
                }

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

data ArduinoCommand =
       SystemReset                              -- ^ Send system reset
     | SetPinModeE PinE (Expr Word8)            -- ^ Set the mode on a pin
     | DigitalPortWriteE PinE (Expr Word8) (Expr Word8)
     | DigitalWriteE PinE (Expr Bool)              
     | AnalogWriteE PinE (Expr Word16)
     | ToneE PinE (Expr Word16) (Maybe (Expr Word32))       -- ^ Play a tone on a pin
     | NoToneE PinE                             -- ^ Stop playing a tone on a pin
     | I2CWrite SlaveAddressE (Expr [Word8])
     | I2CConfig
     | StepperSetSpeedE (Expr Word8) (Expr Int32)
     | ServoDetachE (Expr Word8)
     | ServoWriteE (Expr Word8) (Expr Int16)
     | ServoWriteMicrosE (Expr Word8) (Expr Int16)
     | CreateTaskE TaskIDE (Arduino ())
     | DeleteTaskE TaskIDE
     | ScheduleTaskE TaskIDE TimeMillisE
     | ScheduleReset
     | AttachIntE PinE TaskIDE (Expr Word8)
     | DetachIntE PinE
     | Interrupts
     | NoInterrupts
     | GiveSemE (Expr Word8)
     | TakeSemE (Expr Word8)
     | WriteRemoteRefB (RemoteRef Bool) (Expr Bool)
     | WriteRemoteRefW8 (RemoteRef Word8) (Expr Word8)
     | WriteRemoteRefW16 (RemoteRef Word16) (Expr Word16)
     | WriteRemoteRefW32 (RemoteRef Word32) (Expr Word32)
     | WriteRemoteRefI8 (RemoteRef Int8) (Expr Int8)
     | WriteRemoteRefI16 (RemoteRef Int16) (Expr Int16)
     | WriteRemoteRefI32 (RemoteRef Int32) (Expr Int32)
     | WriteRemoteRefL8 (RemoteRef [Word8]) (Expr [Word8])
     | WriteRemoteRefFloat (RemoteRef Float) (Expr Float)
     | ModifyRemoteRefB (RemoteRef Bool) (Expr Bool -> Expr Bool)
     | ModifyRemoteRefW8 (RemoteRef Word8) (Expr Word8 -> Expr Word8)
     | ModifyRemoteRefW16 (RemoteRef Word16) (Expr Word16 -> Expr Word16)
     | ModifyRemoteRefW32 (RemoteRef Word32) (Expr Word32 -> Expr Word32)
     | ModifyRemoteRefI8 (RemoteRef Int8) (Expr Int8 -> Expr Int8)
     | ModifyRemoteRefI16 (RemoteRef Int16) (Expr Int16 -> Expr Int16)
     | ModifyRemoteRefI32 (RemoteRef Int32) (Expr Int32 -> Expr Int32)
     | ModifyRemoteRefL8 (RemoteRef [Word8]) (Expr [Word8] -> Expr [Word8])
     | ModifyRemoteRefFloat (RemoteRef Float) (Expr Float -> Expr Float)
     | Loop (Arduino ())
     | WhileRemoteRefB (RemoteRef Bool) (Expr Bool -> Expr Bool) (Expr Bool -> Expr Bool) (Arduino ())
     | WhileRemoteRefW8 (RemoteRef Word8) (Expr Word8 -> Expr Bool) (Expr Word8 -> Expr Word8) (Arduino ())
     | WhileRemoteRefW16 (RemoteRef Word16) (Expr Word16 -> Expr Bool) (Expr Word16 -> Expr Word16) (Arduino ())
     | WhileRemoteRefW32 (RemoteRef Word32) (Expr Word32 -> Expr Bool) (Expr Word32 -> Expr Word32) (Arduino ())
     | WhileRemoteRefI8 (RemoteRef Int8) (Expr Int8 -> Expr Bool) (Expr Int8 -> Expr Int8) (Arduino ())
     | WhileRemoteRefI16 (RemoteRef Int16) (Expr Int16 -> Expr Bool) (Expr Int16 -> Expr Int16) (Arduino ())
     | WhileRemoteRefI32 (RemoteRef Int32) (Expr Int32 -> Expr Bool) (Expr Int32 -> Expr Int32) (Arduino ())
     | WhileRemoteRefFloat (RemoteRef Float) (Expr Float -> Expr Bool) (Expr Float -> Expr Float) (Arduino ())
     | WhileRemoteRefL8 (RemoteRef [Word8]) (Expr [Word8] -> Expr Bool) (Expr [Word8] -> Expr [Word8]) (Arduino ())
     | LoopE (Arduino ())
     | ForInE (Expr [Word8]) (Expr Word8 -> Arduino ()) 
     | IfThenElse (Expr Bool) (Arduino ()) (Arduino ())
     -- ToDo: add SPI commands

systemReset :: Arduino ()
systemReset =  Arduino $ command SystemReset

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm =  Arduino $ command $ SetPinModeE (lit p) (lit $ fromIntegral $ fromEnum pm)

setPinModeE :: PinE -> PinMode -> Arduino ()
setPinModeE p pm =  Arduino $ command $ SetPinModeE p (lit $ fromIntegral $ fromEnum pm)

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p b = Arduino $ command $ DigitalWriteE (lit p) (lit b)

digitalWriteE :: PinE -> Expr Bool -> Arduino ()
digitalWriteE p b = Arduino $ command $ DigitalWriteE p b

digitalPortWrite :: Pin -> Word8 -> Word8 -> Arduino ()
digitalPortWrite p b m = Arduino $ command $ DigitalPortWriteE (lit p) (lit b) (lit m)

digitalPortWriteE :: PinE -> Expr Word8 -> Expr Word8 -> Arduino ()
digitalPortWriteE p b m = Arduino $ command $ DigitalPortWriteE p b m

analogWrite :: Pin -> Word16 -> Arduino ()
analogWrite p w = Arduino $ command $ AnalogWriteE (lit p) (lit w)

analogWriteE :: PinE -> Expr Word16 -> Arduino ()
analogWriteE p w = Arduino $ command $ AnalogWriteE p w

tone :: Pin -> Word16 -> Maybe Word32 -> Arduino ()
tone p f Nothing = Arduino $ command $ ToneE (lit p) (lit f) Nothing
tone p f (Just d) = Arduino $ command $ ToneE (lit p) (lit f) (Just $ lit d)

toneE :: PinE -> Expr Word16 -> Maybe (Expr Word32) -> Arduino ()
toneE p f d = Arduino $ command $ ToneE p f d

noTone :: Pin -> Arduino ()
noTone p = Arduino $ command $ NoToneE (lit p)

noToneE :: PinE -> Arduino ()
noToneE p = Arduino $ command $ NoToneE p

i2cWrite :: SlaveAddress -> [Word8] -> Arduino ()
i2cWrite sa ws = Arduino $ command $ I2CWrite (lit sa) (lit ws)

i2cWriteE :: SlaveAddressE -> Expr [Word8] -> Arduino ()
i2cWriteE sa ws = Arduino $ command $ I2CWrite sa ws

i2cConfig :: Arduino ()
i2cConfig = Arduino $ command $ I2CConfig

stepperSetSpeed :: Word8 -> Int32 -> Arduino ()
stepperSetSpeed st sp = Arduino $ command $ StepperSetSpeedE (lit st) (lit sp)

stepperSetSpeedE :: Expr Word8 -> Expr Int32 -> Arduino ()
stepperSetSpeedE st sp = Arduino $ command $ StepperSetSpeedE st sp

servoDetach :: Word8 -> Arduino ()
servoDetach s = Arduino $ command $ ServoDetachE (lit s)

servoDetachE :: Expr Word8 -> Arduino ()
servoDetachE s = Arduino $ command $ ServoDetachE s

servoWrite :: Word8 -> Int16 -> Arduino ()
servoWrite s w = Arduino $ command $ ServoWriteE (lit s) (lit w)

servoWriteE :: Expr Word8 -> Expr Int16 -> Arduino ()
servoWriteE s w = Arduino $ command $ ServoWriteE s w

servoWriteMicros :: Word8 -> Int16 -> Arduino ()
servoWriteMicros s w = Arduino $ command $ ServoWriteMicrosE (lit s) (lit w)

servoWriteMicrosE :: Expr Word8 -> Expr Int16 -> Arduino ()
servoWriteMicrosE s w = Arduino $ command $ ServoWriteMicrosE s w

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Arduino $ command $ CreateTaskE (lit tid) ps

createTaskE :: TaskIDE -> Arduino () -> Arduino ()
createTaskE tid ps = Arduino $ command  $ CreateTaskE tid ps

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Arduino $ command $ DeleteTaskE (lit tid)

deleteTaskE :: TaskIDE -> Arduino ()
deleteTaskE tid = Arduino $ command $ DeleteTaskE tid

scheduleTask :: TaskID -> TimeMillis -> Arduino ()
scheduleTask tid tt = Arduino $ command $ ScheduleTaskE (lit tid) (lit tt)

scheduleTaskE :: TaskIDE -> TimeMillisE -> Arduino ()
scheduleTaskE tid tt = Arduino $ command $ ScheduleTaskE tid tt

attachInt :: Pin -> TaskID -> IntMode -> Arduino ()
attachInt p tid m = Arduino $ command $ AttachIntE (lit p) (lit tid) (lit $ fromIntegral $ fromEnum m)

attachIntE :: PinE -> TaskIDE -> IntMode -> Arduino ()
attachIntE p tid m = Arduino $ command $ AttachIntE p tid (lit $ fromIntegral $ fromEnum m)

detachInt :: Pin -> Arduino ()
detachInt p = Arduino $ command $ DetachIntE (lit p)

detachIntE :: PinE -> Arduino ()
detachIntE p = Arduino $ command $ DetachIntE p

interrupts :: Arduino ()
interrupts = Arduino $ command $ Interrupts

noInterrupts :: Arduino ()
noInterrupts = Arduino $ command $ NoInterrupts

scheduleReset :: Arduino ()
scheduleReset = Arduino $ command ScheduleReset

giveSem :: Word8 -> Arduino ()
giveSem id = Arduino $ command $ GiveSemE (lit id)

giveSemE :: Expr Word8 -> Arduino ()
giveSemE id = Arduino $ command $ GiveSemE id

takeSem :: Word8 -> Arduino ()
takeSem id = Arduino $ command $ TakeSemE (lit id)

takeSemE :: Expr Word8 -> Arduino ()
takeSemE id = Arduino $ command $ TakeSemE id

loopE :: Arduino () -> Arduino()
loopE ps = Arduino $ command $ LoopE ps

forInE :: Expr [Word8] -> (Expr Word8 -> Arduino ()) -> Arduino ()
forInE ws f = Arduino $ command $ ForInE ws f

ifThenElse :: Expr Bool -> Arduino () -> Arduino() -> Arduino()
ifThenElse be tps eps = Arduino $ command $ IfThenElse be tps eps

writeRemoteRefB :: RemoteRef Bool -> Expr Bool -> Arduino ()
writeRemoteRefB r e = Arduino $ command $ WriteRemoteRefB r e

writeRemoteRefW8 :: RemoteRef Word8 -> Expr Word8 -> Arduino ()
writeRemoteRefW8 r e = Arduino $ command $ WriteRemoteRefW8 r e

writeRemoteRefW16 :: RemoteRef Word16 -> Expr Word16 -> Arduino ()
writeRemoteRefW16 r e = Arduino $ command $ WriteRemoteRefW16 r e

writeRemoteRefW32 :: RemoteRef Word32 -> Expr Word32 -> Arduino ()
writeRemoteRefW32 r e = Arduino $ command $ WriteRemoteRefW32 r e

writeRemoteRefI8 :: RemoteRef Int8 -> Expr Int8 -> Arduino ()
writeRemoteRefI8 r e = Arduino $ command $ WriteRemoteRefI8 r e

writeRemoteRefI16 :: RemoteRef Int16 -> Expr Int16 -> Arduino ()
writeRemoteRefI16 r e = Arduino $ command $ WriteRemoteRefI16 r e

writeRemoteRefI32 :: RemoteRef Int32 -> Expr Int32 -> Arduino ()
writeRemoteRefI32 r e = Arduino $ command $ WriteRemoteRefI32 r e

writeRemoteRefL8 :: RemoteRef [Word8] -> Expr [Word8] -> Arduino ()
writeRemoteRefL8 r e = Arduino $ command $ WriteRemoteRefL8 r e

writeRemoteRefFloat :: RemoteRef Float -> Expr Float -> Arduino ()
writeRemoteRefFloat r e = Arduino $ command $ WriteRemoteRefFloat r e

modifyRemoteRefB :: RemoteRef Bool -> (Expr Bool -> Expr Bool) -> Arduino ()
modifyRemoteRefB r f = Arduino $ command $ ModifyRemoteRefB r f

modifyRemoteRefW8 :: RemoteRef Word8 -> (Expr Word8 -> Expr Word8) -> Arduino ()
modifyRemoteRefW8 r f = Arduino $ command $ ModifyRemoteRefW8 r f

modifyRemoteRefW16 :: RemoteRef Word16 -> (Expr Word16 -> Expr Word16) -> Arduino ()
modifyRemoteRefW16 r f = Arduino $ command $ ModifyRemoteRefW16 r f

modifyRemoteRefW32 :: RemoteRef Word32 -> (Expr Word32 -> Expr Word32) -> Arduino ()
modifyRemoteRefW32 r f = Arduino $ command $ ModifyRemoteRefW32 r f

modifyRemoteRefI8 :: RemoteRef Int8 -> (Expr Int8 -> Expr Int8) -> Arduino ()
modifyRemoteRefI8 r f = Arduino $ command $ ModifyRemoteRefI8 r f

modifyRemoteRefI16 :: RemoteRef Int16 -> (Expr Int16 -> Expr Int16) -> Arduino ()
modifyRemoteRefI16 r f = Arduino $ command $ ModifyRemoteRefI16 r f

modifyRemoteRefI32 :: RemoteRef Int32 -> (Expr Int32 -> Expr Int32) -> Arduino ()
modifyRemoteRefI32 r f = Arduino $ command $ ModifyRemoteRefI32 r f

modifyRemoteRefL8 :: RemoteRef [Word8] -> (Expr [Word8] -> Expr [Word8]) -> Arduino ()
modifyRemoteRefL8 r f = Arduino $ command $ ModifyRemoteRefL8 r f

modifyRemoteRefFloat :: RemoteRef Float -> (Expr Float -> Expr Float) -> Arduino ()
modifyRemoteRefFloat r f = Arduino $ command $ ModifyRemoteRefFloat r f

whileRemoteRefB :: RemoteRef Bool -> (Expr Bool -> Expr Bool) -> (Expr Bool -> Expr Bool) -> Arduino () -> Arduino ()
whileRemoteRefB r bf uf cb  = Arduino $ command $ WhileRemoteRefB r bf uf cb

whileRemoteRefW8 :: RemoteRef Word8 -> (Expr Word8 -> Expr Bool) -> (Expr Word8 -> Expr Word8) -> Arduino () -> Arduino ()
whileRemoteRefW8 r bf uf cb = Arduino $ command $ WhileRemoteRefW8 r bf uf cb

whileRemoteRefW16 :: RemoteRef Word16 -> (Expr Word16 -> Expr Bool) -> (Expr Word16 -> Expr Word16) -> Arduino () -> Arduino ()
whileRemoteRefW16 r bf uf cb = Arduino $ command $ WhileRemoteRefW16 r bf uf cb

whileRemoteRefW32 :: RemoteRef Word32 -> (Expr Word32 -> Expr Bool) -> (Expr Word32 -> Expr Word32) -> Arduino () -> Arduino ()
whileRemoteRefW32 r bf uf cb = Arduino $ command $ WhileRemoteRefW32 r bf uf cb

whileRemoteRefI8 :: RemoteRef Int8 -> (Expr Int8 -> Expr Bool) -> (Expr Int8 -> Expr Int8) -> Arduino () -> Arduino ()
whileRemoteRefI8 r bf uf cb = Arduino $ command $ WhileRemoteRefI8 r bf uf cb

whileRemoteRefI16 :: RemoteRef Int16 -> (Expr Int16 -> Expr Bool) -> (Expr Int16 -> Expr Int16) -> Arduino () -> Arduino ()
whileRemoteRefI16 r bf uf cb = Arduino $ command $ WhileRemoteRefI16 r bf uf cb

whileRemoteRefI32 :: RemoteRef Int32 -> (Expr Int32 -> Expr Bool) -> (Expr Int32 -> Expr Int32) -> Arduino () -> Arduino ()
whileRemoteRefI32 r bf uf cb = Arduino $ command $ WhileRemoteRefI32 r bf uf cb

whileRemoteRefL8 :: RemoteRef [Word8] -> (Expr [Word8] -> Expr Bool) -> (Expr [Word8] -> Expr [Word8]) -> Arduino () -> Arduino ()
whileRemoteRefL8 r bf uf cb = Arduino $ command $ WhileRemoteRefL8 r bf uf cb

whileRemoteRefFloat :: RemoteRef Float -> (Expr Float -> Expr Bool) -> (Expr Float -> Expr Float) -> Arduino () -> Arduino ()
whileRemoteRefFloat r bf uf cb = Arduino $ command $ WhileRemoteRefFloat r bf uf cb

class RemoteReference a where
    newRemoteRef          :: Expr a -> Arduino (RemoteRef a)
    readRemoteRef         :: RemoteRef a -> Arduino (Expr a)
    writeRemoteRef        :: RemoteRef a -> Expr a -> Arduino ()
    modifyRemoteRef       :: RemoteRef a -> (Expr a -> Expr a) -> 
                             Arduino ()
    while                 :: RemoteRef a -> (Expr a -> Expr Bool) -> 
                             (Expr a -> Expr a) -> Arduino () -> Arduino ()

instance RemoteReference Bool where
    newRemoteRef = newRemoteRefB
    readRemoteRef = readRemoteRefB
    writeRemoteRef = writeRemoteRefB
    modifyRemoteRef = modifyRemoteRefB
    while = whileRemoteRefB

instance RemoteReference Word8 where
    newRemoteRef = newRemoteRefW8
    readRemoteRef = readRemoteRefW8
    writeRemoteRef = writeRemoteRefW8
    modifyRemoteRef = modifyRemoteRefW8
    while = whileRemoteRefW8

instance RemoteReference Word16 where
    newRemoteRef = newRemoteRefW16
    readRemoteRef = readRemoteRefW16
    writeRemoteRef = writeRemoteRefW16
    modifyRemoteRef = modifyRemoteRefW16
    while = whileRemoteRefW16

instance RemoteReference Word32 where
    newRemoteRef = newRemoteRefW32
    readRemoteRef = readRemoteRefW32
    writeRemoteRef = writeRemoteRefW32
    modifyRemoteRef = modifyRemoteRefW32
    while = whileRemoteRefW32

instance RemoteReference Int8 where
    newRemoteRef = newRemoteRefI8
    readRemoteRef = readRemoteRefI8
    writeRemoteRef = writeRemoteRefI8
    modifyRemoteRef = modifyRemoteRefI8
    while = whileRemoteRefI8

instance RemoteReference Int16 where
    newRemoteRef = newRemoteRefI16
    readRemoteRef = readRemoteRefI16
    writeRemoteRef = writeRemoteRefI16
    modifyRemoteRef = modifyRemoteRefI16
    while = whileRemoteRefI16

instance RemoteReference Int32 where
    newRemoteRef = newRemoteRefI32
    readRemoteRef = readRemoteRefI32
    writeRemoteRef = writeRemoteRefI32
    modifyRemoteRef = modifyRemoteRefI32
    while = whileRemoteRefI32

instance RemoteReference [Word8] where
    newRemoteRef = newRemoteRefL8
    readRemoteRef = readRemoteRefL8
    writeRemoteRef = writeRemoteRefL8
    modifyRemoteRef = modifyRemoteRefL8
    while = whileRemoteRefL8

instance RemoteReference Float where
    newRemoteRef = newRemoteRefFloat
    readRemoteRef = readRemoteRefFloat
    writeRemoteRef = writeRemoteRefFloat
    modifyRemoteRef = modifyRemoteRefFloat
    while = whileRemoteRefFloat

loop :: Arduino () -> Arduino ()
loop m = Arduino $ command $ Loop m

data ArduinoProcedure :: * -> * where
     QueryFirmware  :: ArduinoProcedure Word16                   -- ^ Query the Firmware version installed
     QueryFirmwareE :: ArduinoProcedure (Expr Word16)                  -- ^ Query the Firmware version installed
     QueryProcessor :: ArduinoProcedure Processor                -- ^ Query the type of processor on 
     QueryProcessorE :: ArduinoProcedure (Expr Word8)
     Micros         :: ArduinoProcedure Word32
     MicrosE        :: ArduinoProcedure (Expr Word32)
     Millis         :: ArduinoProcedure Word32
     MillisE        :: ArduinoProcedure (Expr Word32)
     DelayMillis    :: TimeMillis -> ArduinoProcedure ()
     DelayMicros    :: TimeMicros -> ArduinoProcedure ()
     DelayMillisE   :: TimeMillisE -> ArduinoProcedure ()
     DelayMicrosE   :: TimeMicrosE -> ArduinoProcedure ()
     DigitalRead    :: Pin -> ArduinoProcedure Bool            -- ^ Read the avlue ona pin digitally
     DigitalReadE   :: PinE -> ArduinoProcedure (Expr Bool)         -- ^ Read the avlue ona pin digitally
     DigitalPortRead  :: Pin -> Word8 -> ArduinoProcedure Word8          -- ^ Read the values on a port digitally
     DigitalPortReadE :: PinE -> Expr Word8 -> ArduinoProcedure (Expr Word8)
     AnalogRead     :: Pin -> ArduinoProcedure Word16          -- ^ Read the analog value on a pin
     AnalogReadE    :: PinE -> ArduinoProcedure (Expr Word16)          
     I2CRead :: SlaveAddress -> Word8 -> ArduinoProcedure [Word8]
     I2CReadE :: SlaveAddressE -> Expr Word8 -> ArduinoProcedure (Expr [Word8])
     Stepper2Pin :: Word16 -> Pin -> Pin -> ArduinoProcedure Word8
     Stepper2PinE :: Expr Word16 -> PinE -> PinE -> ArduinoProcedure (Expr Word8)
     Stepper4Pin :: Word16 -> Pin -> Pin -> Pin -> Pin -> ArduinoProcedure Word8
     Stepper4PinE :: Expr Word16 -> PinE -> PinE -> PinE -> PinE -> ArduinoProcedure (Expr Word8)
     StepperStepE :: Expr Word8 -> Expr Int16 -> ArduinoProcedure ()
     ServoAttach :: Pin -> ArduinoProcedure Word8
     ServoAttachE :: PinE -> ArduinoProcedure (Expr Word8)
     ServoAttachMinMax :: Pin -> Int16 -> Int16 -> ArduinoProcedure Word8
     ServoAttachMinMaxE :: PinE -> Expr Int16 -> Expr Int16 -> ArduinoProcedure (Expr Word8)
     ServoRead :: Word8 -> ArduinoProcedure Int16
     ServoReadE :: Expr Word8 -> ArduinoProcedure (Expr Int16)
     ServoReadMicros :: Word8 -> ArduinoProcedure Int16
     ServoReadMicrosE :: Expr Word8 -> ArduinoProcedure (Expr Int16)
     QueryAllTasks  :: ArduinoProcedure [TaskID]
     QueryAllTasksE :: ArduinoProcedure (Expr [TaskID])
     QueryTask  :: TaskID -> ArduinoProcedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     QueryTaskE :: TaskIDE -> ArduinoProcedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     BootTaskE :: Expr [Word8] -> ArduinoProcedure (Expr Bool)
     ReadRemoteRefB  :: RemoteRef Bool   -> ArduinoProcedure (Expr Bool)
     ReadRemoteRefW8  :: RemoteRef Word8  -> ArduinoProcedure (Expr Word8)
     ReadRemoteRefW16 :: RemoteRef Word16 -> ArduinoProcedure (Expr Word16)
     ReadRemoteRefW32 :: RemoteRef Word32 -> ArduinoProcedure (Expr Word32)
     ReadRemoteRefI8  :: RemoteRef Int8  -> ArduinoProcedure (Expr Int8)
     ReadRemoteRefI16 :: RemoteRef Int16 -> ArduinoProcedure (Expr Int16)
     ReadRemoteRefI32 :: RemoteRef Int32 -> ArduinoProcedure (Expr Int32)
     ReadRemoteRefL8 :: RemoteRef [Word8] -> ArduinoProcedure (Expr [Word8])
     ReadRemoteRefFloat :: RemoteRef Float -> ArduinoProcedure (Expr Float)
     NewRemoteRefB    :: Expr Bool   -> ArduinoProcedure (RemoteRef Bool)
     NewRemoteRefW8   :: Expr Word8  -> ArduinoProcedure (RemoteRef Word8)
     NewRemoteRefW16  :: Expr Word16 -> ArduinoProcedure (RemoteRef Word16)
     NewRemoteRefW32  :: Expr Word32 -> ArduinoProcedure (RemoteRef Word32)
     NewRemoteRefI8   :: Expr Int8  -> ArduinoProcedure (RemoteRef Int8)
     NewRemoteRefI16  :: Expr Int16 -> ArduinoProcedure (RemoteRef Int16)
     NewRemoteRefI32  :: Expr Int32 -> ArduinoProcedure (RemoteRef Int32)
     NewRemoteRefL8   :: Expr [Word8] -> ArduinoProcedure (RemoteRef [Word8])
     NewRemoteRefFloat :: Expr Float -> ArduinoProcedure (RemoteRef Float)
     LiftIO           :: IO a -> ArduinoProcedure a
     Debug            :: String -> ArduinoProcedure ()
     DebugE           :: Expr [Word8] -> ArduinoProcedure ()
     Die              :: String -> [String] -> ArduinoProcedure ()
     -- ToDo: add SPI procedures

-- deriving instance Show a => Show (Procedure a)

queryFirmware :: Arduino Word16
queryFirmware = Arduino $ procedure QueryFirmware

queryFirmwareE :: Arduino (Expr Word16)
queryFirmwareE = Arduino $ procedure QueryFirmwareE

queryProcessor :: Arduino Processor
queryProcessor = Arduino $ procedure QueryProcessor

queryProcessorE :: Arduino (Expr Word8)
queryProcessorE = Arduino $ procedure QueryProcessorE

micros :: Arduino Word32
micros = Arduino $ procedure Micros

microsE :: Arduino (Expr Word32)
microsE = Arduino $ procedure MicrosE

millis :: Arduino Word32
millis = Arduino $ procedure Millis

millisE :: Arduino (Expr Word32)
millisE = Arduino $ procedure MillisE

delayMillis :: TimeMillis -> Arduino ()
delayMillis t = Arduino $ procedure $ DelayMillis t

delayMillisE :: TimeMillisE -> Arduino ()
delayMillisE t = Arduino $ procedure $ DelayMillisE t

delayMicros :: TimeMicros -> Arduino ()
delayMicros t = Arduino $ procedure $ DelayMicros t

delayMicrosE :: TimeMicrosE -> Arduino ()
delayMicrosE t = Arduino $ procedure $ DelayMicrosE t

digitalRead :: Pin -> Arduino Bool
digitalRead p = Arduino $ procedure $ DigitalRead p

digitalReadE :: PinE -> Arduino (Expr Bool)
digitalReadE p = Arduino $ procedure $ DigitalReadE p

digitalPortRead :: Pin -> Word8 -> Arduino Word8
digitalPortRead p m = Arduino $ procedure $ DigitalPortRead p m

digitalPortReadE :: PinE -> Expr Word8 -> Arduino (Expr Word8)
digitalPortReadE p m = Arduino $ procedure $ DigitalPortReadE p m

analogRead :: Pin -> Arduino Word16
analogRead p = Arduino $ procedure $ AnalogRead p

analogReadE :: PinE -> Arduino (Expr Word16)
analogReadE p = Arduino $ procedure $ AnalogReadE p

i2cRead :: SlaveAddress -> Word8 -> Arduino [Word8]
i2cRead sa cnt = Arduino $ procedure $ I2CRead sa cnt

i2cReadE :: SlaveAddressE -> Expr Word8 -> Arduino (Expr [Word8])
i2cReadE sa cnt = Arduino $ procedure $ I2CReadE sa cnt

stepper2Pin :: Word16 -> Pin -> Pin -> Arduino Word8
stepper2Pin s p1 p2 = Arduino $ procedure $ Stepper2Pin s p1 p2

stepper2PinE :: Expr Word16 -> PinE -> PinE -> Arduino (Expr Word8)
stepper2PinE s p1 p2 = Arduino $ procedure $ Stepper2PinE s p1 p2

stepper4Pin :: Word16 -> Pin -> Pin -> Pin -> Pin -> Arduino Word8
stepper4Pin s p1 p2 p3 p4 = Arduino $ procedure $ Stepper4Pin s p1 p2 p3 p4

stepper4PinE :: Expr Word16 -> PinE -> PinE -> PinE -> PinE -> Arduino (Expr Word8)
stepper4PinE s p1 p2 p3 p4 = Arduino $ procedure $ Stepper4PinE s p1 p2 p3 p4

stepperStep :: Word8 -> Int16 -> Arduino ()
stepperStep st s = Arduino $ procedure $ StepperStepE (lit st) (lit s)

stepperStepE :: Expr Word8 -> Expr Int16 -> Arduino ()
stepperStepE st s = Arduino $ procedure $ StepperStepE st s

servoAttach :: Pin -> Arduino Word8
servoAttach p = Arduino $ procedure $ ServoAttach p

servoAttachE :: PinE -> Arduino (Expr Word8)
servoAttachE p = Arduino $ procedure $ ServoAttachE p

servoAttachMixMax :: Pin -> Int16 -> Int16 -> Arduino Word8
servoAttachMixMax p min max = Arduino $ procedure $ ServoAttachMinMax p min max

servoAttachMixMaxE :: PinE -> Expr Int16 -> Expr Int16 -> Arduino (Expr Word8)
servoAttachMixMaxE p min max = Arduino $ procedure $ ServoAttachMinMaxE p min max

servoRead :: Word8 -> Arduino Int16
servoRead s = Arduino $ procedure $ ServoRead s

servoReadE :: Expr Word8 -> Arduino (Expr Int16)
servoReadE s = Arduino $ procedure $ ServoReadE s

servoReadMicros :: Word8 -> Arduino Int16
servoReadMicros s = Arduino $ procedure $ ServoReadMicros s

servoReadMicrosE :: Expr Word8 -> Arduino (Expr Int16)
servoReadMicrosE s = Arduino $ procedure $ ServoReadMicrosE s

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Arduino $ procedure QueryAllTasks

queryAllTasksE :: Arduino (Expr [TaskID])
queryAllTasksE = Arduino $ procedure QueryAllTasksE

queryTask :: TaskID -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTask tid = Arduino $ procedure $ QueryTask tid

queryTaskE :: TaskIDE -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTaskE tid = Arduino $ procedure $ QueryTaskE tid

bootTaskE :: Expr [Word8] -> Arduino (Expr Bool)
bootTaskE tids = Arduino $ procedure $ BootTaskE tids

readRemoteRefB :: RemoteRef Bool -> Arduino (Expr Bool)
readRemoteRefB n = Arduino $ procedure $ ReadRemoteRefB n

readRemoteRefW8 :: RemoteRef Word8 -> Arduino (Expr Word8)
readRemoteRefW8 n = Arduino $ procedure $ ReadRemoteRefW8 n

readRemoteRefW16 :: RemoteRef Word16 -> Arduino (Expr Word16)
readRemoteRefW16 n = Arduino $ procedure $ ReadRemoteRefW16 n

readRemoteRefW32 :: RemoteRef Word32 -> Arduino (Expr Word32)
readRemoteRefW32 n = Arduino $ procedure $ ReadRemoteRefW32 n

readRemoteRefI8 :: RemoteRef Int8 -> Arduino (Expr Int8)
readRemoteRefI8 n = Arduino $ procedure $ ReadRemoteRefI8 n

readRemoteRefI16 :: RemoteRef Int16 -> Arduino (Expr Int16)
readRemoteRefI16 n = Arduino $ procedure $ ReadRemoteRefI16 n

readRemoteRefI32 :: RemoteRef Int32 -> Arduino (Expr Int32)
readRemoteRefI32 n = Arduino $ procedure $ ReadRemoteRefI32 n

readRemoteRefL8 :: RemoteRef [Word8] -> Arduino (Expr [Word8])
readRemoteRefL8 n = Arduino $ procedure $ ReadRemoteRefL8 n

readRemoteRefFloat :: RemoteRef Float -> Arduino (Expr Float)
readRemoteRefFloat n = Arduino $ procedure $ ReadRemoteRefFloat n

newRemoteRefB :: Expr Bool -> Arduino (RemoteRef Bool)
newRemoteRefB n = Arduino $ procedure $ NewRemoteRefB n

newRemoteRefW8 :: Expr Word8 -> Arduino (RemoteRef Word8)
newRemoteRefW8 n = Arduino $ procedure $ NewRemoteRefW8 n

newRemoteRefW16 :: Expr Word16 -> Arduino (RemoteRef Word16)
newRemoteRefW16 n = Arduino $ procedure $ NewRemoteRefW16 n

newRemoteRefW32 :: Expr Word32 -> Arduino (RemoteRef Word32)
newRemoteRefW32 n = Arduino $ procedure $ NewRemoteRefW32 n

newRemoteRefI8 :: Expr Int8 -> Arduino (RemoteRef Int8)
newRemoteRefI8 n = Arduino $ procedure $ NewRemoteRefI8 n

newRemoteRefI16 :: Expr Int16 -> Arduino (RemoteRef Int16)
newRemoteRefI16 n = Arduino $ procedure $ NewRemoteRefI16 n

newRemoteRefI32 :: Expr Int32 -> Arduino (RemoteRef Int32)
newRemoteRefI32 n = Arduino $ procedure $ NewRemoteRefI32 n

newRemoteRefL8 :: Expr [Word8] -> Arduino (RemoteRef [Word8])
newRemoteRefL8 n = Arduino $ procedure $ NewRemoteRefL8 n

newRemoteRefFloat :: Expr Float -> Arduino (RemoteRef Float)
newRemoteRefFloat n = Arduino $ procedure $ NewRemoteRefFloat n

debug :: String -> Arduino ()
debug msg = Arduino $ procedure $ Debug msg

debugE :: Expr [Word8] -> Arduino ()
debugE msg = Arduino $ procedure $ DebugE msg

die :: String -> [String] -> Arduino ()
die msg msgs = Arduino $ procedure $ Die msg msgs

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
                 | BC_CMD_LOOP
                 | BC_CMD_WHILE
                 | BC_CMD_IF_THEN_ELSE
                 | BC_CMD_FORIN
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
                 | UNKNOWN_COMMAND
                deriving Show

-- | Compute the numeric value of a command
firmwareCmdVal :: FirmwareCmd -> Word8
firmwareCmdVal BC_CMD_SYSTEM_RESET      = 0x10
firmwareCmdVal BC_CMD_SET_PIN_MODE      = 0x11
firmwareCmdVal BC_CMD_DELAY_MILLIS      = 0x12
firmwareCmdVal BC_CMD_DELAY_MICROS      = 0x13
firmwareCmdVal BC_CMD_WHILE             = 0x14
firmwareCmdVal BC_CMD_IF_THEN_ELSE      = 0x15
firmwareCmdVal BC_CMD_LOOP              = 0x16
firmwareCmdVal BC_CMD_FORIN             = 0x17
firmwareCmdVal BS_CMD_REQUEST_VERSION   = 0x20
firmwareCmdVal BS_CMD_REQUEST_TYPE      = 0x21
firmwareCmdVal BS_CMD_REQUEST_MILLIS    = 0x22
firmwareCmdVal BS_CMD_REQUEST_MICROS    = 0x23
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

-- | Compute the numeric value of a command
firmwareValCmd :: Word8 -> FirmwareCmd
firmwareValCmd 0x10 = BC_CMD_SYSTEM_RESET    
firmwareValCmd 0x11 = BC_CMD_SET_PIN_MODE    
firmwareValCmd 0x12 = BC_CMD_DELAY_MILLIS    
firmwareValCmd 0x13 = BC_CMD_DELAY_MICROS    
firmwareValCmd 0x14 = BC_CMD_WHILE           
firmwareValCmd 0x15 = BC_CMD_IF_THEN_ELSE    
firmwareValCmd 0x16 = BC_CMD_LOOP            
firmwareValCmd 0x17 = BC_CMD_FORIN           
firmwareValCmd 0x20 = BS_CMD_REQUEST_VERSION 
firmwareValCmd 0x21 = BS_CMD_REQUEST_TYPE    
firmwareValCmd 0x22 = BS_CMD_REQUEST_MILLIS  
firmwareValCmd 0x23 = BS_CMD_REQUEST_MICROS  
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
firmwareValCmd _    = UNKNOWN_COMMAND        

data RefType = REF_BOOL
             | REF_WORD8
             | REF_WORD16
             | REF_WORD32
             | REF_INT8
             | REF_INT16
             | REF_INT32
             | REF_LIST8
             | REF_FLOAT
            deriving (Show, Enum)

-- | Firmware replies, see: 
-- | https://github.com/ku-fpg/haskino/wiki/Haskino-Firmware-Protocol-Definition
data FirmwareReply =  BC_RESP_DELAY
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
                deriving Show

getFirmwareReply :: Word8 -> Either Word8 FirmwareReply
getFirmwareReply 0x18 = Right BC_RESP_DELAY
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
