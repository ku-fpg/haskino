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
             GeneralizedNewtypeDeriving, NamedFieldPuns, DataKinds, 
             TypeFamilies #-}

module System.Hardware.Haskino.Data where

import           Control.Applicative
import           Control.Concurrent (Chan, MVar, ThreadId, withMVar, modifyMVar, 
                                     modifyMVar_, putMVar, takeMVar, readMVar,
                                     newEmptyMVar)
import           Control.Monad (ap, liftM2, when, unless, void)
import           Control.Monad.Trans

import           Data.Bits ((.|.), (.&.), setBit, shiftL)
import           Data.Boolean as B
import           Data.Boolean.Numbers as BN
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           Data.Monoid
import           Data.Word (Word8, Word16, Word32)

import           System.Hardware.Serialport (SerialPort)

import           System.Hardware.Haskino.Utils

import Debug.Trace
-----------------------------------------------------------------------------

data Arduino :: * -> * where
    Control        :: Control                         -> Arduino ()
    Command        :: Command                         -> Arduino ()
    Local          :: Local a                         -> Arduino a
    Procedure      :: Procedure a                     -> Arduino a
    RemoteBinding  :: RemoteBinding a                 -> Arduino a
    LiftIO         :: IO a                            -> Arduino a
    Bind           :: Arduino a -> (a -> Arduino b)   -> Arduino b
    Return         :: a                               -> Arduino a

instance Monad Arduino where
        return = Return
        (>>=) = Bind

instance Applicative Arduino where
  pure  = return
  (<*>) = ap

instance Functor Arduino where
  fmap f c = c >>= return . f

instance Monoid a => Monoid (Arduino a) where
  mappend = liftM2 mappend
  mempty  = return mempty

instance MonadIO Arduino where
  liftIO m = LiftIO m

type Pin  = Word8
type PinE = Word8E

-- | Bailing out: print the given string on stdout and die
runDie :: ArduinoConnection -> String -> [String] -> IO a
runDie c m ms = do 
    let f = bailOut c
    f m ms

-- | The mode for a pin.
data PinMode = INPUT
             | OUTPUT
             | INPUT_PULLUP
        deriving (Eq, Show, Enum)

-- | LCD's connected to the board
data LCD = LCD {
                 lcdController     :: LCDController -- ^ Actual controller
               , lcdState          :: MVar LCDData  -- ^ State information    
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
                     , lcdRows     :: Int  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Int  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
    | I2CHitachi44780 {
                       address     :: Word8 -- ^ I2C Slave Address of LCD
                     , lcdRows     :: Int  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Int  -- ^ Number of cols (typically 16 or 20, upto 40)
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

-- | State of the connection
data ArduinoConnection = ArduinoConnection {
                message       :: String -> IO ()                      -- ^ Current debugging routine
              , bailOut       :: forall a. String -> [String] -> IO a -- ^ Clean-up and quit with a hopefully informative message
              , port          :: SerialPort                           -- ^ Serial port we are communicating on
              , firmwareID    :: String                               -- ^ The ID of the board (as identified by the Board itself)
              , deviceChannel :: Chan Response                        -- ^ Incoming messages from the board
              , processor     :: Processor                            -- ^ Type of processor on board
              , listenerTid   :: MVar ThreadId                        -- ^ ThreadId of the listener
              }

type VarMap = M.Map String Word8

type SlaveAddress = Word8
type SlaveAddressE = Word8E
type MinPulse = Word16
type MaxPulse = Word16
type TaskLength = Word16
type TaskID = Word8
type TaskIDE = Word8E
type TimeMillis = Word32
type TimeMillisE = Word32E
type TimeMicros = Word32
type TimeMicrosE = Word32E
type TaskPos = Word16
type VarSize = Word8
-- ToDo: Readd Stepper types

data Command =
       SystemReset                              -- ^ Send system reset
     | SetPinMode Pin PinMode                   -- ^ Set the mode on a pin
     | SetPinModeE PinE PinMode                 -- ^ Set the mode on a pin
--  ToDo: PinMode expression?
--     | DigitalPortWrite Port Word8            -- ^ Set the values on a port digitally
--     | DigitalPortWriteE Port (Expr Word8)
     | DigitalWrite Pin Bool                    -- ^ Set the value on a pin digitally
     | DigitalWriteE PinE BoolE              
     | AnalogWrite Pin Word16                   -- ^ Send an analog-write; used for servo control
     | AnalogWriteE PinE Word16E
     | Tone Pin Word16 (Maybe Word32)           -- ^ Play a tone on a pin
     | ToneE PinE Word16E (Maybe Word32E)       -- ^ Play a tone on a pin
     | NoTone Pin                               -- ^ Stop playing a tone on a pin
     | NoToneE PinE                             -- ^ Stop playing a tone on a pin
     | I2CWrite SlaveAddress [Word8]
     | I2CWriteE SlaveAddressE [Word8E]
     | I2CConfig
     | CreateTask TaskID (Arduino ())
     | CreateTaskE TaskIDE (Arduino ())
     | DeleteTask TaskID
     | DeleteTaskE TaskIDE
     | DelayMillis TimeMillis
     | DelayMicros TimeMicros
     | DelayMillisE TimeMillisE
     | DelayMicrosE TimeMicrosE
     | ScheduleTask TaskID TimeMillis
     | ScheduleTaskE TaskIDE TimeMillisE
     | ScheduleReset
     | WriteRemoteRefB (RemoteRef Bool) BoolE
     | WriteRemoteRef8 (RemoteRef Word8) Word8E
     | WriteRemoteRef16 (RemoteRef Word16) Word16E
     | WriteRemoteRef32 (RemoteRef Word32) Word32E
     | ModifyRemoteRefB (RemoteRef Bool) (BoolE -> BoolE)
     | ModifyRemoteRef8 (RemoteRef Word8) (Word8E -> Word8E)
     | ModifyRemoteRef16 (RemoteRef Word16) (Word16E -> Word16E)
     | ModifyRemoteRef32 (RemoteRef Word32) (Word32E -> Word32E)
     | While BoolE (Arduino ())
     | IfThenElse BoolE (Arduino ()) (Arduino ())
     -- ToDo: add one wire and encoder procedures, readd stepper and servo

systemReset :: Arduino ()
systemReset = Command SystemReset

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm = Command $ SetPinMode p pm

setPinModeE :: PinE -> PinMode -> Arduino ()
setPinModeE p pm = Command $ SetPinModeE p pm

-- digitalPortWrite :: Port -> Word8 -> Arduino ()
-- digitalPortWrite p w = Command $ DigitalPortWrite p w

-- digitalPortWriteE :: Port -> (Expr Word8) -> Arduino ()
-- digitalPortWriteE p w = Command $ DigitalPortWriteE p w

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p b = Command $ DigitalWrite p b

digitalWriteE :: PinE -> BoolE -> Arduino ()
digitalWriteE p b = Command $ DigitalWriteE p b

analogWrite :: Pin -> Word16 -> Arduino ()
analogWrite p w = Command $ AnalogWrite p w

analogWriteE :: PinE -> Word16E -> Arduino ()
analogWriteE p w = Command $ AnalogWriteE p w

tone :: Pin -> Word16 -> Maybe Word32 -> Arduino ()
tone p f d = Command $ Tone p f d

toneE :: PinE -> Word16E -> Maybe Word32E -> Arduino ()
toneE p f d = Command $ ToneE p f d

noTone :: Pin -> Arduino ()
noTone p = Command $ NoTone p

noToneE :: PinE -> Arduino ()
noToneE p = Command $ NoToneE p

i2cWrite :: SlaveAddress -> [Word8] -> Arduino ()
i2cWrite sa ws = Command $ I2CWrite sa ws

i2cWriteE :: SlaveAddressE -> [Word8E] -> Arduino ()
i2cWriteE sa ws = Command $ I2CWriteE sa ws

i2cConfig :: Arduino ()
i2cConfig = Command $ I2CConfig

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Command (CreateTask tid ps)

createTaskE :: TaskIDE -> Arduino () -> Arduino ()
createTaskE tid ps = Command (CreateTaskE tid ps)

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Command $ DeleteTask tid

deleteTaskE :: TaskIDE -> Arduino ()
deleteTaskE tid = Command $ DeleteTaskE tid

delayMillis :: TimeMillis -> Arduino ()
delayMillis t = Command $ DelayMillis t

delayMillisE :: TimeMillisE -> Arduino ()
delayMillisE t = Command $ DelayMillisE t

delayMicros :: TimeMicros -> Arduino ()
delayMicros t = Command $ DelayMicros t

delayMicrosE :: TimeMicrosE -> Arduino ()
delayMicrosE t = Command $ DelayMicrosE t

scheduleTask :: TaskID -> TimeMillis -> Arduino ()
scheduleTask tid tt = Command $ ScheduleTask tid tt

scheduleTaskE :: TaskIDE -> TimeMillisE -> Arduino ()
scheduleTaskE tid tt = Command $ ScheduleTaskE tid tt

scheduleReset :: Arduino ()
scheduleReset = Command ScheduleReset

while :: BoolE -> Arduino () -> Arduino()
while be ps = Command $ While be ps

ifThenElse :: BoolE -> Arduino () -> Arduino() -> Arduino()
ifThenElse be tps eps = Command $ IfThenElse be tps eps

writeRemoteRefB :: RemoteRef Bool -> BoolE -> Arduino ()
writeRemoteRefB r e = Command $ WriteRemoteRefB r e

writeRemoteRef8 :: RemoteRef Word8 -> Word8E -> Arduino ()
writeRemoteRef8 r e = Command $ WriteRemoteRef8 r e

writeRemoteRef16 :: RemoteRef Word16 -> Word16E -> Arduino ()
writeRemoteRef16 r e = Command $ WriteRemoteRef16 r e

writeRemoteRef32 :: RemoteRef Word32 -> Word32E -> Arduino ()
writeRemoteRef32 r e = Command $ WriteRemoteRef32 r e

modifyRemoteRefB :: RemoteRef Bool -> (BoolE -> BoolE) -> Arduino ()
modifyRemoteRefB r f = Command $ ModifyRemoteRefB r f

modifyRemoteRef8 :: RemoteRef Word8 -> (Word8E -> Word8E) -> Arduino ()
modifyRemoteRef8 r f = Command $ ModifyRemoteRef8 r f

modifyRemoteRef16 :: RemoteRef Word16 -> (Word16E -> Word16E) -> Arduino ()
modifyRemoteRef16 r f = Command $ ModifyRemoteRef16 r f

modifyRemoteRef32 :: RemoteRef Word32 -> (Word32E -> Word32E) -> Arduino ()
modifyRemoteRef32 r f = Command $ ModifyRemoteRef32 r f

-- ToDo: Readd servo and stepper functions

class RemoteReference a where
    newRemoteRef    :: Expr a -> Arduino (RemoteRef a)
    readRemoteRef   :: RemoteRef a -> Arduino (Expr a)
    writeRemoteRef  :: RemoteRef a -> Expr a -> Arduino ()
    modifyRemoteRef :: RemoteRef a -> (Expr a -> Expr a) -> Arduino ()

instance RemoteReference Bool where
    newRemoteRef = newRemoteRefB
    readRemoteRef = readRemoteRefB
    writeRemoteRef = writeRemoteRefB
    modifyRemoteRef = modifyRemoteRefB

instance RemoteReference Word8 where
    newRemoteRef = newRemoteRef8
    readRemoteRef = readRemoteRef8
    writeRemoteRef = writeRemoteRef8
    modifyRemoteRef = modifyRemoteRef8

instance RemoteReference Word16 where
    newRemoteRef = newRemoteRef16
    readRemoteRef = readRemoteRef16
    writeRemoteRef = writeRemoteRef16
    modifyRemoteRef = modifyRemoteRef16

instance RemoteReference Word32 where
    newRemoteRef = newRemoteRef32
    readRemoteRef = readRemoteRef32
    writeRemoteRef = writeRemoteRef32
    modifyRemoteRef = modifyRemoteRef32

data Control =
      Loop (Arduino ())

loop :: Arduino () -> Arduino ()
loop ps = Control $ Loop ps

data Local :: * -> * where
     Debug            :: String -> Local ()
     Die              :: String -> [String] -> Local ()

deriving instance Show a => Show (Local a)

debug :: String -> Arduino ()
debug msg = Local $ Debug msg

die :: String -> [String] -> Arduino ()
die msg msgs = Local $ Die msg msgs

data Procedure :: * -> * where
     QueryFirmware  :: Procedure Word16                   -- ^ Query the Firmware version installed
     QueryFirmwareE :: Procedure Word16E                  -- ^ Query the Firmware version installed
     QueryProcessor :: Procedure Processor                -- ^ Query the type of processor on 
-- ToDo: E version of QueryProcessor, handle Expr Processor
     Micros         :: Procedure Word32
     MicrosE        :: Procedure Word32E
     Millis         :: Procedure Word32
     MillisE        :: Procedure Word32E
--     DigitalPortRead  :: Port -> Procedure Word8          -- ^ Read the values on a port digitally
--     DigitalPortReadE :: Port -> Procedure (Expr Word8)
     DigitalRead    :: Pin -> Procedure Bool            -- ^ Read the avlue ona pin digitally
     DigitalReadE   :: PinE -> Procedure BoolE         -- ^ Read the avlue ona pin digitally
     AnalogRead     :: Pin -> Procedure Word16          -- ^ Read the analog value on a pin
     AnalogReadE    :: PinE -> Procedure Word16E          
     I2CRead :: SlaveAddress -> Word8 -> Procedure [Word8]
     I2CReadE :: SlaveAddressE -> Word8E -> Procedure [Word8]
     QueryAllTasks :: Procedure [TaskID]
-- ToDo: E version of QueryProcessor, handle Expr [Word8]
     QueryTask  :: TaskID -> Procedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     QueryTaskE :: TaskIDE -> Procedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     -- Todo: add one wire queries, readd pulse?
     ReadRemoteRefB  :: RemoteRef Bool   -> Procedure BoolE
     ReadRemoteRef8  :: RemoteRef Word8  -> Procedure Word8E
     ReadRemoteRef16 :: RemoteRef Word16 -> Procedure Word16E
     ReadRemoteRef32 :: RemoteRef Word32 -> Procedure Word32E

queryFirmware :: Arduino Word16
queryFirmware = Procedure QueryFirmware

queryFirmwareE :: Arduino Word16E
queryFirmwareE = Procedure QueryFirmwareE

queryProcessor :: Arduino Processor
queryProcessor = Procedure QueryProcessor

micros :: Arduino Word32
micros = Procedure Micros

microsE :: Arduino Word32E
microsE = Procedure MicrosE

millis :: Arduino Word32
millis = Procedure Millis

millisE :: Arduino Word32E
millisE = Procedure MillisE

-- ToDo: Do some sort of analog mapping locally?

-- digitalPortRead :: Port -> Arduino Word8
-- digitalPortRead p = Procedure $ DigitalPortRead p

-- digitalPortReadE :: Port -> Arduino (Expr Word8)
-- digitalPortReadE p = Procedure $ DigitalPortReadE p

digitalRead :: Pin -> Arduino Bool
digitalRead p = Procedure $ DigitalRead p

digitalReadE :: PinE -> Arduino BoolE
digitalReadE p = Procedure $ DigitalReadE p

analogRead :: Pin -> Arduino Word16
analogRead p = Procedure $ AnalogRead p

analogReadE :: PinE -> Arduino Word16E
analogReadE p = Procedure $ AnalogReadE p

i2cRead :: SlaveAddress -> Word8 -> Arduino [Word8]
i2cRead sa cnt = Procedure $ I2CRead sa cnt

i2cReadE :: SlaveAddressE -> Word8E -> Arduino [Word8]
i2cReadE sa cnt = Procedure $ I2CReadE sa cnt

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Procedure QueryAllTasks

queryTask :: TaskID -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTask tid = Procedure $ QueryTask tid

queryTaskE :: TaskIDE -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTaskE tid = Procedure $ QueryTaskE tid

readRemoteRefB :: RemoteRef Bool -> Arduino BoolE
readRemoteRefB n = Procedure $ ReadRemoteRefB n

readRemoteRef8 :: RemoteRef Word8 -> Arduino Word8E
readRemoteRef8 n = Procedure $ ReadRemoteRef8 n

readRemoteRef16 :: RemoteRef Word16 -> Arduino Word16E
readRemoteRef16 n = Procedure $ ReadRemoteRef16 n

readRemoteRef32 :: RemoteRef Word32 -> Arduino Word32E
readRemoteRef32 n = Procedure $ ReadRemoteRef32 n

data RemoteBinding :: * -> * where
     NewRemoteRefB    :: BoolE   -> RemoteBinding (RemoteRef Bool)
     NewRemoteRef8    :: Word8E  -> RemoteBinding (RemoteRef Word8)
     NewRemoteRef16   :: Word16E -> RemoteBinding (RemoteRef Word16)
     NewRemoteRef32   :: Word32E -> RemoteBinding (RemoteRef Word32)

newRemoteRefB :: BoolE -> Arduino (RemoteRef Bool)
newRemoteRefB n = RemoteBinding $ NewRemoteRefB n

newRemoteRef8 :: Word8E -> Arduino (RemoteRef Word8)
newRemoteRef8 n = RemoteBinding $ NewRemoteRef8 n

newRemoteRef16 :: Word16E -> Arduino (RemoteRef Word16)
newRemoteRef16 n = RemoteBinding $ NewRemoteRef16 n

newRemoteRef32 :: Word32E -> Arduino (RemoteRef Word32)
newRemoteRef32 n = RemoteBinding $ NewRemoteRef32 n

-- | A response, as returned from the Arduino
data Response = Firmware Word16                      -- ^ Firmware version (maj/min)
              | ProcessorType Word8                  -- ^ Processor report
              | MicrosReply Word32                   -- ^ Elapsed Microseconds
              | MillisReply Word32                   -- ^ Elapsed Milliseconds
              | DigitalReply Word8                   -- ^ Status of a pin
              | AnalogReply Word16                   -- ^ Status of an analog pin
              | StringMessage  String                -- ^ String message from Firmware
              | I2CReply [Word8]                     -- ^ Response to a I2C Read
              | QueryAllTasksReply [Word8]           -- ^ Response to Query All Tasks
              | QueryTaskReply (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
              | NewReply Word8
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
              | EmptyFrame
              | InvalidChecksumFrame [Word8]
    deriving Show

-- | Haskino Firmware commands, see: 
-- | https://github.com/ku-fpg/haskino/wiki/Haskino-Firmware-Protocol-Definition
data FirmwareCmd = BC_CMD_SET_PIN_MODE
                 | BC_CMD_DELAY_MILLIS
                 | BC_CMD_DELAY_MICROS
                 | BC_CMD_SET_PIN_MODE_E
                 | BC_CMD_DELAY_MILLIS_E
                 | BC_CMD_DELAY_MICROS_E
                 | BC_CMD_SYSTEM_RESET
                 | BC_CMD_WHILE
                 | BC_CMD_IF_THEN_ELSE
                 | BS_CMD_REQUEST_VERSION
                 | BS_CMD_REQUEST_TYPE
                 | BS_CMD_REQUEST_MICROS
                 | BS_CMD_REQUEST_MILLIS
                 | BS_CMD_REQUEST_VERSION_E
                 | BS_CMD_REQUEST_TYPE_E
                 | BS_CMD_REQUEST_MICROS_E
                 | BS_CMD_REQUEST_MILLIS_E
                 | DIG_CMD_READ_PIN
                 | DIG_CMD_WRITE_PIN
                 | DIG_CMD_READ_PIN_E
                 | DIG_CMD_WRITE_PIN_E
                 | ALG_CMD_READ_PIN
                 | ALG_CMD_WRITE_PIN
                 | ALG_CMD_TONE_PIN
                 | ALG_CMD_NOTONE_PIN
                 | ALG_CMD_READ_PIN_E
                 | ALG_CMD_WRITE_PIN_E
                 | ALG_CMD_TONE_PIN_E
                 | ALG_CMD_NOTONE_PIN_E
                 | I2C_CMD_CONFIG
                 | I2C_CMD_READ
                 | I2C_CMD_WRITE
                 | I2C_CMD_READ_E
                 | I2C_CMD_WRITE_E
                 | SCHED_CMD_CREATE_TASK
                 | SCHED_CMD_DELETE_TASK
                 | SCHED_CMD_ADD_TO_TASK
                 | SCHED_CMD_SCHED_TASK
                 | SCHED_CMD_QUERY
                 | SCHED_CMD_QUERY_ALL
                 | SCHED_CMD_RESET
                 | SCHED_CMD_DELETE_TASK_E
                 | SCHED_CMD_SCHED_TASK_E
                 | SCHED_CMD_QUERY_E
                 | SCHED_CMD_QUERY_ALL_E
                 | REF_CMD_NEW_B
                 | REF_CMD_NEW_8
                 | REF_CMD_NEW_16
                 | REF_CMD_NEW_32
                 | REF_CMD_READ_B
                 | REF_CMD_READ_8
                 | REF_CMD_READ_16
                 | REF_CMD_READ_32
                 | REF_CMD_WRITE_B
                 | REF_CMD_WRITE_8
                 | REF_CMD_WRITE_16
                 | REF_CMD_WRITE_32
                 | REF_CMD_MOD_B
                 | REF_CMD_MOD_8
                 | REF_CMD_MOD_16
                 | REF_CMD_MOD_32
                deriving Show

-- | Compute the numeric value of a command
firmwareCmdVal :: FirmwareCmd -> Word8
firmwareCmdVal BC_CMD_SET_PIN_MODE      = 0x10
firmwareCmdVal BC_CMD_DELAY_MILLIS      = 0x11
firmwareCmdVal BC_CMD_DELAY_MICROS      = 0x12
firmwareCmdVal BC_CMD_SYSTEM_RESET      = 0x13
firmwareCmdVal BC_CMD_SET_PIN_MODE_E    = 0x14
firmwareCmdVal BC_CMD_DELAY_MILLIS_E    = 0x15
firmwareCmdVal BC_CMD_DELAY_MICROS_E    = 0x16
firmwareCmdVal BC_CMD_WHILE             = 0x17
firmwareCmdVal BC_CMD_IF_THEN_ELSE      = 0x18
firmwareCmdVal BS_CMD_REQUEST_VERSION   = 0x20
firmwareCmdVal BS_CMD_REQUEST_TYPE      = 0x21
firmwareCmdVal BS_CMD_REQUEST_MILLIS    = 0x22
firmwareCmdVal BS_CMD_REQUEST_MICROS    = 0x23
firmwareCmdVal BS_CMD_REQUEST_VERSION_E = 0x24
firmwareCmdVal BS_CMD_REQUEST_TYPE_E    = 0x25
firmwareCmdVal BS_CMD_REQUEST_MILLIS_E  = 0x26
firmwareCmdVal BS_CMD_REQUEST_MICROS_E  = 0x27
firmwareCmdVal DIG_CMD_READ_PIN         = 0x30
firmwareCmdVal DIG_CMD_WRITE_PIN        = 0x31
firmwareCmdVal DIG_CMD_READ_PIN_E       = 0x32
firmwareCmdVal DIG_CMD_WRITE_PIN_E      = 0x33
firmwareCmdVal ALG_CMD_READ_PIN         = 0x40
firmwareCmdVal ALG_CMD_WRITE_PIN        = 0x41
firmwareCmdVal ALG_CMD_TONE_PIN         = 0x42
firmwareCmdVal ALG_CMD_NOTONE_PIN       = 0x43
firmwareCmdVal ALG_CMD_READ_PIN_E       = 0x44
firmwareCmdVal ALG_CMD_WRITE_PIN_E      = 0x45
firmwareCmdVal ALG_CMD_TONE_PIN_E       = 0x46
firmwareCmdVal ALG_CMD_NOTONE_PIN_E     = 0x47
firmwareCmdVal I2C_CMD_CONFIG           = 0x50
firmwareCmdVal I2C_CMD_READ             = 0x51
firmwareCmdVal I2C_CMD_WRITE            = 0x52
firmwareCmdVal I2C_CMD_READ_E           = 0x53
firmwareCmdVal I2C_CMD_WRITE_E          = 0x54
firmwareCmdVal SCHED_CMD_CREATE_TASK    = 0xA0
firmwareCmdVal SCHED_CMD_DELETE_TASK    = 0xA1
firmwareCmdVal SCHED_CMD_ADD_TO_TASK    = 0xA2
firmwareCmdVal SCHED_CMD_SCHED_TASK     = 0xA3
firmwareCmdVal SCHED_CMD_QUERY          = 0xA4
firmwareCmdVal SCHED_CMD_QUERY_ALL      = 0xA5
firmwareCmdVal SCHED_CMD_RESET          = 0xA6
firmwareCmdVal SCHED_CMD_DELETE_TASK_E  = 0xA7
firmwareCmdVal SCHED_CMD_SCHED_TASK_E   = 0xA8
firmwareCmdVal SCHED_CMD_QUERY_E        = 0xA9
firmwareCmdVal SCHED_CMD_QUERY_ALL_E    = 0xAA
firmwareCmdVal REF_CMD_NEW_B            = 0xB0
firmwareCmdVal REF_CMD_NEW_8            = 0xB1
firmwareCmdVal REF_CMD_NEW_16           = 0xB2
firmwareCmdVal REF_CMD_NEW_32           = 0xB3
firmwareCmdVal REF_CMD_READ_B           = 0xB4
firmwareCmdVal REF_CMD_READ_8           = 0xB5
firmwareCmdVal REF_CMD_READ_16          = 0xB6
firmwareCmdVal REF_CMD_READ_32          = 0xB7
firmwareCmdVal REF_CMD_WRITE_B          = 0xB8
firmwareCmdVal REF_CMD_WRITE_8          = 0xB9
firmwareCmdVal REF_CMD_WRITE_16         = 0xBA
firmwareCmdVal REF_CMD_WRITE_32         = 0xBB
firmwareCmdVal REF_CMD_MOD_B            = 0xBC
firmwareCmdVal REF_CMD_MOD_8            = 0xBD
firmwareCmdVal REF_CMD_MOD_16           = 0xBE
firmwareCmdVal REF_CMD_MOD_32           = 0xBF

-- | Firmware replies, see: 
-- | https://github.com/ku-fpg/haskino/wiki/Haskino-Firmware-Protocol-Definition
data FirmwareReply =  BS_RESP_VERSION
                   |  BS_RESP_TYPE
                   |  BS_RESP_MICROS
                   |  BS_RESP_MILLIS
                   |  BS_RESP_STRING
                   |  DIG_RESP_READ_PIN
                   |  ALG_RESP_READ_PIN
                   |  I2C_RESP_READ
                   |  SCHED_RESP_QUERY
                   |  SCHED_RESP_QUERY_ALL
                   |  REF_RESP_NEW
                deriving Show

getFirmwareReply :: Word8 -> Either Word8 FirmwareReply
getFirmwareReply 0x28 = Right BS_RESP_VERSION
getFirmwareReply 0x29 = Right BS_RESP_TYPE
getFirmwareReply 0x2A = Right BS_RESP_MICROS
getFirmwareReply 0x2B = Right BS_RESP_MILLIS
getFirmwareReply 0x2C = Right BS_RESP_STRING
getFirmwareReply 0x38 = Right DIG_RESP_READ_PIN
getFirmwareReply 0x48 = Right ALG_RESP_READ_PIN
getFirmwareReply 0x58 = Right I2C_RESP_READ
getFirmwareReply 0xAB = Right SCHED_RESP_QUERY
getFirmwareReply 0xAC = Right SCHED_RESP_QUERY_ALL
getFirmwareReply 0xC0 = Right REF_RESP_NEW
getFirmwareReply n    = Left n

--stepDelayVal :: StepDelay -> Word8
--stepDelayVal OneUs = 0x00
--stepDelayVal TwoUs = 0x08

--stepDirVal :: StepDir -> Word8
--stepDirVal CW = 0x00
--stepDirVal CCW = 0x01

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
               | UNKNOWN_PROCESSOR Word8
    deriving Show

getProcessor :: Word8 -> Processor
getProcessor  0 = ATMEGA8
getProcessor  1 = ATMEGA168
getProcessor  2 = ATMEGA328P
getProcessor  3 = ATMEGA1280
getProcessor  4 = ATMEGA256
getProcessor  5 = ATMEGA32U4
getProcessor  6 = ATMEGA644P
getProcessor  7 = ATMEGA644
getProcessor  8 = ATMEGA645
getProcessor  9 = SAM3X8E
getProcessor 10 = X86
getProcessor  n = UNKNOWN_PROCESSOR n

data RemoteRef a where
    RemoteRefB   :: Int -> RemoteRef Bool
    RemoteRefW8  :: Int -> RemoteRef Word8
    RemoteRefW16 :: Int -> RemoteRef Word16
    RemoteRefW32 :: Int -> RemoteRef Word32

-- Expression definitions

remoteRefToExpr :: RemoteRef a -> Expr a
remoteRefToExpr r = case r of
                         RemoteRefB i -> RefB i
                         RemoteRefW8 i -> Ref8 i
                         RemoteRefW16 i -> Ref16 i
                         RemoteRefW32 i -> Ref32 i

deriving instance Show a => Show (RemoteRef a)

type BoolE   = Expr Bool
type Word8E  = Expr Word8
type Word16E = Expr Word16
type Word32E = Expr Word32
type StringE = Expr String

data Expr a where
  LitB      :: Bool -> BoolE
  Lit8      :: Word8 -> Word8E
  Lit16     :: Word16 -> Word16E
  Lit32     :: Word32 -> Word32E
  RefB      :: Int -> BoolE
  Ref8      :: Int -> Word8E
  Ref16     :: Int -> Word16E
  Ref32     :: Int -> Word32E
  NotB      :: BoolE -> BoolE
  AndB      :: BoolE -> BoolE -> BoolE
  OrB       :: BoolE -> BoolE -> BoolE
  Neg8      :: Word8E -> Word8E
  Sign8     :: Word8E -> Word8E
  Add8      :: Word8E -> Word8E -> Word8E
  Sub8      :: Word8E -> Word8E -> Word8E
  Mult8     :: Word8E -> Word8E -> Word8E
  Div8      :: Word8E -> Word8E -> Word8E
  Rem8      :: Word8E -> Word8E -> Word8E
  And8      :: Word8E -> Word8E -> Word8E
  Or8       :: Word8E -> Word8E -> Word8E
  Xor8      :: Word8E -> Word8E -> Word8E
  Comp8     :: Word8E -> Word8E
  ShfL8     :: Word8E -> Word8E -> Word8E
  ShfR8     :: Word8E -> Word8E -> Word8E
  Eq8       :: Word8E -> Word8E -> BoolE
  Less8     :: Word8E -> Word8E -> BoolE
  If8       :: BoolE -> Word8E -> Word8E -> Word8E
  Neg16     :: Word16E -> Word16E
  Sign16    :: Word16E -> Word16E
  Add16     :: Word16E -> Word16E -> Word16E
  Sub16     :: Word16E -> Word16E -> Word16E
  Mult16    :: Word16E -> Word16E -> Word16E
  Div16     :: Word16E -> Word16E -> Word16E
  Rem16     :: Word16E -> Word16E -> Word16E
  And16     :: Word16E -> Word16E -> Word16E
  Or16      :: Word16E -> Word16E -> Word16E
  Xor16     :: Word16E -> Word16E -> Word16E
  Comp16    :: Word16E -> Word16E
  ShfL16    :: Word16E -> Word8E -> Word16E
  ShfR16    :: Word16E -> Word8E -> Word16E
  Eq16      :: Word16E -> Word16E -> BoolE
  Less16    :: Word16E -> Word16E -> BoolE
  If16      :: BoolE -> Word16E -> Word16E -> Word16E
  Neg32     :: Word32E -> Word32E
  Sign32    :: Word32E -> Word32E
  Add32     :: Word32E -> Word32E -> Word32E
  Sub32     :: Word32E -> Word32E -> Word32E
  Mult32    :: Word32E -> Word32E -> Word32E
  Div32     :: Word32E -> Word32E -> Word32E
  Rem32     :: Word32E -> Word32E -> Word32E
  And32     :: Word32E -> Word32E -> Word32E
  Or32      :: Word32E -> Word32E -> Word32E
  Xor32     :: Word32E -> Word32E -> Word32E
  Comp32    :: Word32E -> Word32E
  ShfL32    :: Word32E -> Word8E -> Word32E
  ShfR32    :: Word32E -> Word8E -> Word32E
  Eq32      :: Word32E -> Word32E -> BoolE
  Less32    :: Word32E -> Word32E -> BoolE
  If32      :: BoolE -> Word32E -> Word32E -> Word32E

deriving instance Show a => Show (Expr a)

class LiteralB a where
    lit  :: a -> Expr a

instance LiteralB Word8 where
    lit = Lit8

instance LiteralB Word16 where
    lit = Lit16

instance LiteralB Word32 where
    lit = Lit32

instance LiteralB Bool where
    lit = LitB

-- ToDo:  Add BitsB class for and, or, xor, complement and shifts
-- ToDo:  Add fromInteger/toInteger properly to do typing on Arduino

instance B.Boolean BoolE where
  true  = LitB True
  false = LitB False
  notB  = NotB
  (&&*) = AndB
  (||*) = OrB

instance Num Word8E where
  (+) x y = Add8 x y
  (-) x y = Sub8 x y
  (*) x y = Mult8 x y
  negate x = Neg8 x
  abs x  = x
  signum x = Sign8 x
  fromInteger x = Lit8 $ fromInteger x

type instance BooleanOf Word8E = BoolE

instance B.EqB Word8E where
  (==*) = Eq8

instance B.OrdB Word8E where
  (<*) = Less8

instance B.IfB Word8E where
  ifB = If8

instance BN.NumB Word8E where
  type IntegerOf Word8E = Word8
  fromIntegerB x = Lit8 x

instance BN.IntegralB Word8E where
  div = Div8
  rem = Rem8
  quot = Div8
  mod = Rem8
  toIntegerB x = case x of
                      Lit8 n -> n

instance  Num Word16E where
  (+) x y = Add16 x y
  (-) x y = Sub16 x y
  (*) x y = Mult16 x y
  negate x = Neg16 x
  abs x  = x
  signum x = Sign16 x
  fromInteger x = Lit16 $ fromInteger x

type instance BooleanOf Word16E = BoolE

instance B.EqB Word16E where
  (==*) = Eq16

instance B.OrdB Word16E where
  (<*) = Less16

instance B.IfB Word16E where
  ifB = If16

instance BN.NumB Word16E where
  type IntegerOf Word16E = Word16
  fromIntegerB x = Lit16 x

instance BN.IntegralB Word16E where
  div = Div16
  rem = Rem16
  quot = Div16
  mod = Rem16
  toIntegerB x = case x of
                    Lit16 n -> n

instance  Num Word32E where
  (+) x y = Add32 x y
  (-) x y = Sub32 x y
  (*) x y = Mult32 x y
  negate x = Neg32 x
  abs x  = x
  signum x = Sign32 x
  fromInteger x = Lit32 $ fromInteger x

type instance BooleanOf Word32E = BoolE

instance B.EqB Word32E where
  (==*) = Eq32

instance B.OrdB Word32E where
  (<*) = Less32

instance B.IfB Word32E where
  ifB = If32

instance BN.NumB Word32E where
  type IntegerOf Word32E = Word32
  fromIntegerB x = Lit32 x

instance BN.IntegralB Word32E where
  div = Div32
  rem = Rem32
  quot = Div32
  mod = Rem32
  toIntegerB x = case x of
                    Lit32 n -> n

-- | Haskino Firmware expresions, see:tbd 
data ExprType = EXPR_BOOL
              | EXPR_WORD8
              | EXPR_WORD16
              | EXPR_WORD32

data ExprOp = EXPR_LIT
            | EXPR_REF
            | EXPR_PROC
            | EXPR_NOT
            | EXPR_AND
            | EXPR_OR
            | EXPR_XOR
            | EXPR_NEG
            | EXPR_SIGN
            | EXPR_ADD
            | EXPR_SUB
            | EXPR_MULT
            | EXPR_DIV
            | EXPR_REM
            | EXPR_COMP
            | EXPR_SHFL
            | EXPR_SHFR
            | EXPR_EQ
            | EXPR_LESS
            | EXPR_IF

-- | Compute the numeric value of a command
exprTypeVal :: ExprType -> Word8
exprTypeVal EXPR_BOOL   = 0x01
exprTypeVal EXPR_WORD8  = 0x02
exprTypeVal EXPR_WORD16 = 0x03
exprTypeVal EXPR_WORD32 = 0x04

exprOpVal :: ExprOp -> Word8
exprOpVal EXPR_LIT  = 0x00
exprOpVal EXPR_REF  = 0x01
exprOpVal EXPR_PROC = 0x02
exprOpVal EXPR_NOT  = 0x03
exprOpVal EXPR_AND  = 0x04
exprOpVal EXPR_OR   = 0x05
exprOpVal EXPR_XOR  = 0x06
exprOpVal EXPR_NEG  = 0x07
exprOpVal EXPR_SIGN = 0x08
exprOpVal EXPR_ADD  = 0x09
exprOpVal EXPR_SUB  = 0x0A
exprOpVal EXPR_MULT = 0x0B
exprOpVal EXPR_DIV  = 0x0C
exprOpVal EXPR_REM  = 0x0D
exprOpVal EXPR_COMP = 0x0E
exprOpVal EXPR_SHFL = 0x0F
exprOpVal EXPR_SHFR = 0x10
exprOpVal EXPR_EQ   = 0x11
exprOpVal EXPR_LESS = 0x12
exprOpVal EXPR_IF   = 0x13

exprCmdVal :: ExprType -> ExprOp -> Word8
exprCmdVal t o = exprTypeVal t `shiftL` 5 .|. exprOpVal o
