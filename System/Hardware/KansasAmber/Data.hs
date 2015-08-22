-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Data
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
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module System.Hardware.KansasAmber.Data where

import           Control.Applicative
import           Control.Concurrent (Chan, MVar, ThreadId, withMVar, modifyMVar, 
                                     modifyMVar_, putMVar, takeMVar, readMVar,
                                     newEmptyMVar)
import           Control.Monad (ap, liftM2, when, unless, void)
import           Control.Monad.Trans

import           Data.Bits ((.|.), (.&.), setBit)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           Data.Monoid
import           Data.Word (Word8, Word16, Word32)

import           System.Hardware.Serialport (SerialPort)

import           System.Hardware.KansasAmber.Expr
import           System.Hardware.KansasAmber.Utils

import Debug.Trace
-----------------------------------------------------------------------------

data Arduino :: * -> * where
    Control        :: Control                         -> Arduino ()
    Command        :: Command                         -> Arduino ()
    Local          :: Local a                         -> Arduino a
    Procedure      :: Procedure a                     -> Arduino a
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

type Pin = Word8

-- | Bailing out: print the given string on stdout and die
runDie :: ArduinoConnection -> String -> [String] -> IO a
runDie c m ms = do 
    let f = bailOut c
    f m ms

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

type SlaveAddress = Word8
type SlaveRegister = Word16
type MinPulse = Word16
type MaxPulse = Word16
type TaskLength = Word16
type TaskID = Word8
type TimeMillis = Word32
type TimeMicros = Word32
type TaskPos = Word16
-- ToDo: Readd Stepper types

data Command =
       SystemReset                              -- ^ Send system reset
     | SetPinMode Pin PinMode                   -- ^ Set the mode on a pin
--     | DigitalPortWrite Port Word8              -- ^ Set the values on a port digitally
--     | DigitalPortWriteE Port (Expr Word8)
     | DigitalWrite Pin Bool                    -- ^ Set the value on a pin digitally
--     | DigitalWriteE Pin (Expr Bool)              
     | AnalogWrite Pin Word16                   -- ^ Send an analog-write; used for servo control
     | Tone Pin Word16 (Maybe Word32)           -- ^ Play a tone on a pin
     | NoTone Pin                               -- ^ Stop playing a tone on a pin
--     | AnalogWriteE Pin (Expr Word16)
     | I2CWrite SlaveAddress [Word8]
--     | I2CWriteE SlaveAddress (Expr [Word8])
     | I2CConfig
     | CreateTask TaskID (Arduino ())
     | DeleteTask TaskID
     | DelayMillis TimeMillis
     | DelayMicros TimeMicros
--     | DelayE (Expr TaskTime)
     | ScheduleTask TaskID TimeMillis
     | ScheduleReset
     -- ToDo: add one wire and encoder procedures, readd stepper and servo

systemReset :: Arduino ()
systemReset = Command SystemReset

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm = Command $ SetPinMode p pm

-- digitalPortWrite :: Port -> Word8 -> Arduino ()
-- digitalPortWrite p w = Command $ DigitalPortWrite p w

-- digitalPortWriteE :: Port -> (Expr Word8) -> Arduino ()
-- digitalPortWriteE p w = Command $ DigitalPortWriteE p w

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p b = Command $ DigitalWrite p b

-- digitalWriteE :: Pin -> (Expr Bool) -> Arduino ()
-- digitalWriteE p b = Command $ DigitalWriteE p b

analogWrite :: Pin -> Word16 -> Arduino ()
analogWrite p w = Command $ AnalogWrite p w

tone :: Pin -> Word16 -> Maybe Word32 -> Arduino ()
tone p f d = Command $ Tone p f d

noTone :: Pin -> Arduino ()
noTone p = Command $ NoTone p

-- analogWriteE :: Pin -> (Expr Word16) -> Arduino ()
-- analogWriteE p w = Command $ AnalogWriteE p w

i2cWrite :: SlaveAddress -> [Word8] -> Arduino ()
i2cWrite sa ws = Command $ I2CWrite sa ws

-- i2cWriteE :: SlaveAddress -> (Expr [Word8]) -> Arduino ()
-- i2cWriteE sa ws = Command $ I2CWriteE sa ws

i2cConfig :: Arduino ()
i2cConfig = Command $ I2CConfig

-- i2cConfigE :: (Expr Word16) -> Arduino ()
-- i2cConfigE w = Command $ I2CConfigE w

-- ToDo: Readd servo and stepper functions

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Command (CreateTask tid ps)

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Command $ DeleteTask tid

delayMillis :: TimeMillis -> Arduino ()
delayMillis t = Command $ DelayMillis t

delayMicros :: TimeMicros -> Arduino ()
delayMicros t = Command $ DelayMicros t

-- delayE :: (Expr TaskTime) -> Arduino ()
-- delayE t = Command $ DelayE t

scheduleTask :: TaskID -> TimeMillis -> Arduino ()
scheduleTask tid tt = Command $ ScheduleTask tid tt

scheduleReset :: Arduino ()
scheduleReset = Command ScheduleReset

data Control =
     Loop (Arduino ())

loop :: Arduino () -> Arduino ()
loop ps = Control (Loop ps)

data Local :: * -> * where
     Debug            :: String -> Local ()
     Die              :: String -> [String] -> Local ()

deriving instance Show a => Show (Local a)

debug :: String -> Arduino ()
debug msg = Local $ Debug msg

die :: String -> [String] -> Arduino ()
die msg msgs = Local $ Die msg msgs

data Procedure :: * -> * where
     QueryFirmware  :: Procedure (Word8, Word8        )   -- ^ Query the Firmata version installed
     QueryProcessor :: Procedure Processor                -- ^ Query the type of processor on 
--     DigitalPortRead  :: Port -> Procedure Word8          -- ^ Read the values on a port digitally
--     DigitalPortReadE :: Port -> Procedure (Expr Word8)
     DigitalRead    :: Pin -> Procedure Bool            -- ^ Read the avlue ona pin digitally
--     DigitalReadE     :: Pin -> Procedure (Expr Bool)
     AnalogRead     :: Pin -> Procedure Word16          -- ^ Read the analog value on a pin
--     AnalogReadE      :: Pin -> Procedure (Expr Word16)          
     I2CRead :: SlaveAddress -> Word8 -> Procedure [Word8]
     QueryAllTasks :: Procedure [TaskID]
     QueryTask :: TaskID -> Procedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     -- Todo: add one wire queries, readd pulse?

deriving instance Show a => Show (Procedure a)

queryFirmware :: Arduino (Word8, Word8)
queryFirmware = Procedure QueryFirmware

queryProcessor :: Arduino Processor
queryProcessor = Procedure QueryProcessor

-- ToDo: Do some sort of analog mapping locally?

-- digitalPortRead :: Port -> Arduino Word8
-- digitalPortRead p = Procedure $ DigitalPortRead p

-- digitalPortReadE :: Port -> Arduino (Expr Word8)
-- digitalPortReadE p = Procedure $ DigitalPortReadE p

digitalRead :: Pin -> Arduino Bool
digitalRead p = Procedure $ DigitalRead p

-- digitalReadE :: Pin -> Arduino (Expr Bool)
-- digitalReadE p = Procedure $ DigitalReadE p

analogRead :: Pin -> Arduino Word16
analogRead p = Procedure $ AnalogRead p

-- analogReadE :: Pin -> Arduino (Expr Word16)
-- analogReadE p = Procedure $ AnalogReadE p

i2cRead :: SlaveAddress -> Word8 -> Arduino [Word8]
i2cRead sa cnt = Procedure $ I2CRead sa cnt

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Procedure QueryAllTasks

queryTask :: TaskID -> Arduino (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
queryTask tid = Procedure $ QueryTask tid

-- | A response, as returned from the Arduino
data Response = Firmware Word8 Word8                 -- ^ Firmware version (maj/min and indentifier
              | ProcessorType Word8                  -- ^ Processor report
              | MicrosReply Word32                   -- ^ Elapsed Microseconds
              | MillisReply Word32                   -- ^ Elapsed Milliseconds
              | DigitalReply Word8                   -- ^ Status of a pin
              | AnalogReply Word16                   -- ^ Status of an analog pin
              | StringMessage  String                -- ^ String message from Firmata
              | I2CReply [Word8]                     -- ^ Response to a I2C Read
              | QueryAllTasksReply [Word8]           -- ^ Response to Query All Tasks
              | QueryTaskReply (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
              | EmptyFrame
              | InvalidChecksumFrame [Word8]
    deriving Show

-- | Amber Firmware commands, see: http://tbd
data FirmwareCmd = BC_CMD_SET_PIN_MODE
                 | BC_CMD_DELAY_MILLIS
                 | BC_CMD_DELAY_MICROS
                 | BC_CMD_SYSTEM_RESET
                 | BS_CMD_REQUEST_VERSION
                 | BS_CMD_REQUEST_TYPE
                 | BS_CMD_REQUEST_MICROS
                 | BS_CMD_REQUEST_MILLIS
                 | DIG_CMD_READ_PIN
                 | DIG_CMD_WRITE_PIN
                 | ALG_CMD_READ_PIN
                 | ALG_CMD_WRITE_PIN
                 | ALG_CMD_TONE_PIN
                 | ALG_CMD_NOTONE_PIN
                 | I2C_CMD_CONFIG
                 | I2C_CMD_READ
                 | I2C_CMD_WRITE
                 | SCHED_CMD_CREATE_TASK
                 | SCHED_CMD_DELETE_TASK
                 | SCHED_CMD_ADD_TO_TASK
                 | SCHED_CMD_SCHED_TASK
                 | SCHED_CMD_QUERY
                 | SCHED_CMD_QUERY_ALL
                 | SCHED_CMD_RESET
                deriving Show

-- | Compute the numeric value of a command
firmwareCmdVal :: FirmwareCmd -> Word8
firmwareCmdVal BC_CMD_SET_PIN_MODE    = 0x10
firmwareCmdVal BC_CMD_DELAY_MILLIS    = 0x11
firmwareCmdVal BC_CMD_DELAY_MICROS    = 0x12
firmwareCmdVal BC_CMD_SYSTEM_RESET    = 0x13
firmwareCmdVal BS_CMD_REQUEST_VERSION = 0x20
firmwareCmdVal BS_CMD_REQUEST_TYPE    = 0x21
firmwareCmdVal BS_CMD_REQUEST_MILLIS  = 0x22
firmwareCmdVal DIG_CMD_READ_PIN       = 0x30
firmwareCmdVal DIG_CMD_WRITE_PIN      = 0x31
firmwareCmdVal ALG_CMD_READ_PIN       = 0x40
firmwareCmdVal ALG_CMD_WRITE_PIN      = 0x41
firmwareCmdVal ALG_CMD_TONE_PIN       = 0x42
firmwareCmdVal ALG_CMD_NOTONE_PIN     = 0x43
firmwareCmdVal I2C_CMD_CONFIG         = 0x50
firmwareCmdVal I2C_CMD_READ           = 0x51
firmwareCmdVal I2C_CMD_WRITE          = 0x52
firmwareCmdVal SCHED_CMD_CREATE_TASK  = 0xA0
firmwareCmdVal SCHED_CMD_DELETE_TASK  = 0xA1
firmwareCmdVal SCHED_CMD_ADD_TO_TASK  = 0xA2
firmwareCmdVal SCHED_CMD_SCHED_TASK   = 0xA3
firmwareCmdVal SCHED_CMD_QUERY        = 0xA4
firmwareCmdVal SCHED_CMD_QUERY_ALL    = 0xA5
firmwareCmdVal SCHED_CMD_RESET        = 0xA6

-- | Firmware replies, see: https:tbd
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
getFirmwareReply 0xA8 = Right SCHED_RESP_QUERY
getFirmwareReply 0xA9 = Right SCHED_RESP_QUERY_ALL
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
