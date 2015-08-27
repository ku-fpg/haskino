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

import           System.Hardware.Haskino.Expr
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
              , variables     :: MVar VarMap
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
     | AssignProcB BoolE (Arduino Bool)
     | AssignExprB BoolE BoolE
     | AssignProc8 Word8E (Arduino Word8)
     | AssignExpr8 Word8E Word8E
     | AssignProc16 Word16E (Arduino Word16)
     | AssignExpr16 Word16E Word16E
     | AssignProc32 Word32E (Arduino Word32)
     | AssignExpr32 Word32E Word32E
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

-- ToDo: Readd servo and stepper functions

infix 1 =*,=**

class Assign a where
    (=*)  :: Expr a -> Expr a -> Arduino()
    (=**) :: Expr a -> Arduino a -> Arduino() 

instance Assign Bool where
    (=*)  s e  = Command $ AssignExprB s e
    (=**) s pe = Command $ AssignProcB s pe

instance Assign Word8 where
    (=*)  s e  = Command $ AssignExpr8 s e
    (=**) s pe = Command $ AssignProc8 s pe

instance Assign Word16 where
    (=*)  s e  = Command $ AssignExpr16 s e
    (=**) s pe = Command $ AssignProc16 s pe

instance Assign Word32 where
    (=*)  s e  = Command $ AssignExpr32 s e
    (=**) s pe = Command $ AssignProc32 s pe

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
     QueryFirmware  :: Procedure (Word8, Word8        )   -- ^ Query the Firmata version installed
     QueryProcessor :: Procedure Processor                -- ^ Query the type of processor on 
     Micros         :: Procedure Word32
     Millis         :: Procedure Word32
--     DigitalPortRead  :: Port -> Procedure Word8          -- ^ Read the values on a port digitally
--     DigitalPortReadE :: Port -> Procedure (Expr Word8)
     DigitalRead    :: Pin -> Procedure Bool            -- ^ Read the avlue ona pin digitally
     DigitalReadE   :: PinE -> Procedure Bool          -- ^ Read the avlue ona pin digitally
     AnalogRead     :: Pin -> Procedure Word16          -- ^ Read the analog value on a pin
     AnalogReadE    :: PinE -> Procedure Word16          
     I2CRead :: SlaveAddress -> Word8 -> Procedure [Word8]
     I2CReadE :: SlaveAddressE -> Word8E -> Procedure [Word8]
     QueryAllTasks :: Procedure [TaskID]
     QueryTask  :: TaskID -> Procedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     QueryTaskE :: TaskIDE -> Procedure (Maybe (TaskLength, TaskLength, TaskPos, TimeMillis))
     -- Todo: add one wire queries, readd pulse?

deriving instance Show a => Show (Procedure a)

queryFirmware :: Arduino (Word8, Word8)
queryFirmware = Procedure QueryFirmware

queryProcessor :: Arduino Processor
queryProcessor = Procedure QueryProcessor

micros :: Arduino Word32
micros = Procedure Micros

millis :: Arduino Word32
millis = Procedure Millis

-- ToDo: Do some sort of analog mapping locally?

-- digitalPortRead :: Port -> Arduino Word8
-- digitalPortRead p = Procedure $ DigitalPortRead p

-- digitalPortReadE :: Port -> Arduino (Expr Word8)
-- digitalPortReadE p = Procedure $ DigitalPortReadE p

digitalRead :: Pin -> Arduino Bool
digitalRead p = Procedure $ DigitalRead p

digitalReadE :: PinE -> Arduino Bool
digitalReadE p = Procedure $ DigitalReadE p

analogRead :: Pin -> Arduino Word16
analogRead p = Procedure $ AnalogRead p

analogReadE :: PinE -> Arduino Word16
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

data RemoteBinding :: * -> * where
     NewVarB       :: String  -> RemoteBinding BoolE
     NewVar8       :: String  -> RemoteBinding Word8E
     NewVar16      :: String  -> RemoteBinding Word16E
     NewVar32      :: String  -> RemoteBinding Word32E

newVarB :: String -> Arduino BoolE
newVarB n = RemoteBinding $ NewVarB n

newVar8 :: String -> Arduino Word8E
newVar8 n = RemoteBinding $ NewVar8 n

newVar16 :: String -> Arduino Word16E
newVar16 n = RemoteBinding $ NewVar16 n

newVar32 :: String -> Arduino Word32E
newVar32 n = RemoteBinding $ NewVar32 n

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
              | NewReply Word8
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
              | EmptyFrame
              | InvalidChecksumFrame [Word8]
    deriving Show

-- | Amber Firmware commands, see: 
-- | https://github.com/ku-fpg/kansas-amber/wiki/Amber-Firmware-Protocol-Definition
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
                 | VAR_CMD_NEW
                 | VAR_CMD_ASGN_PROCB
                 | VAR_CMD_ASGN_EXPRB
                 | VAR_CMD_ASGN_PROC8
                 | VAR_CMD_ASGN_EXPR8
                 | VAR_CMD_ASGN_PROC16
                 | VAR_CMD_ASGN_EXPR16
                 | VAR_CMD_ASGN_PROC32
                 | VAR_CMD_ASGN_EXPR32
                deriving Show

-- | Compute the numeric value of a command
firmwareCmdVal :: FirmwareCmd -> Word8
firmwareCmdVal BC_CMD_SET_PIN_MODE    = 0x10
firmwareCmdVal BC_CMD_DELAY_MILLIS    = 0x11
firmwareCmdVal BC_CMD_DELAY_MICROS    = 0x12
firmwareCmdVal BC_CMD_SYSTEM_RESET    = 0x13
firmwareCmdVal BC_CMD_SET_PIN_MODE_E  = 0x14
firmwareCmdVal BC_CMD_DELAY_MILLIS_E  = 0x15
firmwareCmdVal BC_CMD_DELAY_MICROS_E  = 0x16
firmwareCmdVal BC_CMD_WHILE           = 0x17
firmwareCmdVal BC_CMD_IF_THEN_ELSE    = 0x18
firmwareCmdVal BS_CMD_REQUEST_VERSION = 0x20
firmwareCmdVal BS_CMD_REQUEST_TYPE    = 0x21
firmwareCmdVal BS_CMD_REQUEST_MILLIS  = 0x22
firmwareCmdVal DIG_CMD_READ_PIN       = 0x30
firmwareCmdVal DIG_CMD_WRITE_PIN      = 0x31
firmwareCmdVal DIG_CMD_READ_PIN_E     = 0x32
firmwareCmdVal DIG_CMD_WRITE_PIN_E    = 0x33
firmwareCmdVal ALG_CMD_READ_PIN       = 0x40
firmwareCmdVal ALG_CMD_WRITE_PIN      = 0x41
firmwareCmdVal ALG_CMD_TONE_PIN       = 0x42
firmwareCmdVal ALG_CMD_NOTONE_PIN     = 0x43
firmwareCmdVal ALG_CMD_READ_PIN_E     = 0x44
firmwareCmdVal ALG_CMD_WRITE_PIN_E    = 0x45
firmwareCmdVal ALG_CMD_TONE_PIN_E     = 0x46
firmwareCmdVal ALG_CMD_NOTONE_PIN_E   = 0x47
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
firmwareCmdVal SCHED_CMD_DELETE_TASK_E = 0xA7
firmwareCmdVal SCHED_CMD_SCHED_TASK_E  = 0xA8
firmwareCmdVal SCHED_CMD_QUERY_E       = 0xA9
firmwareCmdVal VAR_CMD_NEW            = 0xB0
firmwareCmdVal VAR_CMD_ASGN_PROCB     = 0xB1
firmwareCmdVal VAR_CMD_ASGN_EXPRB     = 0xB2
firmwareCmdVal VAR_CMD_ASGN_PROC8     = 0xB3
firmwareCmdVal VAR_CMD_ASGN_EXPR8     = 0xB4
firmwareCmdVal VAR_CMD_ASGN_PROC16    = 0xB5
firmwareCmdVal VAR_CMD_ASGN_EXPR16    = 0xB6
firmwareCmdVal VAR_CMD_ASGN_PROC32    = 0xB7
firmwareCmdVal VAR_CMD_ASGN_EXPR32    = 0xB8

-- | Firmware replies, see: 
-- | https://github.com/ku-fpg/kansas-amber/wiki/Amber-Firmware-Protocol-Definition
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
                   |  VAR_RESP_NEW
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
getFirmwareReply 0xBC = Right VAR_RESP_NEW
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
