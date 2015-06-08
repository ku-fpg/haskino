{-# LANGUAGE FlexibleInstances, GADTs, KindSignatures, RankNTypes,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving,
             GeneralizedNewtypeDeriving, NamedFieldPuns #-}

module System.Hardware.DeepArduino.Data where

import           Control.Applicative
import           Control.Concurrent (Chan, MVar, ThreadId)
import           Control.Monad (ap, liftM, liftM2)
import           Control.Monad.State (StateT, MonadIO, MonadState, gets, liftIO)

import           Data.Bits ((.|.), (.&.))
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import           Data.Monoid
import           Data.Word (Word8, Word16, Word32)

import           System.Hardware.Serialport (SerialPort)

import           System.Hardware.DeepArduino.Utils

-----------------------------------------------------------------------------

data Arduino :: * -> * where
    Procedure      :: Procedure                       -> Arduino ()
    Local          :: Local a                         -> Arduino a
    Query          :: Query a                         -> Arduino a
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

-- | A pin on the Arduino, as specified by the user via 'pin', 'digital', and 'analog' functions.
data Pin = DigitalPin {userPinNo :: Word8}
         | AnalogPin  {userPinNo :: Word8}
         | MixedPin   {userPinNo :: Word8}
         deriving Show

-- | A pin on the Arduino, as viewed by the library; i.e., real-pin numbers
data IPin = InternalPin { pinNo :: Word8 }
          deriving (Eq, Ord, Show)

-- | A port (containing 8 pins)
data Port = Port { portNo :: Word8 } 
          deriving (Eq, Ord, Show)

-- | On the arduino, digital pin numbers are in 1-to-1 match with
-- the board pins. However, ANALOG pins come at an offset, determined by
-- the capabilities query. Users of the library refer to these pins
-- simply by their natural numbers, which makes for portable programs
-- between boards that have different number of digital pins. We adjust
-- for this shift here.
getInternalPin :: ArduinoConnection -> Pin -> IPin
getInternalPin c (MixedPin p)   = InternalPin p
getInternalPin c (DigitalPin p) = InternalPin p
getInternalPin c (AnalogPin p) = InternalPin p
-- TBD Fix me
{-
getInternalPin c (AnalogPin p)
  = do BoardCapabilities caps <- boardCapabilities c
       case listToMaybe [realPin | (realPin, PinCapabilities{analogPinNumber = Just n}) <- M.toAscList caps, p == n] of
         Nothing -> die ("DeepArduino: " ++ show p ++ " is not a valid analog-pin on this board.")
                        -- Try to be helpful in case they are trying to use a large value thinking it needs to be offset
                        ["Hint: To refer to analog pin number k, simply use 'pin k', not 'pin (k+noOfDigitalPins)'" | p > 13]
         Just rp -> return rp
-}

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which port it belongs to
pinPort :: IPin -> Port
pinPort p = Port (pinNo p `quot` 8)

-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which index it belongs to in its port
pinPortIndex :: IPin -> Word8
pinPortIndex p = pinNo p `rem` 8

-- | The mode for a pin.
data PinMode = INPUT    -- ^ Digital input
             | OUTPUT   -- ^ Digital output
             | ANALOG   -- ^ Analog input
             | PWM      -- ^ PWM (Pulse-Width-Modulation) output 
             | SERVO    -- ^ Servo Motor controller
             | SHIFT    -- ^ Shift controller
             | I2C      -- ^ I2C (Inter-Integrated-Circuit) connection
             | ONEWIRE  -- ^ pin configured for 1-wire
             | STEPPER  -- ^ pin configured for stepper motor
             | ENCODER  -- ^ pin configured for encoders
             deriving (Eq, Show, Enum)

-- | Resolution, as referred to in http://firmata.org/wiki/Protocol#Capability_Query
-- TODO: Not quite sure how this is used, so merely keep it as a Word8 now
type Resolution = Word8

-- | Capabilities of a pin
data PinCapabilities  = PinCapabilities {
                          analogPinNumber :: Maybe Word8              -- ^ Analog pin number, if any
                        , allowedModes    :: [(PinMode, Resolution)]  -- ^ Allowed modes and resolutions
                        }
    deriving Show

-- | Data associated with a pin
data PinData = PinData {
                 pinMode  :: PinMode
               , pinValue :: Maybe (Either Bool Int)
               }
               deriving Show

-- | What the board is capable of and current settings
newtype BoardCapabilities = BoardCapabilities (M.Map IPin PinCapabilities)

instance Show BoardCapabilities where
  show (BoardCapabilities m) = intercalate "\n" (map sh (M.toAscList m))
    where sh (p, PinCapabilities{analogPinNumber, allowedModes}) = show p ++ sep ++ unwords [show md | (md, _) <- allowedModes]
             where sep = maybe ": " (\i -> "[A" ++ show i ++ "]: ") analogPinNumber

type SlaveAddress = Word16
type SlaveRegister = Word16
type MinPulse = Word16
type MaxPulse = Word16
type TaskLength = Word16
type TaskID = Word8
type TaskTime = Word32
type TaskPos = Word16
data I2CAddrMode = Bit7 | Bit10
      deriving Show

data Procedure =
       SystemReset                              -- ^ Send system reset
     | SetPinMode Pin PinMode                  -- ^ Set the mode on a pin
     | DigitalReport Port Bool                  -- ^ Digital report values on port enable/disable
     | AnalogReport Pin Bool                   -- ^ Analog report values on pin enable/disable
     | DigitalPortWrite Port Word8 Word8        -- ^ Set the values on a port digitally
     | DigitalPinWrite Pin Bool                -- ^ Set the value on a pin digitally
     | AnalogPinWrite Pin Word8 Word8          -- ^ Send an analog-write; used for servo control
     | AnalogPinExtendedWrite Pin [Word8]      -- ^ 
     | SamplingInterval Word8 Word8             -- ^ Set the sampling interval
     | I2CWrite I2CAddrMode SlaveAddress [Word16]
     | I2CConfig Word16
     -- TBD add I2C continuous read
     | ServoConfig Pin MinPulse MaxPulse
     -- TBD add stepper procedures
     | CreateTask TaskID (Arduino ())
     | DeleteTask TaskID
     | DelayTask TaskTime
     | ScheduleTask TaskID TaskTime
     | ScheduleReset

systemReset :: Arduino ()
systemReset = Procedure SystemReset

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm = Procedure (SetPinMode p pm)

digitalReport :: Port -> Bool -> Arduino ()
digitalReport p b = Procedure (DigitalReport p b)

analogReport :: Pin -> Bool -> Arduino ()
analogReport p b = Procedure (AnalogReport p b)

digitalPortWrite :: Port -> Word16 -> Arduino ()
digitalPortWrite p w = Procedure (DigitalPortWrite p w1 w2)
  where
    [w1, w2] = word16ToArduinoBytes w

digitalPinWrite :: Pin -> Bool -> Arduino ()
digitalPinWrite p b = Procedure (DigitalPinWrite p b)

analogPinWrite :: Pin -> Word16 -> Arduino ()
analogPinWrite p w = Procedure (AnalogPinWrite p w1 w2)
  where
    [w1, w2] = word16ToArduinoBytes w

analogPinExtendedWrite :: Pin -> [Word8] -> Arduino ()
analogPinExtendedWrite p ws = Procedure (AnalogPinExtendedWrite p ws)

samplingInterval :: Word8 -> Word8 -> Arduino ()
samplingInterval w1 w2 = Procedure (SamplingInterval w1 w2)

i2cWrite :: I2CAddrMode -> SlaveAddress -> [Word16] -> Arduino ()
i2cWrite m sa ws = Procedure (I2CWrite m sa ws)

i2cConfig :: Word16 -> Arduino ()
i2cConfig w = Procedure (I2CConfig w)

servoConfig :: Pin -> MinPulse -> MaxPulse -> Arduino ()
servoConfig p min max = Procedure (ServoConfig p min max)

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Procedure (DeleteTask tid)

delayTask :: TaskTime -> Arduino ()
delayTask t = Procedure (DelayTask t)

scheduleTask :: TaskID -> TaskTime -> Arduino ()
scheduleTask tid tt = Procedure (ScheduleTask tid tt)

scheduleReset :: Arduino ()
scheduleReset = Procedure ScheduleReset

data Local :: * -> * where
     DigitalPortRead  :: Port -> Local Word8          -- ^ Read the values on a port digitally
     DigitalPinRead   :: IPin -> Local Bool           -- ^ Read the avlue ona pin digitally
     AnalogPinRead    :: IPin -> Local Word8          -- ^ Read the analog value on a pin
     HostDelay        :: Int  -> Local (IO ())

digitalPortRead :: Port -> Arduino Word8
digitalPortRead p = Local (DigitalPortRead p)

digitalPinRead :: IPin -> Arduino Bool
digitalPinRead p = Local (DigitalPinRead p)

analogPinRead :: IPin -> Arduino Word8
analogPinRead p = Local (AnalogPinRead p)

hostDelay :: Int -> Arduino (IO ())
hostDelay d = Local (HostDelay d)

anaPinRead :: IPin -> Word8
anaPinRead _ = 4 :: Word8

digPinRead :: IPin -> Bool
digPinRead _ = True

digPortRead :: Port -> Word8
digPortRead _ = 10 :: Word8

deriving instance Show a => Show (Local a)

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Procedure (CreateTask tid ps)

data Query :: * -> * where
     QueryFirmware  :: Query (Word8, Word8, String)   -- ^ Query the Firmata version installed
     CapabilityQuery :: Query BoardCapabilities       -- ^ Query the capabilities of the board
     AnalogMappingQuery :: Query [Word8]              -- ^ Query the mapping of analog pins
     Pulse :: IPin -> Bool -> Word32 -> Word32 -> Query Word32 -- ^ Request for a pulse reading on a pin, value, duration, timeout
     I2CRead :: I2CAddrMode -> SlaveAddress -> Maybe SlaveRegister -> Query [Word16]
     QueryAllTasks :: Query [TaskID]
     QueryTask :: TaskID -> Query (TaskID, TaskTime, TaskLength, TaskPos, [Word8])

deriving instance Show a => Show (Query a)

queryFirmware :: Arduino (Word8, Word8, String)
queryFirmware = Query QueryFirmware

capabilityQuery :: Arduino BoardCapabilities
capabilityQuery = Query CapabilityQuery

analogMappingQuery :: Arduino [Word8]
analogMappingQuery = Query AnalogMappingQuery

pulse :: IPin -> Bool -> Word32 -> Word32 -> Arduino Word32
pulse p b w1 w2 = Query (Pulse p b w1 w2)

i2cRead :: I2CAddrMode -> SlaveAddress -> Maybe SlaveRegister -> Arduino [Word16]
i2cRead am sa sr = Query (I2CRead am sa sr)

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Query QueryAllTasks

queryTask :: TaskID -> Arduino (TaskID, TaskTime, TaskLength, TaskPos, [Word8])
queryTask tid = Query (QueryTask tid)

-- | A response, as returned from the Arduino
data Response = Firmware Word8 Word8 String          -- ^ Firmware version (maj/min and indentifier
              | Capabilities BoardCapabilities       -- ^ Capabilities report
              | AnalogMapping [Word8]                -- ^ Analog pin mappings
              | DigitalMessage Port Word8 Word8      -- ^ Status of a port
              | AnalogMessage  IPin Word8 Word8      -- ^ Status of an analog pin
              | StringMessage  String                -- ^ String message from Firmata
              | PulseResponse  IPin Word32           -- ^ Repsonse to a PulseInCommand
              | I2CReply Word16 Word16 [Word16]      -- ^ Response to a I2C Read
              | QueryAllTasksReply [Word8]           -- ^ Response to Query All Tasks
              | QueryTaskReply TaskID TaskTime TaskLength TaskPos [Word8]
              | ErrorTaskReply TaskTime TaskLength TaskPos [Word8]
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
    deriving Show

-- | Firmata commands, see: http://firmata.org/wiki/Protocol#Message_Types
data FirmataCmd = ANALOG_MESSAGE      IPin -- ^ @0xE0@ pin
                | DIGITAL_MESSAGE     Port -- ^ @0x90@ port
                | REPORT_ANALOG_PIN   IPin -- ^ @0xC0@ pin
                | REPORT_DIGITAL_PORT Port -- ^ @0xD0@ port
                | START_SYSEX              -- ^ @0xF0@
                | SET_PIN_MODE             -- ^ @0xF4@
                | SET_DIGITAL_PIN_VALUE    -- ^ @0xF5@
                | END_SYSEX                -- ^ @0xF7@
                | PROTOCOL_VERSION         -- ^ @0xF9@
                | SYSTEM_RESET             -- ^ @0xFF@
                deriving Show

-- | Compute the numeric value of a command
firmataCmdVal :: FirmataCmd -> Word8
firmataCmdVal (ANALOG_MESSAGE      p) = 0xE0 .|. pinNo  p
firmataCmdVal (DIGITAL_MESSAGE     p) = 0x90 .|. portNo p
firmataCmdVal (REPORT_ANALOG_PIN   p) = 0xC0 .|. pinNo  p
firmataCmdVal (REPORT_DIGITAL_PORT p) = 0xD0 .|. portNo p
firmataCmdVal START_SYSEX             = 0xF0
firmataCmdVal SET_PIN_MODE            = 0xF4
firmataCmdVal SET_DIGITAL_PIN_VALUE   = 0xF5
firmataCmdVal END_SYSEX               = 0xF7
firmataCmdVal PROTOCOL_VERSION        = 0xF9
firmataCmdVal SYSTEM_RESET            = 0xFF

-- | Compute the numeric value of a mode
firmataI2CModeVal :: I2CAddrMode -> Word8
firmataI2CModeVal Bit7            = 0x00
firmataI2CModeVal Bit10           = 0x20

-- | Firmata scheduler commands, see: https://github.com/firmata/protocol/blob/master/scheduler.md
data SchedulerCmd = CREATE_TASK    -- ^ @0x00@
                | DELETE_TASK      -- ^ @0x01@
                | ADD_TO_TASK      -- ^ @0x02@
                | DELAY_TASK       -- ^ @0x03@
                | SCHEDULE_TASK    -- ^ @0x04@
                | QUERY_ALL_TASKS  -- ^ @0x05@
                | QUERY_TASK       -- ^ @0x06@
                | SCHEDULER_RESET  -- ^ @0x07@

-- | Compute the numeric value of a scheduler command
schedulerCmdVal :: SchedulerCmd -> Word8
schedulerCmdVal CREATE_TASK     = 0x00
schedulerCmdVal DELETE_TASK     = 0x01
schedulerCmdVal ADD_TO_TASK     = 0x02
schedulerCmdVal DELAY_TASK      = 0x03
schedulerCmdVal SCHEDULE_TASK   = 0x04
schedulerCmdVal QUERY_ALL_TASKS = 0x05
schedulerCmdVal QUERY_TASK      = 0x06
schedulerCmdVal SCHEDULER_RESET = 0x07

data SchedulerReply = QUERY_ALL_TASKS_REPLY
                | QUERY_TASK_REPLY
                | ERROR_FIRMATA_TASK_REPLY

getSchedulerReply :: Word8 -> Either Word8 SchedulerReply
getSchedulerReply 0x09 = Right QUERY_ALL_TASKS_REPLY
getSchedulerReply 0x0A = Right QUERY_TASK_REPLY
getSchedulerReply 0x0B = Right ERROR_FIRMATA_TASK_REPLY
getSchedulerReply n    = Left n

-- | Convert a byte to a Firmata command
getFirmataCmd :: Word8 -> Either Word8 FirmataCmd
getFirmataCmd w = classify
  where extract m | w .&. m == m = Just $ fromIntegral (w .&. 0x0F)
                  | True         = Nothing
        classify | w == 0xF0              = Right START_SYSEX
                 | w == 0xF4              = Right SET_PIN_MODE
                 | w == 0xF5              = Right SET_DIGITAL_PIN_VALUE
                 | w == 0xF7              = Right END_SYSEX
                 | w == 0xF9              = Right PROTOCOL_VERSION
                 | w == 0xFF              = Right SYSTEM_RESET
                 | Just i <- extract 0xE0 = Right $ ANALOG_MESSAGE      (InternalPin i)
                 | Just i <- extract 0x90 = Right $ DIGITAL_MESSAGE     (Port i)
                 | Just i <- extract 0xC0 = Right $ REPORT_ANALOG_PIN   (InternalPin i)
                 | Just i <- extract 0xD0 = Right $ REPORT_DIGITAL_PORT (Port i)
                 | True                   = Left w

-- | Sys-ex commands, see: https://github.com/firmata/protocol/blob/master/protocol.md
data SysExCmd = RESERVED_COMMAND        -- ^ @0x00@  2nd SysEx data byte is a chip-specific command (AVR, PIC, TI, etc).
              | ANALOG_MAPPING_QUERY    -- ^ @0x69@  ask for mapping of analog to pin numbers
              | ANALOG_MAPPING_RESPONSE -- ^ @0x6A@  reply with mapping info
              | CAPABILITY_QUERY        -- ^ @0x6B@  ask for supported modes and resolution of all pins
              | CAPABILITY_RESPONSE     -- ^ @0x6C@  reply with supported modes and resolution
              | PIN_STATE_QUERY         -- ^ @0x6D@  ask for a pin's current mode and value
              | PIN_STATE_RESPONSE      -- ^ @0x6E@  reply with a pin's current mode and value
              | EXTENDED_ANALOG         -- ^ @0x6F@  analog write (PWM, Servo, etc) to any pin
              | SERVO_CONFIG            -- ^ @0x70@  set max angle, minPulse, maxPulse, freq
              | STRING_DATA             -- ^ @0x71@  a string message with 14-bits per char
              | PULSE                   -- ^ @0x74@  Pulse, see: https://github.com/rwldrn/johnny-five/issues/18
              | SHIFT_DATA              -- ^ @0x75@  shiftOut config/data message (34 bits)
              | I2C_REQUEST             -- ^ @0x76@  I2C request messages from a host to an I/O board
              | I2C_REPLY               -- ^ @0x77@  I2C reply messages from an I/O board to a host
              | I2C_CONFIG              -- ^ @0x78@  Configure special I2C settings such as power pins and delay times
              | REPORT_FIRMWARE         -- ^ @0x79@  report name and version of the firmware
              | SAMPLING_INTERVAL       -- ^ @0x7A@  sampling interval
              | SCHEDULER_DATA          -- ^ @0x7B@  Scheduling commands
              | SYSEX_NON_REALTIME      -- ^ @0x7E@  MIDI Reserved for non-realtime messages
              | SYSEX_REALTIME          -- ^ @0x7F@  MIDI Reserved for realtime messages
              deriving Show

-- | Convert a 'SysExCmd' to a byte
sysExCmdVal :: SysExCmd -> Word8
sysExCmdVal RESERVED_COMMAND        = 0x00
sysExCmdVal ANALOG_MAPPING_QUERY    = 0x69
sysExCmdVal ANALOG_MAPPING_RESPONSE = 0x6A
sysExCmdVal CAPABILITY_QUERY        = 0x6B
sysExCmdVal CAPABILITY_RESPONSE     = 0x6C
sysExCmdVal PIN_STATE_QUERY         = 0x6D
sysExCmdVal PIN_STATE_RESPONSE      = 0x6E
sysExCmdVal EXTENDED_ANALOG         = 0x6F
sysExCmdVal SERVO_CONFIG            = 0x70
sysExCmdVal STRING_DATA             = 0x71
sysExCmdVal PULSE                   = 0x74
sysExCmdVal SHIFT_DATA              = 0x75
sysExCmdVal I2C_REQUEST             = 0x76
sysExCmdVal I2C_REPLY               = 0x77
sysExCmdVal I2C_CONFIG              = 0x78
sysExCmdVal REPORT_FIRMWARE         = 0x79
sysExCmdVal SAMPLING_INTERVAL       = 0x7A
sysExCmdVal SCHEDULER_DATA          = 0x7B
sysExCmdVal SYSEX_NON_REALTIME      = 0x7E
sysExCmdVal SYSEX_REALTIME          = 0x7F

-- | Convert a byte into a 'SysExCmd'
getSysExCommand :: Word8 -> Either Word8 SysExCmd
getSysExCommand 0x00 = Right RESERVED_COMMAND
getSysExCommand 0x69 = Right ANALOG_MAPPING_QUERY
getSysExCommand 0x6A = Right ANALOG_MAPPING_RESPONSE
getSysExCommand 0x6B = Right CAPABILITY_QUERY
getSysExCommand 0x6C = Right CAPABILITY_RESPONSE
getSysExCommand 0x6D = Right PIN_STATE_QUERY
getSysExCommand 0x6E = Right PIN_STATE_RESPONSE
getSysExCommand 0x6F = Right EXTENDED_ANALOG
getSysExCommand 0x70 = Right SERVO_CONFIG
getSysExCommand 0x71 = Right STRING_DATA
getSysExCommand 0x75 = Right SHIFT_DATA
getSysExCommand 0x76 = Right I2C_REQUEST
getSysExCommand 0x77 = Right I2C_REPLY
getSysExCommand 0x78 = Right I2C_CONFIG
getSysExCommand 0x79 = Right REPORT_FIRMWARE
getSysExCommand 0x7A = Right SAMPLING_INTERVAL
getSysExCommand 0x7B = Right SCHEDULER_DATA
getSysExCommand 0x7E = Right SYSEX_NON_REALTIME
getSysExCommand 0x7F = Right SYSEX_REALTIME
getSysExCommand 0x74 = Right PULSE
getSysExCommand n    = Left n

-- | State of the board
data BoardState = BoardState {
                    boardCapabilities    :: BoardCapabilities   -- ^ Capabilities of the board
                  , analogReportingPins  :: S.Set IPin          -- ^ Which analog pins are reporting
                  , digitalReportingPins :: S.Set IPin          -- ^ Which digital pins are reporting
                  , pinStates            :: M.Map IPin PinData  -- ^ For-each pin, store its data
                  , digitalWakeUpQueue   :: [MVar ()]           -- ^ Semaphore list to wake-up upon receiving a digital message
                  }

-- | State of the connection
data ArduinoConnection = ArduinoConnection {
                message       :: String -> IO ()                      -- ^ Current debugging routine
              , bailOut       :: forall a. String -> [String] -> IO a -- ^ Clean-up and quit with a hopefully informative message
              , port          :: SerialPort                           -- ^ Serial port we are communicating on
              , firmataID     :: String                               -- ^ The ID of the board (as identified by the Board itself)
              , boardState    :: MVar BoardState                      -- ^ Current state of the board
              , deviceChannel :: Chan Response                        -- ^ Incoming messages from the board
              , capabilities  :: BoardCapabilities                    -- ^ Capabilities of the board
              , listenerTid   :: MVar ThreadId                        -- ^ ThreadId of the listener
              }
