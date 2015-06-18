-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Comm
--                Based on System.Hardware.Arduino.comm
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

module System.Hardware.DeepArduino.Data where

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

import           System.Hardware.DeepArduino.Utils

import Debug.Trace
-----------------------------------------------------------------------------

data Arduino :: * -> * where
    Procedure      :: Procedure                       -> Arduino ()
    Local          :: Local a                         -> Arduino a
    Query          :: Query a                         -> Arduino a
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

-- | Declare a pin by its index. For maximum portability, prefer 'digital'
-- and 'analog' functions, which will adjust pin indexes properly based on
-- which board the program is running on at run-time, as Arduino boards
-- differ in their pin numbers. This function is provided for cases where
-- a pin is used in mixed-mode, i.e., both for digital and analog purposes,
-- as Arduino does not really distinguish pin usage. In these cases, the
-- user has the proof obligation to make sure that the index used is supported
-- on the board with appropriate capabilities.
pin :: Word8 -> Pin
pin = MixedPin

-- | Declare an digital pin on the board. For instance, to refer to digital pin no 12
-- use 'digital' @12@.
digital :: Word8 -> Pin
digital = DigitalPin

-- | Declare an analog pin on the board. For instance, to refer to analog pin no 0
-- simply use 'analog' @0@.
--
-- Note that 'analog' @0@ on an Arduino UNO will be appropriately adjusted
-- internally to refer to pin 14, since UNO has 13 digital pins, while on an
-- Arduino MEGA, it will refer to internal pin 55, since MEGA has 54 digital pins;
-- and similarly for other boards depending on their capabilities.
-- (Also see the note on 'pin' for pin mappings.)
analog :: Word8 -> Pin
analog = AnalogPin

-- | Bailing out: print the given string on stdout and die
die :: ArduinoConnection -> String -> [String] -> IO a
die c m ms = do 
    let f = bailOut c
    f m ms

-- | On the arduino, digital pin numbers are in 1-to-1 match with
-- the board pins. However, ANALOG pins come at an offset, determined by
-- the capabilities query. Users of the library refer to these pins
-- simply by their natural numbers, which makes for portable programs
-- between boards that have different number of digital pins. We adjust
-- for this shift here.
getInternalPin :: ArduinoConnection -> Pin -> IO IPin
getInternalPin c (MixedPin p)   = return $ InternalPin p
getInternalPin c (DigitalPin p) = return $ InternalPin p
getInternalPin c (AnalogPin p)=
    case listToMaybe [realPin | (realPin, PinCapabilities{analogPinNumber = Just n}) <- M.toAscList caps, p == n] of
         Nothing -> die c ("DeepArduino: " ++ show p ++ " is not a valid analog-pin on this board.")
                        -- Try to be helpful in case they are trying to use a large value thinking it needs to be offset
                        ["Hint: To refer to analog pin number k, simply use 'pin k', not 'pin (k+noOfDigitalPins)'" | p > 13]
         Just rp -> return rp
  where 
    BoardCapabilities caps = capabilities c

-- | Similar to getInternalPin above, except also makes sure the pin is in a required mode.
convertAndCheckPin :: ArduinoConnection -> String -> Pin -> PinMode -> IO (IPin, PinData)
convertAndCheckPin c what p' m = do
   p  <- getInternalPin c p'
   pd <- getPinData c p
   let user = userPinNo p'
       board = pinNo p
       bInfo
         | user == board = ""
         | True          = " (On board " ++ show p ++ ")"
   when (pinMode pd /= m) $ die c ("Invalid " ++ what ++ " call on pin " ++ show p' ++ bInfo)
                                [ "The current mode for this pin is: " ++ show (pinMode pd)
                                , "For " ++ what ++ ", it must be set to: " ++ show m
                                , "via a proper call to setPinMode"
                                ]
   return (p, pd)


-- | On the Arduino, pins are grouped into banks of 8.
-- Given a pin, this function determines which port it belongs to
pinPort :: IPin -> Port
pinPort p = Port $ pinNoPortNo $ pinNo p

-- Given a pin number, this function determines which port it belongs to
pinNoPortNo :: Word8 -> Word8
pinNoPortNo n = n `quot` 8

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

-- | LCD's connected to the board
newtype LCD = LCD Int
            deriving (Eq, Ord, Show)

-- | Hitachi LCD controller: See: <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller>.
-- We model only the 4-bit variant, with RS and EN lines only. (The most common Arduino usage.)
-- The data sheet can be seen at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>.
data LCDController = Hitachi44780 {
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
                , lcdController     :: LCDController -- ^ Actual controller
                }

-- | What the board is capable of and current settings
newtype BoardCapabilities = BoardCapabilities (M.Map IPin PinCapabilities)

instance Show BoardCapabilities where
  show (BoardCapabilities m) = intercalate "\n" (map sh (M.toAscList m))
    where sh (p, PinCapabilities{analogPinNumber, allowedModes}) = show p ++ sep ++ unwords [show md | (md, _) <- allowedModes]
             where sep = maybe ": " (\i -> "[A" ++ show i ++ "]: ") analogPinNumber

type SlaveAddress = Word8
type SlaveRegister = Word8
type MinPulse = Word16
type MaxPulse = Word16
type TaskLength = Word16
type TaskID = Word8
type TaskTime = Word32
type TaskPos = Word16
type StepDevice = Word8
type NumSteps = Word32
type StepSpeed = Word16
type StepAccel = Int
type StepPerRev = Word16
data StepDelay = OneUs | TwoUs
      deriving Show
data StepDir = CW | CCW
      deriving Show
data StepType = TwoWire | FourWire | StepDir
      deriving Show

data Procedure =
       SystemReset                              -- ^ Send system reset
     | SetPinMode Pin PinMode                   -- ^ Set the mode on a pin
     | DigitalPortReport Port Bool              -- ^ Digital report values on port enable/disable
     | DigitalReport Pin Bool                   -- ^ Digital report values on port enable/disable
     | AnalogReport Pin Bool                    -- ^ Analog report values on pin enable/disable
     | DigitalPortWrite Port Word8 Word8        -- ^ Set the values on a port digitally
     | DigitalWrite Pin Bool                    -- ^ Set the value on a pin digitally
     | AnalogWrite Pin Word8 Word8              -- ^ Send an analog-write; used for servo control
     | AnalogExtendedWrite Pin [Word8]          -- ^ 
     | SamplingInterval Word8 Word8             -- ^ Set the sampling interval
     | I2CWrite SlaveAddress [Word8]
     | I2CConfig Word16
     -- TBD add I2C continuous read
     | ServoConfig Pin MinPulse MaxPulse
     -- TBD add one wire and encoder procedures
     | StepperConfig StepDevice StepType StepDelay StepPerRev Pin Pin (Maybe Pin) (Maybe Pin)
     | StepperStep StepDevice StepDir NumSteps StepSpeed (Maybe StepAccel)
     | CreateTask TaskID (Arduino ())
     | DeleteTask TaskID
     | Delay TaskTime
     | ScheduleTask TaskID TaskTime
     | ScheduleReset

systemReset :: Arduino ()
systemReset = Procedure SystemReset

setPinMode :: Pin -> PinMode -> Arduino ()
setPinMode p pm = Procedure $ SetPinMode p pm

digitalPortReport :: Port -> Bool -> Arduino ()
digitalPortReport p b = Procedure $ DigitalPortReport p b

digitalReport :: Pin -> Bool -> Arduino ()
digitalReport p b = Procedure $ DigitalReport p b

analogReport :: Pin -> Bool -> Arduino ()
analogReport p b = Procedure $ AnalogReport p b

digitalPortWrite :: Port -> Word16 -> Arduino ()
digitalPortWrite p w = Procedure $ DigitalPortWrite p w1 w2
  where
    [w1, w2] = word16ToArduinoBytes w

digitalWrite :: Pin -> Bool -> Arduino ()
digitalWrite p b = Procedure $ DigitalWrite p b

analogWrite :: Pin -> Word16 -> Arduino ()
analogWrite p w = Procedure $ AnalogWrite p w1 w2
  where
    [w1, w2] = word16ToArduinoBytes w

analogExtendedWrite :: Pin -> [Word8] -> Arduino ()
analogExtendedWrite p ws = Procedure $ AnalogExtendedWrite p ws

samplingInterval :: Word16 -> Arduino ()
samplingInterval w = Procedure $ SamplingInterval w1 w2
  where
    [w1, w2] = word16ToArduinoBytes w

i2cWrite :: SlaveAddress -> [Word8] -> Arduino ()
i2cWrite sa ws = Procedure $ I2CWrite sa ws

i2cConfig :: Word16 -> Arduino ()
i2cConfig w = Procedure $ I2CConfig w

servoConfig :: Pin -> MinPulse -> MaxPulse -> Arduino ()
servoConfig p min max = Procedure $ ServoConfig p min max

stepperConfig :: StepDevice -> StepType -> StepDelay -> StepPerRev -> Pin -> Pin -> (Maybe Pin) -> (Maybe Pin) -> Arduino ()
stepperConfig dev ty d sr p1 p2 p3 p4 = Procedure $ StepperConfig dev ty d sr p1 p2 p3 p4

stepperStep :: StepDevice -> StepDir -> NumSteps -> StepSpeed -> Maybe StepAccel -> Arduino ()
stepperStep dev sd ns sp ac = Procedure $ StepperStep dev sd ns sp ac

deleteTask :: TaskID -> Arduino ()
deleteTask tid = Procedure $ DeleteTask tid

delay :: TaskTime -> Arduino ()
delay t = Procedure $ Delay t

scheduleTask :: TaskID -> TaskTime -> Arduino ()
scheduleTask tid tt = Procedure $ ScheduleTask tid tt

scheduleReset :: Arduino ()
scheduleReset = Procedure ScheduleReset

data Local :: * -> * where
     DigitalPortRead  :: Port -> Local Word8          -- ^ Read the values on a port digitally
     DigitalRead      :: Pin -> Local Bool            -- ^ Read the avlue ona pin digitally
     AnalogRead       :: Pin -> Local Word16          -- ^ Read the analog value on a pin
     WaitFor          :: Pin -> Local Bool
     WaitAny          :: [Pin] -> Local [Bool]
     WaitAnyHigh      :: [Pin] -> Local [Bool]
     WaitAnyLow       :: [Pin] -> Local [Bool]
     Debug            :: String -> Local ()
     -- TBD Add pin reporting Local?
deriving instance Show a => Show (Local a)

digitalPortRead :: Port -> Arduino Word8
digitalPortRead p = Local $ DigitalPortRead p

digitalRead :: Pin -> Arduino Bool
digitalRead p = Local $ DigitalRead p

analogRead :: Pin -> Arduino Word16
analogRead p = Local $ AnalogRead p

waitFor :: Pin -> Arduino Bool
waitFor p = Local $ WaitFor p

waitAny :: [Pin] -> Arduino [Bool]
waitAny ps = Local $ WaitAny ps

waitAnyHigh :: [Pin] -> Arduino [Bool]
waitAnyHigh ps = Local $ WaitAnyHigh ps

waitAnyLow :: [Pin] -> Arduino [Bool]
waitAnyLow ps = Local $ WaitAnyLow ps

debug :: String -> Arduino ()
debug msg = Local $ Debug msg

-- | Read the value of a pin in digital mode; this is a non-blocking call, returning
-- the current value immediately. See 'waitFor' for a version that waits for a change
-- in the pin first.
runDigitalRead :: ArduinoConnection -> Pin -> IO Bool
runDigitalRead c p' = do
   (_, pd) <- convertAndCheckPin c "digitalRead" p' INPUT
   return $ case pinValue pd of
              Just (Left v) -> v
              _             -> False -- no (correctly-typed) value reported yet, default to False

-- | Read the value of a pin in analog mode; this is a non-blocking call, immediately
-- returning the last sampled value. It returns @0@ if the voltage on the pin
-- is 0V, and @1023@ if it is 5V, properly scaled. (See `setAnalogSamplingInterval` for
-- sampling frequency.)
runAnalogRead :: ArduinoConnection -> Pin -> IO Int
runAnalogRead c p' = do
   (_, pd) <- convertAndCheckPin c "analogRead" p' ANALOG
   return $ case pinValue pd of
              Just (Right v) -> v
              _              -> 0 -- no (correctly-typed) value reported yet, default to 0

-- | Read the value of a port in digital mode; this is a non-blocking call, returning
-- the current value immediately. See 'waitAny' for a version that waits for a change
-- in the port first.
runDigitalPortRead :: ArduinoConnection -> Port -> IO Word8
runDigitalPortRead c p = do
    let bs = boardState c
    let err = bailOut c
    withMVar bs $ \bst ->
       return $ M.findWithDefault 0 p (portStates bst)

-- | Wait for a change in the value of the digital input pin. Returns the new value.
-- Note that this is a blocking call. For a non-blocking version, see 'digitalRead', which returns the current
-- value of a pin immediately.
runWaitFor :: ArduinoConnection -> Pin -> IO Bool
runWaitFor c p = head `fmap` (runWaitAny c) [p]

-- | Wait for a change in any of the given pins. Once a change is detected, all the new values are
-- returned. Similar to 'waitFor', but is useful when we are watching multiple digital inputs.
runWaitAny :: ArduinoConnection -> [Pin] -> IO [Bool]
runWaitAny c ps = map snd `fmap` (runWaitGeneric c) ps

-- | Wait for any of the given pins to go from low to high. If all of the pins are high to start
-- with, then we first wait for one of them to go low, and then wait for one of them to go back high.
-- Returns the new values.
runWaitAnyHigh :: ArduinoConnection -> [Pin] -> IO [Bool]
runWaitAnyHigh c ps = do
   curVals <- mapM (runDigitalRead c) ps
   when (and curVals) $ void $ runWaitAnyLow c ps   -- all are H to start with, wait for at least one to go low
   vs <- runWaitGeneric c ps  -- wait for some change
   if (False, True) `elem` vs
      then return $ map snd vs
      else runWaitAnyHigh c ps

-- | Wait for any of the given pins to go from high to low. If all of the pins are low to start
-- with, then we first wait for one of them to go high, and then wait for one of them to go back low.
-- Returns the new values.
runWaitAnyLow :: ArduinoConnection -> [Pin] -> IO [Bool]
runWaitAnyLow c ps = do
   curVals <- mapM (runDigitalRead c) ps
   unless (or curVals) $ void $ runWaitAnyHigh c ps   -- all are L to start with, wait for at least one to go high
   vs <- runWaitGeneric c ps  -- wait for some change
   if (True, False) `elem` vs
      then return $ map snd vs
      else runWaitAnyLow c ps

-- | A utility function, waits for any change on any given pin
-- and returns both old and new values. It's guaranteed that
-- at least one returned pair have differing values.
runWaitGeneric :: ArduinoConnection -> [Pin] -> IO [(Bool, Bool)]
runWaitGeneric c ps = do
   curVals <- mapM (runDigitalRead c) ps
   semaphore <- newEmptyMVar
   let wait = do digitalWakeUp c semaphore
                 takeMVar semaphore
                 newVals <- mapM (runDigitalRead c) ps
                 if curVals == newVals
                    then wait
                    else return $ zip curVals newVals
   wait

createTask :: TaskID -> Arduino () -> Arduino ()
createTask tid ps = Procedure (CreateTask tid ps)

data Query :: * -> * where
     QueryFirmware  :: Query (Word8, Word8, String)   -- ^ Query the Firmata version installed
     CapabilityQuery :: Query BoardCapabilities       -- ^ Query the capabilities of the board
     AnalogMappingQuery :: Query [Word8]              -- ^ Query the mapping of analog pins
     Pulse :: IPin -> Bool -> Word32 -> Word32 -> Query Word32 -- ^ Request for a pulse reading on a pin, value, duration, timeout
     I2CRead :: SlaveAddress -> Maybe SlaveRegister -> Word8 -> Query [Word8]
     -- TBD add one wire queries
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
pulse p b w1 w2 = Query $ Pulse p b w1 w2

i2cRead :: SlaveAddress -> Maybe SlaveRegister -> Word8 -> Arduino [Word8]
i2cRead sa sr cnt = Query $ I2CRead sa sr cnt

queryAllTasks :: Arduino [TaskID]
queryAllTasks = Query QueryAllTasks

queryTask :: TaskID -> Arduino (TaskID, TaskTime, TaskLength, TaskPos, [Word8])
queryTask tid = Query $ QueryTask tid

-- | A response, as returned from the Arduino
data Response = Firmware Word8 Word8 String          -- ^ Firmware version (maj/min and indentifier
              | Capabilities BoardCapabilities       -- ^ Capabilities report
              | AnalogMapping [Word8]                -- ^ Analog pin mappings
              | DigitalMessage Port Word8 Word8      -- ^ Status of a port
              | AnalogMessage  IPin Word8 Word8      -- ^ Status of an analog pin
              | StringMessage  String                -- ^ String message from Firmata
              | PulseResponse  IPin Word32           -- ^ Repsonse to a PulseInCommand
              | I2CReply Word8 Word8 [Word8]       -- ^ Response to a I2C Read
              | QueryAllTasksReply [Word8]           -- ^ Response to Query All Tasks
              | QueryTaskReply TaskID TaskTime TaskLength TaskPos [Word8]
              | ErrorTaskReply TaskID TaskTime TaskLength TaskPos [Word8]
              | Unimplemented (Maybe String) [Word8] -- ^ Represents messages currently unsupported
    deriving Show

-- | Which modes does this pin support?
getPinModes :: ArduinoConnection -> IPin -> IO [PinMode]
getPinModes c p = do
  let BoardCapabilities caps = capabilities c
  case p `M.lookup` caps of
    Nothing                            -> return []
    Just PinCapabilities{allowedModes} -> return $ map fst allowedModes

-- | Current state of the pin
getPinData :: ArduinoConnection -> IPin -> IO PinData
getPinData c p = do
  let bs = boardState c
  let err = bailOut c
  withMVar bs $ \bst ->
     case p `M.lookup` pinStates bst of
       Nothing -> err ("Trying to access " ++ show p ++ " without proper setup.")
                      ["Make sure that you use 'setPinMode' to configure this pin first."]
       Just pd -> return pd

-- | Keep track of listeners on a digital message
digitalWakeUp :: ArduinoConnection -> MVar () -> IO ()
digitalWakeUp c semaphore = do
    let bs = boardState c
    modifyMVar_ bs $ \bst -> return bst{digitalWakeUpQueue = semaphore : digitalWakeUpQueue bst}

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

-- | Firmata scheduler commands, see: https://github.com/firmata/protocol/blob/master/scheduler.md
data StepperCmd = CONFIG_STEPPER    -- ^ @0x00@
                | STEP_STEPPER      -- ^ @0x01@

-- | Compute the numeric value of a scheduler command
stepperCmdVal :: StepperCmd -> Word8
stepperCmdVal CONFIG_STEPPER = 0x00
stepperCmdVal STEP_STEPPER   = 0x01

stepDelayVal :: StepDelay -> Word8
stepDelayVal OneUs = 0x00
stepDelayVal TwoUs = 0x08

stepDirVal :: StepDir -> Word8
stepDirVal CW = 0x00
stepDirVal CCW = 0x01

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
              | STEPPER_DATA            -- ^ @0x72@  a stepper motor config or step command
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
sysExCmdVal STEPPER_DATA            = 0x72
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
getSysExCommand 0x72 = Right STEPPER_DATA
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

registerDigitalPinReport :: ArduinoConnection -> IPin -> Bool -> IO Bool
registerDigitalPinReport c p r = registerDigitalReport c pins portNo r 
  where
    pn = pinNo p
    pins = S.singleton pn
    portNo = pinNoPortNo pn

registerDigitalPortReport :: ArduinoConnection -> Port -> Bool -> IO Bool
registerDigitalPortReport c p r = registerDigitalReport c pins pn r 
  where
    pn = portNo p
    pins = S.fromList [(pn*8)..((pn*8+7))]

registerDigitalReport :: ArduinoConnection -> S.Set Word8 -> Word8 -> Bool -> IO Bool
registerDigitalReport c pins portNo r = do
    bs <- takeMVar (boardState c)
    let oldPins = digitalReportingPins bs
    let newPins = if r then S.union oldPins pins else S.difference oldPins pins
    putMVar (boardState c) bs {digitalReportingPins = newPins}
    return $ if (r) then notEl oldPins
                    else (el oldPins) && (notEl newPins) 
  where
    notEl ps = portNo `notElem` map pinNoPortNo (S.elems ps)
    el ps = portNo `elem` map pinNoPortNo (S.elems ps)

-- | Keep track of pin-mode changes
registerPinMode :: ArduinoConnection -> IPin -> PinMode -> IO ()
registerPinMode c p m = do
        -- first check that the requested mode is supported for this pin
        let BoardCapabilities caps = capabilities c
        case p `M.lookup` caps of
          Nothing
             -> die c ("Invalid access to unsupported pin: " ++ show p)
                    ("Available pins are: " : ["  " ++ show k | (k, _) <- M.toAscList caps])
          Just PinCapabilities{allowedModes}
            | m `notElem` map fst allowedModes
            -> die c ("Invalid mode " ++ show m ++ " set for " ++ show p)
                   ["Supported modes for this pin are: " ++ unwords (if null allowedModes then ["NONE"] else map show allowedModes)]
          _ -> return ()
        -- Modify the board state MVar for the mode change
        modifyMVar_ (boardState c) $ \bst -> return bst{pinStates = M.insert p PinData{pinMode = m, pinValue = Nothing} (pinStates bst) }
        return ()

-- | State of the board
data BoardState = BoardState {
                    boardCapabilities    :: BoardCapabilities   -- ^ Capabilities of the board
                  , digitalReportingPins :: S.Set Word8         -- ^ Which digital pins are reporting
                  , pinStates            :: M.Map IPin PinData  -- ^ For-each pin, store its data
                  , portStates           :: M.Map Port Word8    -- ^ For-each digital port, store its data
                  , digitalWakeUpQueue   :: [MVar ()]           -- ^ Semaphore list to wake-up upon receiving a digital message
                  , nextStepperDevice    :: Word8
                  , lcds                 :: M.Map LCD LCDData   -- ^ LCD's attached to the board
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
