{-# LANGUAGE FlexibleInstances, GADTs, KindSignatures,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving #-}

module DeepArduino.Arduino where

import           Control.Applicative
import           Control.Monad (ap, liftM2)

import qualified Data.Map as M
import           Data.Monoid
import           Data.Word (Word8, Word16, Word32)

-----------------------------------------------------------------------------

data Arduino :: * -> * where
    Procedure      :: Procedure                       -> Arduino ()
    Local          :: Local a                         -> Arduino a
    Query          :: Query a                         -> Arduino a
    TaskProcedure  :: TaskProcedure                   -> Arduino a
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

-- | The mode for a pin.
data PinMode = INPUT    -- ^ Digital input
             | OUTPUT   -- ^ Digital output
             | ANALOG   -- ^ Analog input
             | PWM      -- ^ PWM (Pulse-Width-Modulation) output 
             | SERVO    -- ^ Servo Motor controller
             | SHIFT    -- ^ Shift controller
             | I2C      -- ^ I2C (Inter-Integrated-Circuit) connection
             deriving (Eq, Show, Enum)

-- | Resolution, as referred to in http://firmata.org/wiki/Protocol#Capability_Query
-- TODO: Not quite sure how this is used, so merely keep it as a Word8 now
type Resolution = Word8

-- | Capabilities of a pin
data PinCapabilities  = PinCapabilities {
                          analogPinNumber :: Maybe Word8              -- ^ Analog pin number, if any
                        , allowedModes    :: [(PinMode, Resolution)]  -- ^ Allowed modes and resolutions
                        }

-- | What the board is capable of and current settings
newtype BoardCapabilities = BoardCapabilities (M.Map IPin PinCapabilities)

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
     | SetPinMode IPin PinMode                  -- ^ Set the mode on a pin
     | DigitalReport Port Bool                  -- ^ Digital report values on port enable/disable
     | AnalogReport IPin Bool                   -- ^ Analog report values on pin enable/disable
     | DigitalPortWrite Port Word8 Word8        -- ^ Set the values on a port digitally
     | AnalogPinWrite IPin Word8 Word8          -- ^ Send an analog-write; used for servo control
     | AnalogPinExtendedWrite IPin [Word8]      -- ^ 
     | SamplingInterval Word8 Word8             -- ^ Set the sampling interval
     | I2CWrite I2CAddrMode SlaveAddress [Word16]
     | I2CConfig Word16
     | ServoConfig IPin MinPulse MaxPulse
     deriving Show

data Local :: * -> * where
     DigitalPortRead  :: Port -> Local Word8          -- ^ Set the values on a port digitally
     AnalogPinRead    :: IPin -> Local Word8          -- ^ Send an analog-write; used for servo control

deriving instance Show a => Show (Local a)

data TaskProcedure =
       CrateTask TaskID TaskLength
     | DeleteTask TaskID
     | AddToTask TaskID [Procedure]
     | DelayTask TaskTime
     | ScheduleTask TaskID TaskTime
     | ScheduleReset

data Query :: * -> * where
     QueryFirmware  :: Query (Word8, Word8, String)   -- ^ Query the Firmata version installed
     CapabilityQuery :: Query BoardCapabilities       -- ^ Query the capabilities of the board
     AnalogMappingQuery :: Query [Word8]              -- ^ Query the mapping of analog pins
     Pulse :: IPin -> Bool -> Word32 -> Word32 -> Query Word32 -- ^ Request for a pulse reading on a pin, value, duration, timeout
     I2CRead :: I2CAddrMode -> SlaveAddress -> Maybe SlaveRegister -> Query [Word16]
     QueryAllTasks :: Query [TaskID]
     QueryTask :: TaskID -> Query (TaskTime, TaskLength, TaskPos, [Procedure])

deriving instance Show a => Show (Query a)
