-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Pars.ShiftRegisters
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Abstractions for shift-register IC parts.
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.KansasAmber.Parts.ShiftRegisters(
     -- * Shift register abstraction
     ShiftRegister(..)
     -- * Supported shift-registers
     -- ** Texas Instruments 7HC595
   , SR_74HC595(..)
   ) where

import Data.Foldable (forM_)

import System.Hardware.KansasAmber
import System.Hardware.KansasAmber.Data (die)

-- | A shift-register class as supported by the KansasAmber library.
class ShiftRegister a where
  -- | Capacity
  size :: a -> Int
  -- | Display name
  name :: a -> String
  -- | Data sheet (typically a URL)
  dataSheet :: a -> String
  -- | Initialize the shift-register
  initialize :: a -> Arduino ()
  -- | Disable the output, putting it into high-impedance state
  disable :: a -> Arduino ()
  -- | Enable the output, getting it out of the high-impedance state
  enable :: a -> Arduino ()
  -- | Clear the contents
  clear  :: a -> Arduino ()
  -- | Push a single bit down the shift-register
  push   :: a -> Bool -> Arduino ()
  -- | Store the pushed-in values in the storage register
  store  :: a -> Arduino ()
  -- | Read the current value stored
  read :: a -> Arduino [Bool]

-- | The Texas-Instruments 74HC595 8-bit shift register with 3-state
-- outputs. Data sheet: <http://www.ti.com/lit/ds/symlink/sn74hc595.pdf>.
--
-- This is a versatile 8-bit shift-register with separate serial and register
-- clocks, allowing shifting to be done while the output remains untouched. We
-- model all control pins provided. Note that the enable and clear lines are
-- negated.
data SR_74HC595 = SR_74HC595 {
               serial  :: Pin         -- ^ Chip Pin: 14: Serial input
             , nEnable :: Pin         -- ^ Chip Pin: 13: Negated output-enable
             , rClock  :: Pin         -- ^ Chip Pin: 12: Register clock, positive triggered
             , sClock  :: Pin         -- ^ Chip Pin: 11: Serial clock, positive triggered
             , nClear  :: Pin         -- ^ Chip Pin: 10: Negated clear-data
             , mbBits  :: Maybe [Pin] -- ^ Chip Pins: 15, 1-7, and 8: Sequence of output bits, connect only if reading is necessary
             }

instance ShiftRegister SR_74HC595 where
  size      _ = 8
  name      _ = "TI SR_74HC595"
  dataSheet _ = "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf"
  initialize sr@SR_74HC595{nEnable, serial, rClock, sClock, nClear, mbBits} =
        do mapM_ (`setPinMode` OUTPUT) [nEnable, nClear, serial, rClock, sClock]
           clear sr
           enable sr
           forM_ mbBits (mapM_ (`setPinMode` INPUT))
  disable SR_74HC595{nEnable} = digitalWrite nEnable True
  enable  SR_74HC595{nEnable} = digitalWrite nEnable False
  clear SR_74HC595{nClear}    = do digitalWrite nClear False
                                   digitalWrite nClear True
  push  SR_74HC595{serial, sClock} b = fallingEdge sClock $ digitalWrite serial b
  store SR_74HC595{rClock}           = fallingEdge rClock (return ())
  read  sr@SR_74HC595{mbBits} = case mbBits of
                                Nothing  -> return []
                                Just pins -> mapM digitalRead pins

-- | Execute action, followed by a simulated falling edge on the given clock
fallingEdge :: Pin -> Arduino a -> Arduino a
fallingEdge clk action = do r <- action
                            digitalWrite clk True
                            digitalWrite clk False
                            return r
