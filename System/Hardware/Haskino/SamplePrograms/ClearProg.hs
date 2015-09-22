-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ClearProg
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Based on our initial simple example
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ClearProg where

import System.Hardware.Haskino

clearProg :: IO ()
clearProg = withArduino False "/dev/cu.usbmodem1421" $ do
            scheduleReset
