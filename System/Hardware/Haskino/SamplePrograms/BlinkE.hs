-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.BlinkE where

import Control.Monad (forever)

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

blinkE :: IO ()
blinkE = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = lit 13
           let delay = lit 1000
           setPinModeE led OUTPUT
           loop $ do digitalWriteE led (lit True)
                     delayMillisE delay
                     digitalWriteE led (lit False)
                     delayMillisE delay
