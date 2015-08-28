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

blink :: IO ()
blink = withArduino False "/dev/cu.usbmodem1421" $ do
           let button = lit 2
           let led1 = lit 10
           let led2 = lit 11
           x <- newVarB "x"
           setPinModeE button INPUT
           setPinModeE led1 OUTPUT
           setPinModeE led2 OUTPUT
           while (LitB True) $ do x =** digitalReadE button
                                  digitalWriteE led1 x
                                  digitalWriteE led2 (notB x)
                                  delayMillis 100 
