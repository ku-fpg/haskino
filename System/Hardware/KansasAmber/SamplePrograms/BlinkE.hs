-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.SamplePrograms.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.KansasAmber.SamplePrograms.BlinkE where

import Control.Monad (forever)

import System.Hardware.KansasAmber
import Data.Boolean

blink :: IO ()
blink = withArduino False "/dev/cu.usbmodem1421" $ do
           let button = Lit8 10
           let led1 = Lit8 10
           let led2 = Lit8 10
           x <- newVarB "x"
           setPinModeE led1 OUTPUT
           setPinModeE led2 OUTPUT
           while (LitB True) $ do x =** digitalReadE button
                                  digitalWriteE led1 x
                                  digitalWriteE led2 (notB x)
                                  delayMillis 100 

