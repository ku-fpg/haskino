-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ExampleE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This simple example uses a button to control the state of two leds.  One
-- led is on when the button is pressed, and the other is on when the button
-- is not pressed.  This version is written with the expression based version
-- of the commands and procedures introduced in version 0.3 of Haskino
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ExampleE where

import Control.Monad (forever)

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

example :: IO ()
example = withArduino False "/dev/cu.usbmodem1421" $ do
           let button = lit 2
           let led1 = lit 6
           let led2 = lit 7
           x <- newRemoteRef false
           setPinModeE button INPUT
           setPinModeE led1 OUTPUT
           setPinModeE led2 OUTPUT
           while (lit True) $ do writeEffectRemoteRef x (digitalReadE button)
                                 ex <- readRemoteRef x
                                 digitalWriteE led1 ex
                                 digitalWriteE led2 (notB ex)
                                 delayMillis 100 
