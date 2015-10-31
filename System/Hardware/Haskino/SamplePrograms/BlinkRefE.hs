-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.BlinkRefE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written using remote references (along with expressions), introduced in
-- version 0.3 of Haskino.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.BlinkRefE where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

blinkRefE :: IO ()
blinkRefE = withArduino False "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef true
           loopE $ do onOff <- readRemoteRef r 
                      digitalWriteE led onOff
                      delayMillisE delay
                      modifyRemoteRef r (\x -> notB x)
                      onOff <- readRemoteRef r 
                      digitalWriteE led onOff
                      delayMillisE delay
                      modifyRemoteRef r (\x -> notB x)
