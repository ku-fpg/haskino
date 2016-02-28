-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.BlinkEApp
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written with the expression based version of the commands and procedures
-- introduced in version 0.3 of Haskino.  This also tests applicative style
-- in the deep version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.BlinkE where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

blinkE :: IO ()
blinkE = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef true
           loopE $ do x <- readRemoteRef r
                      digitalWriteE led x
                      delayMillisE delay
                      digitalWriteE led false
                      delayMillisE delay

blinkEApp :: IO ()
blinkEApp = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef true
           loopE $ do _ <- digitalWriteE <$> pure led <*> readRemoteRef r
                      delayMillisE delay
                      digitalWriteE led false
                      delayMillisE delay
