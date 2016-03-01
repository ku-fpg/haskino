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
           t <- readRemoteRef r
           loopE $ do x <- readRemoteRef r
                      digitalWriteE led x
                      delayMillisE delay
                      digitalWriteE led (notB x) -- false
                      delayMillisE delay

blinkEApp :: IO ()
blinkEApp = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef true
           loopE $ do digitalWriteE led <$> readRemoteRef r
                      delayMillisE delay
                      pure (digitalWriteE led) <*> readRemoteRef r
                      delayMillisE delay

blinkE2Blink :: IO ()
blinkE2Blink = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef true
           digitalWriteE led <$> readRemoteRef r
           delayMillisE delay
           modifyRemoteRef r (\x -> notB x)
           digitalWriteE led <$> readRemoteRef r
           delayMillisE delay

