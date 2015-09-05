-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.BlinkRefE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.BlinkRefE where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

blinkRefE :: IO ()
blinkRefE = withArduino False "/dev/cu.usbmodem1421" $ do
           let led = lit 13
           let delay = lit 1000
           setPinModeE led OUTPUT
           r <- newRemoteRef $ lit True
           loop $ do onOff <- readRemoteRef r 
                     digitalWriteE led onOff
                     delayMillisE delay
                     modifyRemoteRef r (\x -> notB x)
                     onOff <- readRemoteRef r 
                     digitalWriteE led onOff
                     delayMillisE delay
                     modifyRemoteRef r (\x -> notB x)
