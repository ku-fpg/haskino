-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Test.BlinkRefIfE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led. This version is
-- written using remote references, and demonstrating If Then Else blocks in
-- the remote monad.  (These features were introduced in version 0.3 of 
-- Haskino)
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Test.BlinkRefIfE where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

blinkOnOff :: RemoteRef Bool -> Expr Word8 -> Expr Word32 -> Arduino ()
blinkOnOff ref led del = do onOff <- readRemoteRef ref
                            digitalWriteE led onOff
                            delayMillisE del
                            modifyRemoteRef ref (\x -> notB x)
                            onOff <- readRemoteRef ref 
                            digitalWriteE led onOff
                            delayMillisE del
                            modifyRemoteRef ref (\x -> notB x)

blinkRefIfE :: IO ()
blinkRefIfE = withArduino False "/dev/cu.usbmodem1421" $ do
              let led = 13
              let slow = 2000
              let fast = 1000
              setPinModeE led OUTPUT
              r1 <- newRemoteRef true
              r2 <- newRemoteRef false
              loopE $ do slowFast <- readRemoteRef r2
                         ifThenElseE (slowFast) (blinkOnOff r1 led slow) (blinkOnOff r1 led fast)
                         modifyRemoteRef r2 (\x -> notB x)
