-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.WhileBlinkE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written with the expression based version of the commands and procedures
-- introduced in version 0.3 of Haskino
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.WhileBlinkE where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

whileBlinkE :: IO ()
whileBlinkE = withArduino True "/dev/cu.usbmodem1421" $ do
              let led = 13
              let delay = 1000
              setPinModeE led OUTPUT
              i <- newRemoteRef (0 :: Expr Word8)
              while i (\x -> x <* 3) $ do 
                  digitalWriteE led true
                  delayMillisE delay
                  digitalWriteE led false
                  delayMillisE delay
                  modifyRemoteRef i (\x -> x + 1)
