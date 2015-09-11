-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.BlinkE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.  This version is
-- written with the expression based version of the commands and procedures
-- introduced in version 0.3 of Haskino
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.EvalE where

import Control.Monad.Trans (liftIO)
import Data.Boolean.Bits
import System.Hardware.Haskino
-- import Data.Boolean

evalE :: IO ()
evalE = withArduino True "/dev/cu.usbmodem1421" $ do
           let e = lit 13 + lit 12 * lit 7 - lit 10
           v <- eval8 e
           let e2 = bit 4
           v2 <- eval8 e2
           liftIO $ print v
           liftIO $ print v2
