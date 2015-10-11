-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ExampleE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Based on our initial simple example
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ExampleE where

import Control.Concurrent   (threadDelay)
import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

exampleE :: IO ()
exampleE = withArduino True "/dev/cu.usbmodem1421" $ do
           let button = 2
           let led1 = 6
           let led2 = 7
           x <- newRemoteRef false
           setPinModeE button INPUT
           setPinModeE led1 OUTPUT
           setPinModeE led2 OUTPUT
           while true $ do writeRemoteRef x  =<< digitalReadE button
                           ex <- readRemoteRef x
                           digitalWriteE led1 ex
                           digitalWriteE led2 (notB ex)
                           delayMillis 100 
