-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Strong.Example
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Based on our initial simple example
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Strong.Example where

import System.Hardware.Haskino
import Data.Word

example :: IO ()
example = withArduino False "/dev/cu.usbmodem1421" $ do
          let button = 2
          let led1 = 6
          let led2 = 7
          setPinMode button INPUT
          setPinMode led1 OUTPUT
          setPinMode led2 OUTPUT
          loop $ do x <- digitalRead button
                    digitalWrite led1 x
                    digitalWrite led2 (not x)
                    delayMillis 100 
