-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ExampleProg
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Based on our initial simple example
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ExampleProg where

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

example :: Arduino ()
example = do let button = 2 :: Expr Word8
             let led1 = lit 6
             let led2 = lit 7
             x <- newRemoteRef (lit False)
             setPinModeE button INPUT
             setPinModeE led1 OUTPUT
             setPinModeE led2 OUTPUT
             while true $ do writeRemoteRef x  =<< digitalReadE button
                             ex <- readRemoteRef x
                             digitalWriteE led1 ex
                             digitalWriteE led2 (notB ex)
                             delayMillis 100 

exampleProg :: IO ()
exampleProg = withArduino False "/dev/cu.usbmodem1421" $ do
              let tid = 1
              createTaskE tid example
              bootTaskE tid
