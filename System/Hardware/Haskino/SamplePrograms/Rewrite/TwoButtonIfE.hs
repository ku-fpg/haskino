-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIfE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIfE where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

twoButtonProg1E :: Arduino ()
twoButtonProg1E = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinModeE led1 OUTPUT
    setPinModeE led2 OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    loopE $ do 
        a <- digitalReadE button1
        digitalWriteE led1 true
        b <- digitalReadE button2
        ifThenElseE 
          (a ||* b)
          (   do
            digitalWriteE led1 a
            digitalWriteE led2 b
            return (notB a))
          (   do
            digitalWriteE led1 (notB a)
            digitalWriteE led2 (notB b)
            a' <- digitalReadE led1
            return (a' &&* b) )
        delayMillisE 1000

twoButtonProg2E :: Arduino ()
twoButtonProg2E = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinModeE led1 OUTPUT
    setPinModeE led2 OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    loopE $ do 
        a <- digitalReadE button1
        b <- digitalReadE button2
        ifThenElseE 
          (a ||* b)
          (   do
            digitalWriteE led1 a
            digitalWriteE led2 b
            return true )
          (   do
            digitalWriteE led1 (notB a)
            digitalWriteE led2 (notB b)
            digitalReadE led1 )
        delayMillisE 1000

twoButtonProg3E :: Arduino ()
twoButtonProg3E = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinModeE led1 OUTPUT
    setPinModeE led2 OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    loopE $ do 
        a <- digitalReadE button1
        b <- digitalReadE button2
        ifThenElseUnitE 
          (a ||* b)
          (   do
            digitalWriteE led1 a
            digitalWriteE led2 b )
          (   do
            digitalWriteE led1 (notB a)
            digitalWriteE led2 (notB b) )
        delayMillisE 1000
