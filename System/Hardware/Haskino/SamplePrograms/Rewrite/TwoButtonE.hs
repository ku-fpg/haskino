-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonE where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

twoButtonProgE :: Arduino ()
twoButtonProgE = do
    let led = 13
    let button1 = 2
    let button2 = 3
    setPinModeE led OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    loopE $ do 
        a <- digitalReadE button1
        b <- digitalReadE button2
        c <- return (a ||* b)
        digitalWriteE led c
        delayMillisE 1000
