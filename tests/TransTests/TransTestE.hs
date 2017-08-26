-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Base test example used for rewrite written directly in deep version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransTestE where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

transTestProgE :: Arduino (Expr ())
transTestProgE = do
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
