-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFuncE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFuncE where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

myRead1E :: Expr Word8 -> Arduino (Expr Bool)
myRead1E p = do
    delayMillisE 100
    a <- digitalReadE (p+1)
    return (notB a)

myRead2E :: Expr Word8 -> Arduino (Expr Bool)
myRead2E p = do
    delayMillisE 100
    digitalReadE (p+1)

myRead3E :: Expr Word8 -> Arduino (Expr Bool)
myRead3E p = do
    delayMillisE 100
    return true

myWriteE :: Expr Word8 -> Expr Bool -> Arduino ()
myWriteE p b = do
    delayMillisE 100
    digitalWriteE (p+1) (notB b)

twoButtonProg1E :: Arduino ()
twoButtonProg1E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead1E 2
        b <- myRead1E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

twoButtonProg2E :: Arduino ()
twoButtonProg2E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead2E 2
        b <- myRead2E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

twoButtonProg3E :: Arduino ()
twoButtonProg3E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead3E 2
        b <- myRead3E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

