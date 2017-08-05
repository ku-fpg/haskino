-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransFunTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Function test example used for rewrite written directly in deep version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTestE where

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

myWriteE :: Expr Word8 -> Expr Bool -> Arduino (Expr ())
myWriteE p b = do
    delayMillisE 100
    digitalWriteE (p+1) (notB b)

transTestProg1E :: Arduino (Expr ())
transTestProg1E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do
        a <- myRead1E 2
        b <- myRead1E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

transTestProg2E :: Arduino (Expr ())
transTestProg2E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do
        a <- myRead2E 2
        b <- myRead2E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

transTestProg3E :: Arduino (Expr ())
transTestProg3E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do
        a <- myRead3E 2
        b <- myRead3E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

