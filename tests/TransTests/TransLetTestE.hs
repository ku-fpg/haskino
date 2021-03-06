-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransLetTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Let test example used for rewrite written directly in deep version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransLetTestE where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

transTestProg1E :: Arduino (Expr ())
transTestProg1E = do
    let myWriteE p b = do
        delayMillisE 100
        digitalWriteE (1) (notB b)
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinMode 3 INPUT
    loopE $ do
        a <- do
            delayMillisE 100
            a' <- digitalReadE (1)
            return (notB a')
        myWriteE 13 (a ||* false)
        delayMillisE 1000

transTestProg2E :: Arduino (Expr ())
transTestProg2E = do
    let myReadE p = do
        delayMillisE 101
        a <- digitalReadE (p+1)
        return (notB a)
    let myWriteE p b = do
        delayMillisE 101
        digitalWriteE (1) (notB b)
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loopE $ do
        a <- myReadE 2
        b <- myReadE 3
        myWriteE 13 (a ||* false)
        delayMillisE 1000

transTestProg3E :: Arduino (Expr ())
transTestProg3E = do
    let myRead p = do
        delayMillisE 102
        a <- digitalReadE (p+1)
        return (notB a)
    let myWrite p b = do
        delayMillisE 102
        digitalWriteE (p) (notB b)
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do
        a <- myRead 2
        b <- myRead 3
        myWrite 13 (a ||* false)
        myWrite 14 b
        delayMillisE 1000

