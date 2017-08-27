-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Function test example used for rewrite written directly in deep version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTestE(transTestProg1E
                                                                      , transTestProg2E
                                                                      , transTestProg3E 
                                                                      ) where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
-- import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTestE

data PinData = PinData {
                        pin1 :: Expr Word8
                      , pin2 :: Expr Word8
                      , pin3 :: Expr Word8
                       }

myRead1 :: PinData -> Arduino (Expr Bool)
myRead1 p = do
    delayMillisE 100
    a <- digitalReadE ((pin1 p)+1)
    return (notB a)

myRead2 :: PinData -> Arduino (Expr Bool)
myRead2 p = do
    delayMillisE 100
    digitalReadE ((pin2 p)+1)

myRead3 :: PinData -> Arduino (Expr Bool)
myRead3 p = do
    delayMillisE 100
    return true

myWrite :: PinData -> (Expr Bool) -> Arduino (Expr ())
myWrite p b = do
    delayMillisE 100
    digitalWriteE ((pin3 p)+1) (notB b)

ps :: PinData
ps = PinData { pin1 = 2
             , pin2 = 3
             , pin3 = 13
             }

transTestProg1E :: Arduino (Expr ())
transTestProg1E = do
    setPinModeE (pin3 ps) OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead1 ps
        b <- myRead1 ps
        myWrite ps (a ||* b)
        delayMillisE 1000

transTestProg2E :: Arduino (Expr ())
transTestProg2E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead2 ps
        b <- myRead2 ps
        myWrite ps (a ||* b)
        delayMillisE 1000

transTestProg3E :: Arduino (Expr ())
transTestProg3E = do
    setPinModeE (pin3 ps) OUTPUT
    setPinModeE (pin1 ps) INPUT
    setPinModeE (pin2 ps) INPUT
    loopE $ do 
        a <- myRead2 ps
        b <- myRead3 ps
        myWrite ps (a ||* b)
        delayMillisE 1000
