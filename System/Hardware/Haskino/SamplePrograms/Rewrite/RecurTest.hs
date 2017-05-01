{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.RecurTest
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import Control.Monad
import Control.Monad.Fix
import Data.Word
import Data.Boolean

led = 6
button1 = 2

wait :: Arduino ()
wait = do
    b <- digitalRead button1
    if b then return () else wait

blink :: Word8 -> Arduino ()
blink 0 = return ()
blink t = do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blink ( t-1 )

recurProg :: Arduino ()
recurProg = do
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    wait
    blink 3
    return ()

main :: IO ()
main = do
    compileProgram recurProg "recurTest.ino"

