{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurTest
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Recursion test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurLetTest where

import System.Hardware.Haskino
import Control.Monad
import Control.Monad.Fix
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurLetTestE

led = 6
button1 = 2
button2 = 3

recurProg :: Arduino ()
recurProg = do
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    wait
    blink 2
    wait
    blink 3
    return ()
  where
    wait :: Arduino ()
    wait = do
      b <- digitalRead button1
      if b then return () else wait
      where
        wait' = delayMillis $ fromIntegral led

    blink :: Word8 -> Arduino ()
    blink 0 = return ()
    blink t = do
      digitalWrite led True
      delayMillis 1000
      digitalWrite led False
      delayMillis 1000
      blink ( t-1 )

recurProg2 :: Arduino ()
recurProg2 = do
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    wait
    blink 4
    wait
    blink 7
    return ()
  where
    wait :: Arduino ()
    wait = do
      b <- digitalRead (button1 + 1)
      if b then return () else wait
      where
        wait' = delayMillis $ fromIntegral led

    blink :: Word8 -> Arduino ()
    blink 0 = return ()
    blink t = do
      digitalWrite led True
      delayMillis 2000
      digitalWrite led False
      delayMillis 2000
      blink ( t-1 )

test1 :: Bool
test1 = (show recurProg) == (show recurProgE)

transRecurLetTest :: IO ()
transRecurLetTest = do
  putStrLn "Recursion Let Translation Test"
  if test1
  then putStrLn "    *** Recursion Let Test1 Passed"
  else do
      putStrLn "    *** Recursion Let Test1 Failed"
      putStrLn $ show $ recurProg
      putStrLn "    -----------------"
      putStrLn $ show $ recurProgE

