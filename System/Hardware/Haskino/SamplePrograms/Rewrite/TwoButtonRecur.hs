{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButton
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
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonE

led = 13
button1 = 2
button2 = 3

-- stuff :: Arduino (Iter a ())
-- stuff = done <$> return ()

blink :: Word8 -> Arduino ()
blink 0 = return ()
blink t = do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blink ( t-1 )

wait :: Arduino ()
wait = do
  b <- digitalRead button1
  if b then return () else wait

recurProg :: Arduino ()
recurProg = do
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    blink 3

{-
blinkf :: Word8 -> Arduino Word8
blinkf 0 = return 0
blinkf t = do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blinkf $ t-1

blink2 = iterLoop blink2I

blink2I :: Word8 -> Arduino (Iter Word8 ())
blink2I 0 = done <$> return ()
blink2I t = done <$> do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blink2 $ return $ t-1
-}
{-
blink2I :: Word8 -> Arduino (Iter Word8 ())
blink2I 0 = done <$> return ()
blink2I t = step <$> do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    return $ t-1
-}
{-
blink2I' :: Word8 -> Arduino (Iter Word8 ())
blink2I' 0 = done <$> doneFn
  where
    doneFn = return ()
blink2I' t = step <$> stepFn
  where
    stepFn = do
      digitalWrite led True
      delayMillis 1000
      digitalWrite led False
      delayMillis 1000
      return $ t-1



blink2' t = do
    while t (\x -> not(x == 0)) (\x -> do
      digitalWrite led True
      delayMillis 1000
      digitalWrite led False
      delayMillis 1000
      return $ x-1)
    return ()

wait :: Arduino ()
wait = do
  b <- digitalRead button1
  if b then return () else wait

wait2 :: Arduino ()
wait2 = wait2' True

wait2' :: Bool -> Arduino ()
wait2' i = do
     b <- digitalRead button1
     if b then return () else wait2' True

wait2I :: Bool -> Arduino (Iter Bool ())
wait2I i = done <$> do
     b <- digitalRead button1
     if b then return () else wait2' True

wait2I' :: Bool -> Arduino (Iter Bool ())
wait2I' i = do
     b <- digitalRead button1
     done <$> if b then return () else wait2' True

wait2I'' :: Bool -> Arduino (Iter Bool ())
wait2I'' i = do
     b <- digitalRead button1
     if b then done <$> return () else done <$> wait2' True

wait2I''' :: Bool -> Arduino (Iter Bool ())
wait2I''' i = do
     b <- digitalRead button1
     if b then done <$> return () else step <$> return True

wait2'' t = do
    while True (\x -> x) (\x -> do
      b <- digitalRead button1
      if b then return False else return True)
    return ()
{-
wait2I' = do
  b <- digitalRead button1
  done $ if b then return () else wait2

wait2I'' = do
  b <- digitalRead button1
  if b then done $ return () else done $ wait2

wait2I''' = do
  b <- digitalRead button1
  if b then done $ return () else step $ return b

wait' = do
  while True (\x -> x) (\x -> do
    b < digitalRead button1
    if b then
  return ()

wait' = do
  while True (\x -> x) (\x -> do
    b < digitalRead button1
    return b)
  return ()
-}

{-
littleLoop :: Arduino ()
littleLoop = do
    digitalWrite led True
    delayMillis 10
    littleLoop
-}
{-
test :: Bool
test = (show twoButtonProg) == (show twoButtonProgE)

main :: IO ()
main = do
  if test
  then putStrLn "*** Test Passed"
  else do
      putStrLn "*** Test Failed"
      putStrLn $ show twoButtonProg
      putStrLn "-----------------"
      putStrLn $ show twoButtonProgE
-}
-}
main :: IO ()
main = do
  putStrLn $ show recurProg


