{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIf
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module Main where

import Prelude hiding (abs)

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIfE

twoButtonProg1 :: Arduino ()
twoButtonProg1 = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinMode led1 OUTPUT
    setPinMode led2 OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    loop $ do 
        a <- digitalRead button1
        digitalWrite led1 True
        b <- digitalRead button2
        if a || b
        then do
          digitalWrite led1 a
          digitalWrite led2 b
          return (not a)
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
          a' <- digitalRead led1
          return (a' && b)
        delayMillis 1000

twoButtonProg2 :: Arduino ()
twoButtonProg2 = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinMode led1 OUTPUT
    setPinMode led2 OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        if a || b
        then do
          digitalWrite led1 a
          digitalWrite led2 b
          return True
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
          digitalRead led1
        delayMillis 1000

twoButtonProg3 :: Arduino ()
twoButtonProg3 = do
    let led1 = 12
    let led2 = 13
    let button1 = 2
    let button2 = 3
    setPinMode led1 OUTPUT
    setPinMode led2 OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        if a || b
        then do
          digitalWrite led1 a
          digitalWrite led2 b
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
        delayMillis 1000

test1 :: Bool
test1 = (show twoButtonProg1) == (show twoButtonProg1E)

test2 :: Bool
test2 = (show twoButtonProg2) == (show twoButtonProg2E)

test3 :: Bool
test3 = (show twoButtonProg3) == (show twoButtonProg3E)

main :: IO ()
main = do
  if test1
  then putStrLn "*** Test1 Passed"
  else do
      putStrLn "*** Test1 Failed"
      putStrLn $ show twoButtonProg1
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg1E
  if test2
  then putStrLn "*** Test2 Passed"
  else do
      putStrLn "*** Test2 Failed"
      putStrLn $ show twoButtonProg2
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg2E
  if test3
  then putStrLn "*** Test3 Passed"
  else do
      putStrLn "*** Test3 Failed"
      putStrLn $ show twoButtonProg3
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg3E

-- main :: IO ()
-- main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

-- Phase 2 Rules
-- Command/Procedure shallow->deep rules

{-# RULES 
    "digitalRead" [2]
    forall (p :: Word8).
    digitalRead p
      = 
    abs_ <$> (digitalReadE $ rep_ p) 
  #-}

{-# RULES "digitalWrite" [2]
    forall (p :: Word8) (b :: Bool).
    digitalWrite p b
      =
    digitalWriteE (rep_ p) (rep_ b)
  #-}

{-# RULES "pinMode" [2]
    forall (p :: Word8) m.
    setPinMode p m
      =
    setPinModeE (rep_ p) m
  #-}

{-# RULES "delayMillis" [2]
    forall (d :: Word32).
    delayMillis d
      =
    delayMillisE (rep_ d)
  #-}

{-# RULES "loop" [2]
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

-- Phase 1 Rules
-- rep/abs manipulation rules Rules

-- Expr rep rules

{-# RULES "rep-push-or" [1]
    forall (b1 :: Bool) (b2 :: Bool).
    rep_ (b1 || b2)
      =
    (rep_ b1) ||* (rep_ b2)
  #-}

{-# RULES "rep-push-and" [1]
    forall (b1 :: Bool) (b2 :: Bool).
    rep_ (b1 && b2)
      =
    (rep_ b1) &&* (rep_ b2)
  #-}

{-# RULES "rep-push-not" [1]
    forall (b :: Bool).
    rep_ (not b)
      =
    notB (rep_ b)
  #-}

{-# RULES "abs-3rd-monad" [1]
    forall (f :: Arduino (Expr Bool)) (k :: Bool -> Arduino b).
    abs_ <$> f >>= k 
      =
    f >>= k . abs_
  #-}

-- Phase 0 Rules
-- Fusion Rules

{-# RULES "rep-abs-fuse" [0]
    forall x.
    rep_(abs_(x))
      =
    x
  #-}

{-# RULES "rep-abs-app-fuse" [0]
    forall (m :: Arduino (Expr Bool)).
    rep_ <$> (abs_ <$> m)
      =
    m
  #-}
