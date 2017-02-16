{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
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
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonE

twoButtonProg :: Arduino ()
twoButtonProg = do
    let led = 13
    let button1 = 2
    let button2 = 3
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        c <- return (a || b)
        digitalWrite led c
        delayMillis 1000

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

-- main :: IO ()
-- main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

-- Phase 2 Rules
-- Command/Procedure shallow->deep rules

{-# RULES 
    "digitalRead" [2]
    forall (p :: Word8).
    digitalRead p = abs_ <$> (digitalReadE $ rep_ p) 
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

{-# RULES "rep-push-not" [1]
    forall (b :: Bool).
    rep_ (not b)
      =
    notB (rep_ b)
  #-}

{-# RULES "abs-3rd-monad" [1]
    forall (f :: Arduino (Expr a)) (k :: a -> Arduino b).
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
