{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc2b
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc2b where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

myRead1 :: Word8 -> Arduino Bool
myRead1 p = do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

myRead2 :: Word8 -> Arduino Bool
myRead2 p = do
    delayMillis 100
    digitalRead (p+1)

myRead3 :: Word8 -> Arduino Bool
myRead3 p = do
    delayMillis 100
    return True

myWrite :: Word8 -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite (p+1) (not b)

-- main :: IO ()
-- main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

-- Phase 2 Rules
-- Command/Procedure shallow->deep rules
{-
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
-}
-- Phase 1 Rules
-- rep/abs manipulation rules Rules

-- Expr rep rules

{-# RULES "rep-push-add" [1]
    forall (b1 :: Word8) (b2 :: Word8).
    rep_ (b1 + b2)
      =
    (rep_ b1) + (rep_ b2)
  #-}

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
{-
{-# RULES "abs-3rd-monad" [1]
    forall (f :: Arduino (Expr a)) (k :: a -> Arduino b).
    abs_ <$> f >>= k 
      =
    f >>= k . abs_
  #-}
-}
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
