{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonLet
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
-- import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFuncE

twoButtonProg1 :: Arduino ()
twoButtonProg1 = do
--    let myRead1 = do
--        delayMillis 100
--        a <- digitalRead (1)
--        return (not a)
    let myWrite p b = do
        delayMillis 100
        digitalWrite (1) (not b)
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do
        a <- do
            delayMillis 100
            a' <- digitalRead (1)
            return (not a')
        myWrite 13 (a || False)
        delayMillis 1000
--    myRead1 :: Word8 -> Arduino Bool
--    myWrite :: Word8 -> Bool -> Arduino ()


twoButtonProg2 :: Arduino ()
twoButtonProg2 = do
    let myRead p = do
        delayMillis 100
        a <- digitalRead (p+1)
        return (not a)
    let myWrite p b = do
        delayMillis 100
        digitalWrite (1) (not b)
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do
        a <- myRead 2
        myWrite 13 (a || False)
        delayMillis 1000


main :: IO ()
main = do
       putStrLn $ show twoButtonProg2

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

{-
{-# RULES "assoc" [2]
    forall (m1 :: Arduino a) (m2 :: Arduino b) (m3 :: b -> Arduino c).
    (m1 >> m2) >>= m3
       =
    m1 >> (m2 >>= m3)
  #-}

{-# RULES "assoc2" [2]
    forall (m1 :: Arduino a) (m2 :: a -> Arduino b) (m3 :: Arduino c).
    m1 >>= (m2 >> m3)
       =
    (m1 >>= m2) >> m3
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

{-# RULES "abs-3rd-monad2" [1]
    forall (f :: Arduino a) (g :: Arduino (Expr b)) (k :: b -> Arduino c).
    (f >> abs_ <$> g) >>= k
      =
    (f >> g) >>= k . abs_
  #-}

{-# RULES "abs-3rd-monad3" [1]
    forall (f :: Arduino a) (g :: a -> Arduino (Expr b)) (k :: b -> Arduino c).
    (f >>= (abs_ <$> g)) >>= k
      =
    (f >>= g) >>= k . abs_
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
