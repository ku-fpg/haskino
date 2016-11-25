{-# OPTIONS_GHC -fenable-rewrite-rules #-}
{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Strong.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Strong.TwoButton where

import Prelude hiding (abs)

import System.Hardware.Haskino
-- import System.Hardware.Haskino.Rules
import Control.Monad
import Data.Word
import Data.Boolean

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
        digitalWrite led (a || b)

twoButton :: IO ()
twoButton = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

{-
{-# NOINLINE rep #-}
rep :: Expr a -> a
rep _ = error "Internal error: repB called"

{-# NOINLINE abs #-}
abs :: ExprB a => a -> Expr a
abs w = lit w
-}

{-# RULES 
    "digitalRead" [0]
    forall (p :: Word8).
    digitalRead p = rep_ <$> (digitalReadE $ abs_ p) 
  #-}

{-# RULES "digitalWrite" [0]
    forall (p :: Word8) (b :: Bool).
    digitalWrite p b
      =
    digitalWriteE (abs_ p) (abs_ b)
  #-}

{-# RULES "pinMode" [0]
    forall (p :: Word8) m.
    setPinMode p m
      =
    setPinModeE (abs_ p) m
  #-}

{-# RULES "loop" [0]
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

{-# RULES "abs-push-or" [0]
    forall (b1 :: Bool) (b2 :: Bool).
    abs_ (b1 || b2)
      =
    (abs_ b1) ||* (abs_ b2)
  #-}

{-# RULES "rep-3rd-monad" [0]
    forall (f :: Arduino (Expr Bool)) (k :: Bool -> Arduino b).
    rep_ <$> f >>= k 
      =
    f >>= k . rep_
  #-}

{-# RULES "abs-rep-fuse" [1]
    forall x.
    abs_(rep_(x))
      =
    x
  #-}

{-
{-# RULES "rep-let"
    forall f.
    (\x -> f ).rep
      =
    (\x' -> let x=rep(x') in f)
  #-}
-}
{-
{-# RULES "rep-let"
    forall p s.
    (\x -> digitalWriteE p (s ||* abs(x))).rep
      =
    let x=rep(x') in (\x' -> digitalWriteE p (s ||* abs(x)))
  #-}
-}
{-
{-# RULES "rep-let"
    forall led a.
    (\x -> digitalWriteE (abs(led)) ((abs(a) ||* abs(x)))).rep
      =
    (\x' -> let x=rep(x') in digitalWriteE (abs(led)) ((abs(a) ||* abs(x))))
  #-}

-}

