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

{-# NOINLINE rep #-}
rep :: Expr a -> a
rep _ = error "Internal error: repB called"

{-# NOINLINE abs #-}
abs :: ExprB a => a -> Expr a
abs w = lit w

{-# RULES 
    "digitalRead"
    forall (p :: Word8).
    digitalRead p = rep <$> (digitalReadE $ abs p) 
  #-}

{-# RULES "digitalWrite"
    forall (p :: Word8) (b :: Bool).
    digitalWrite p b
      =
    digitalWriteE (abs p) (abs b)
  #-}

{-# RULES "pinMode"
    forall (p :: Word8) m.
    setPinMode p m
      =
    setPinModeE (abs p) m
  #-}

{-# RULES "loop"
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

{-# RULES "abs-push-or"
    forall (b1 :: Bool) (b2 :: Bool).
    abs (b1 || b2)
      =
    (abs b1) ||* (abs b2)
  #-}

{-# RULES "rep-3rd-monad"
    forall (f :: Arduino (Expr a)) (k :: a -> Arduino b).
    rep <$> f >>= k 
      =
    f >>= k . rep
  #-}

{-
{-# RULES "rep-let"
    forall f.
    (\x -> f ).rep
      =
    (\x' -> let x=rep(x') in f)
  #-}
-}

{-# RULES "rep-let"
    forall p s.
    (\x -> digitalWriteE p (s ||* abs(x))).rep
      =
    let x=rep(x') in (\x' -> digitalWriteE p (s ||* abs(x)))
  #-}

{-
{-# RULES "rep-let"
    forall led a.
    (\x -> digitalWriteE (abs(led)) ((abs(a) ||* abs(x)))).rep
      =
    (\x' -> let x=rep(x') in digitalWriteE (abs(led)) ((abs(a) ||* abs(x))))
  #-}
-}

{-# RULES "abs-rep-fuse"
    forall x.
    abs(rep(x))
      =
    x
  #-}

