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

{-# NOINLINE rep8 #-}
rep8 :: Expr Word8 -> Arduino Word8
rep8 _ = error "Internal error: rep8 called"

{-# NOINLINE abs8 #-}
abs8 :: Word8 -> Expr Word8
abs8 w = lit w

{-# NOINLINE repB #-}
repB :: Expr Bool -> Bool
repB _ = error "Internal error: repB called"

{-# NOINLINE absB #-}
absB :: Bool -> Expr Bool
absB w = lit w

{-# RULES 
    "digitalRead"
    forall (p :: Word8).
    digitalRead p = repB <$> (digitalReadE $ abs8 p) 
  #-}

{-# RULES "digitalWrite"
    forall (p :: Word8) (b :: Bool).
    digitalWrite p b
      =
    digitalWriteE (abs8 p) (absB b)
  #-}

{-# RULES "pinMode"
    forall (p :: Word8) m.
    setPinMode p m
      =
    setPinModeE (abs8 p) m
  #-}

{-# RULES "loop"
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

{-# RULES "abs-push-or"
    forall (b1 :: Bool) (b2 :: Bool).
    absB (b1 || b2)
      =
    (absB b1) ||* (absB b2)
  #-}

{-# RULES "repB-3rd-monad"
    forall (p :: Expr Word8) (k :: Bool -> Arduino b).
    repB <$> digitalReadE p >>= k 
      =
    digitalReadE p >>= k . repB
  #-}

--{-# RULES "elimM-repB-absB"
--    forall (x :: Expr Bool).
--    (liftM(absB))(liftM(repB)) x = x
--  #-}
--
--{-# RULES "+-intro"
--    forall (x :: Word32) (y :: Word32) .
--    x + y
--      =
--    lit x + lit y
--  #-}

