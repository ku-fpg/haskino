{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
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

module Main where

import System.Hardware.Haskino
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
        delayMillis 1000

main :: IO ()
main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

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

{-# RULES "rep-return" [1]
    forall (t :: Bool).
    rep_ <$> return t 
      =
    return $ rep_ t
  #-}

{-# RULES "rep-abs-fuse" [0]
    forall x.
    rep_(abs_(x))
      =
    x
  #-}
