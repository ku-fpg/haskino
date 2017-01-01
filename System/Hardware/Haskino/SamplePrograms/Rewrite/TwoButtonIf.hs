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

import Prelude hiding (abs)

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

twoButtonProg :: Arduino ()
twoButtonProg = do
    let led1 = 13
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

main :: IO ()
main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

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

{-# RULES "delayMillis" [0]
    forall (d :: Word32).
    delayMillis d
      =
    delayMillisE (abs_ d)
  #-}

{-# RULES "loop" [0]
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

{-# RULES "if-then-else" [0]
    forall (b :: Bool) (t :: Arduino ()) (e :: Arduino ()).
    ifThenElseS b t e
      =
    ifThenElse (abs_ b) t e
  #-}

{-# RULES "abs-push-or" [0]
    forall (b1 :: Bool) (b2 :: Bool).
    abs_ (b1 || b2)
      =
    (abs_ b1) ||* (abs_ b2)
  #-}

{-# RULES "abs-push-not" [0]
    forall (b :: Bool).
    abs_ (not b)
      =
    notB (abs_ b)
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
