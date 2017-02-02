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
          return a
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
          return b
        delayMillis 1000

testWait :: Arduino Bool
testWait = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    if a || b then return $ a || b else return $ a

testWait2 :: Arduino Bool
testWait2 = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    ifThenElse  (a || b) (return $ a || b) (return $ a)

-- This is what we want testWait to be transformed to
-- Currently we do all but changing types at the bind
-- level.  Perhaps another pass is needed for that?
testWaitE :: Arduino Bool
testWaitE = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    rep_ <$> ifThenElseE (abs_ (a ||* b)) (abs_ <$> (return $ (a ||* b))) (abs_ <$> (return a))

testCompile :: Arduino ()
testCompile = do
    b <- testWaitE
    return ()

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

{-# RULES "if-then-else-unit" [0]
    forall (b :: Bool) (t :: Arduino ()) (e :: Arduino ()).
    ifThenElseUnit b t e
      =
    ifThenElseUnitE (abs_ b) t e
  #-}

{-
{-# RULES "if-then-else-bool" [0]
    forall (b :: Bool) (t :: Arduino Bool) (e :: Arduino Bool).
    ifThenElseBool b t e
      =
    rep_ <$> ifThenElseBoolE (abs_ b) (abs_ <$> t) (abs_ <$> e)
  #-}
-}

-- I expected this general rule to work, but it didn't went 
-- with specific rule above.
{-# RULES "if-then-else-proc" [0]
    forall (b :: Bool) (t :: ArduinoConditional a => Arduino a) (e :: ArduinoConditional a => Arduino a).
    ifThenElse b t e
      =
    rep_ <$> ifThenElseE (abs_ b) (abs_ <$> t) (abs_ <$> e)
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

{-# RULES "abs-return" [0]
    forall (t :: Bool).
    abs_ <$> return t 
      =
    return $ abs_ t
  #-}

{-# RULES "abs-rep-fuse" [1]
    forall x.
    abs_(rep_(x))
      =
    x
  #-}
