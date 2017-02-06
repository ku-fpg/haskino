{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIf
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
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
        delayMillis 1000

testWait :: Arduino Bool
testWait = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    if a || b then return $ a || b else return $ a

testWaitWord8 :: Arduino Word8
testWaitWord8 = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    if a || b then return (0::Word8) else return (1::Word8)

testWait2 :: Arduino Bool
testWait2 = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    c <- if a || b then return $ a || b else return $ a
    digitalWrite button1 c
    return c

-- This is what we want testWait to be transformed to
-- Currently we do all but changing types at the bind
-- level.  Perhaps another pass is needed for that?
testWaitE :: Arduino Bool
testWaitE = do
    let button1 = 2
    let button1 = 3
    a <- digitalRead button1
    b <- digitalRead button1
    abs_ <$> ifThenElseE (rep_ (a ||* b)) (rep_ <$> (return $ (a ||* b))) (rep_ <$> (return a))

testCompile :: Arduino ()
testCompile = do
    b <- testWaitE
    return ()

main :: IO ()
main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

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

{-
{-# RULES "if-then-else-unit" [0]
    forall (b :: Bool) (t :: Arduino ()) (e :: Arduino ()).
    ifThenElseUnit b t e
      =
    ifThenElseUnitE (rep_ b) t e
  #-}

{-# RULES "if-then-else-bool" [0]
    forall (b :: Bool) (t :: Arduino Bool) (e :: Arduino Bool).
    ifThenElseBool b t e
      =
    abs_ <$> ifThenElseBoolE (rep_ b) (rep_ <$> t) (rep_ <$> e)
  #-}

-- I expected this general rule to work, but it didn't went 
-- with specific rule above.
{-# RULES "if-then-else-proc" [0]
    forall (b :: Bool) (t :: ArduinoConditional a => Arduino a) (e :: ArduinoConditional a => Arduino a).
    ifThenElse b t e
      =
    abs_ <$> ifThenElseE (rep_ b) (rep_ <$> t) (rep_ <$> e)
  #-}
-}

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
    forall (f :: Arduino (Expr Bool)) (k :: Bool -> Arduino b).
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
