{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
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

myRead :: Word8 -> Arduino Bool
myRead p = do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

myWrite :: Word8 -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite (p+1) (not b)

myRead_2 :: Word8 -> Arduino (Expr Bool)
myRead_2 p = rep_ <$> do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

{-
testit :: Arduino Bool
testit = rep_ <$> (abs_ <$> digitalReadE 4)
-}

twoButtonProg :: Arduino ()
twoButtonProg = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead 2
        b <- myRead 3
        myWrite 13 (a || b)
        delayMillis 1000

main :: IO ()
main = putStrLn $ show twoButtonProg

mainOld :: IO ()
mainOld = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

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

{-# RULES "rep-push-add" [3]
    forall (b1 :: Word8) (b2 :: Word8).
    rep_ (b1 + b2)
      =
    (rep_ b1) + (rep_ b2)
  #-}

{-# RULES "rep-push-or" [3]
    forall (b1 :: Bool) (b2 :: Bool).
    rep_ (b1 || b2)
      =
    (rep_ b1) ||* (rep_ b2)
  #-}

{-# RULES "rep-push-not" [3]
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

{-  Don't really want this
{-# RULES "rep-3rd-monad" [1]
    forall (f :: Arduino a) (k :: Expr a -> Arduino b).
    rep_ <$> f >>= k 
      =
    f >>= k . rep_
  #-}
-}

{-# RULES "rep-then-through" [3]
    forall (f :: Arduino a) (k :: Arduino b).
    rep_ <$> (f >> k) 
      =
    f >> (rep_ <$> k)
  #-}

{-# RULES "rep-monad-return" [3]
    forall (f :: Arduino a) (g :: a -> a).
    rep_ <$> (f >>= (\x -> return (g x)))
      =
    f >>= (\x -> return (rep_ (g x)))
  #-}

{-
{-# RULES "rep-3rd-monad3" [1]
    forall (f :: Arduino a) (g :: b -> b).
    rep_ <$> (f >>= (\x -> return (g x)))
      =
    f >>= (\x -> return (rep_ (g x)))
  #-}

{-# RULES "rep-3rd-monad3" [1]
    forall (f :: Arduino a) (x :: a).
    rep_ <$> (f >>= (return x)) 
      =
    f >>= (return (rep_ x))
  #-}
-}

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
