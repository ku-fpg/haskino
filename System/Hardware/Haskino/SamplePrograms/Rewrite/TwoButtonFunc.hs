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

myRead1 :: Word8 -> Arduino Bool
myRead1 p = do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

myRead1E :: Expr Word8 -> Arduino (Expr Bool)
myRead1E p = do
    delayMillisE 100
    a <- digitalReadE (p+1)
    return (notB a)

myRead2 :: Word8 -> Arduino Bool
myRead2 p = do
    delayMillis 100
    digitalRead (p+1)

myRead2E :: Expr Word8 -> Arduino (Expr Bool)
myRead2E p = do
    delayMillis 100
    digitalReadE (p+1)

myRead3 :: Word8 -> Arduino Bool
myRead3 p = do
    delayMillis 100
    return True

myRead3E :: Expr Word8 -> Arduino (Expr Bool)
myRead3E p = do
    delayMillisE 100
    return true

myWrite :: Word8 -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite (p+1) (not b)

myWriteE :: Expr Word8 -> Expr Bool -> Arduino ()
myWriteE p b = do
    delayMillisE 100
    digitalWriteE (p+1) (notB b)

twoButtonProg1 :: Arduino ()
twoButtonProg1 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead1 2
        b <- myRead1 3
        myWrite 13 (a || b)
        delayMillis 1000

twoButtonProg1E :: Arduino ()
twoButtonProg1E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead1E 2
        b <- myRead1E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

twoButtonProg2 :: Arduino ()
twoButtonProg2 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead2 2
        b <- myRead2 3
        myWrite 13 (a || b)
        delayMillis 1000

twoButtonProg2E :: Arduino ()
twoButtonProg2E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead2E 2
        b <- myRead2E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

twoButtonProg3 :: Arduino ()
twoButtonProg3 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead3 2
        b <- myRead3 3
        myWrite 13 (a || b)
        delayMillis 1000

twoButtonProg3E :: Arduino ()
twoButtonProg3E = do
    setPinModeE 13 OUTPUT
    setPinModeE 2 INPUT
    setPinModeE 3 INPUT
    loopE $ do 
        a <- myRead3E 2
        b <- myRead3E 3
        myWriteE 13 (a ||* b)
        delayMillisE 1000

test1 :: Bool
test1 = (show twoButtonProg1) == (show twoButtonProg1E)

test2 :: Bool
test2 = (show twoButtonProg2) == (show twoButtonProg2E)

test3 :: Bool
test3 = (show twoButtonProg3) == (show twoButtonProg3E)

main :: IO ()
main = do
  if test1
  then putStrLn "*** Test1 Passed"
  else do
      putStrLn "*** Test1 Failed"
      putStrLn $ show twoButtonProg1
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg1E
  if test2
  then putStrLn "*** Test2 Passed"
  else do
      putStrLn "*** Test2 Failed"
      putStrLn $ show twoButtonProg2
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg2E
  if test3
  then putStrLn "*** Test3 Passed"
  else do
      putStrLn "*** Test3 Failed"
      putStrLn $ show twoButtonProg3
      putStrLn "-----------------"
      putStrLn $ show twoButtonProg3E

-- main = putStrLn $ show twoButtonProg

-- main :: IO ()
-- main = withArduino True "/dev/cu.usbmodem1421" twoButtonProg

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
{-
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
-}
{-
{-# RULES "rep-return" [1]
    forall (t :: Bool).
    rep_ <$> return t 
      =
    return $ rep_ t
  #-}

{-# RULES "abs-return" [1]
    forall (t :: Expr Bool).
    abs_ <$> return t 
      =
    return $ abs_ t
  #-}
-}
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
