{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransIfTest
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- IfThenElse test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransIfTest where

import Prelude hiding (abs)

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransIfTestE

testTransProg1 :: Arduino ()
testTransProg1 = do
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
        digitalWrite led1 True
        b <- digitalRead button2
        if a || b
        then do
          digitalWrite led1 a
          digitalWrite led2 b
          return (not a)
        else do
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
          a' <- digitalRead led1
          return (a' && b)
        delayMillis 1000

testTransProg2 :: Arduino ()
testTransProg2 = do
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
          return True
        else do
          c <- digitalRead led1
          digitalWrite led1 (not a)
          digitalWrite led2 (not b)
          digitalRead led1
          return c
        delayMillis 1000

testTransProg3 :: Arduino ()
testTransProg3 = do
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

testTransProg4 :: Arduino ()
testTransProg4 = do
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
        digitalWrite led1 True
        b <- digitalRead button2
        c <- if a || b
             then do
               digitalWrite led1 a
               digitalWrite led2 b
               return (not a)
             else do
               digitalWrite led1 (not a)
               digitalWrite led2 (not b)
               a' <- digitalRead led1
               return (a' && b)
        digitalWrite led2 c
        delayMillis 1000

testTransProg5 :: Arduino ()
testTransProg5 = do
    let led1 = 12 :: Word8
    let led2 = 13 :: Word8
    let button1 = 2
    setPinMode led1 OUTPUT
    setPinMode led2 OUTPUT
    setPinMode button1 INPUT
    loop $ do
        a <- digitalRead button1
        if led1 > led2
        then do
          digitalWrite led1 a
        else do
          digitalWrite led1 (not a)
        delayMillis 1000

test1 :: Bool
test1 = (show testTransProg1) == (show testTransProg1E)

test2 :: Bool
test2 = (show testTransProg2) == (show testTransProg2E)

test3 :: Bool
test3 = (show testTransProg3) == (show testTransProg3E)

test4 :: Bool
test4 = (show testTransProg4) == (show testTransProg4E)

transIfTest :: IO ()
transIfTest = do
  putStrLn "IfThenElse Translation Test"
  if test1
  then putStrLn "    *** IfThenElse Test1 Passed"
  else do
      putStrLn "    *** IfThenElse Test1 Failed"
      putStrLn $ show testTransProg1
      putStrLn "    -----------------"
      putStrLn $ show testTransProg1E
  if test2
  then putStrLn "    *** IfThenElse Test2 Passed"
  else do
      putStrLn "    *** IfThenElse Test2 Failed"
      putStrLn $ show testTransProg2
      putStrLn "    -----------------"
      putStrLn $ show testTransProg2E
  if test3
  then putStrLn "    *** IfThenElse Test3 Passed"
  else do
      putStrLn "    *** IfThenElse Test3 Failed"
      putStrLn $ show testTransProg3
      putStrLn "    -----------------"
      putStrLn $ show testTransProg3E
  if test4
  then putStrLn "    *** IfThenElse Test4 Passed"
  else do
      putStrLn "    *** IfThenElse Test4 Failed"
      putStrLn $ show testTransProg4
      putStrLn "    -----------------"
      putStrLn $ show testTransProg4E

