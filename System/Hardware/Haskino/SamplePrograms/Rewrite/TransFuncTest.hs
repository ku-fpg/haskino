{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransFunTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Function test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTest where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTestE

myRead1 :: Word8 -> Arduino Bool
myRead1 p = do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

myRead2 :: Word8 -> Arduino Bool
myRead2 p = do
    delayMillis 100
    digitalRead (p+1)

myRead3 :: Word8 -> Arduino Bool
myRead3 p = do
    delayMillis 100
    return True

myWrite :: Word8 -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite (p+1) (not b)

transTestProg1 :: Arduino ()
transTestProg1 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead1 2
        b <- myRead1 3
        myWrite 13 (a || b)
        delayMillis 1000

transTestProg2 :: Arduino ()
transTestProg2 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead2 2
        b <- myRead2 3
        myWrite 13 (a || b)
        delayMillis 1000

transTestProg3 :: Arduino ()
transTestProg3 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead3 2
        b <- myRead3 3
        myWrite 13 (a || b)
        delayMillis 1000

test1 :: Bool
test1 = (show transTestProg1) == (show transTestProg1E)

test2 :: Bool
test2 = (show transTestProg2) == (show transTestProg2E)

test3 :: Bool
test3 = (show transTestProg3) == (show transTestProg3E)

transFuncTest :: IO ()
transFuncTest = do
  putStrLn "Function Translation Test"
  if test1
  then putStrLn "    *** Function Test1 Passed"
  else do
      putStrLn "    *** Function Test1 Failed"
      putStrLn $ show transTestProg1
      putStrLn "    -----------------"
      putStrLn $ show transTestProg1E
  if test2
  then putStrLn "    *** Function Test2 Passed"
  else do
      putStrLn "    *** Function Test2 Failed"
      putStrLn $ show transTestProg2
      putStrLn "    -----------------"
      putStrLn $ show transTestProg2E
  if test3
  then putStrLn "    *** Function Test3 Passed"
  else do
      putStrLn "    *** Function Test3 Failed"
      putStrLn $ show transTestProg3
      putStrLn "     -----------------"
      putStrLn $ show transTestProg3E

