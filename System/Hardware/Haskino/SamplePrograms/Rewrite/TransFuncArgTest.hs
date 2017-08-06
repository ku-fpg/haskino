{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTest
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Function test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTest where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncArgTestE

data PinData = PinData {
                        pin1 :: Word8
                      , pin2 :: Word8
                      , pin3 :: Word8
                       }

myRead1 :: PinData -> Arduino Bool
myRead1 p = do
    delayMillis 100
    a <- digitalRead ((pin1 p)+1)
    return (not a)

myRead2 :: PinData -> Arduino Bool
myRead2 p = do
    delayMillis 100
    digitalRead ((pin2 p)+1)

myRead3 :: PinData -> Arduino Bool
myRead3 p = do
    delayMillis 100
    return True

myWrite :: PinData -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite ((pin3 p)+1) (not b)

ps :: PinData
ps = PinData { pin1 = 2
             , pin2 = 3
             , pin3 = 13
             }

transTestProg1 :: Arduino ()
transTestProg1 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead1 ps
        b <- myRead1 ps
        myWrite ps (a || b)
        delayMillis 1000

transTestProg2 :: Arduino ()
transTestProg2 = do
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do 
        a <- myRead2 ps
        b <- myRead2 ps
        myWrite ps (a || b)
        delayMillis 1000

transTestProg3 :: Arduino ()
transTestProg3 = do
    setPinMode (pin3 ps) OUTPUT
    setPinMode (pin1 ps) INPUT
    setPinMode (pin2 ps) INPUT
    loop $ do 
        a <- myRead2 ps
        b <- myRead3 ps
        myWrite ps (a || b)
        delayMillis 1000

test1 :: Bool
test1 = (show transTestProg1) == show transTestProg1E

test2 :: Bool
test2 = (show transTestProg2) == (show transTestProg2E)

test3 :: Bool
test3 = (show transTestProg3) == (show transTestProg3E)

transFuncArgTest :: IO ()
transFuncArgTest = do
  putStrLn "Function Arg Translation Test"
  if test1
  then putStrLn "    *** Function Arg Test1 Passed"
  else do
      putStrLn "    *** Function Arg Test1 Failed"
      putStrLn $ show transTestProg1
      putStrLn "    -----------------"
      putStrLn $ show transTestProg1E
  if test2
  then putStrLn "    *** Function Arg Test2 Passed"
  else do
      putStrLn "    *** Function Arg Test2 Failed"
      putStrLn $ show transTestProg2
      putStrLn "    -----------------"
      putStrLn $ show transTestProg2E
  if test3
  then putStrLn "    *** Function Arg Test3 Passed"
  else do
      putStrLn "    *** Function Arg Test3 Failed"
      putStrLn $ show transTestProg3
      putStrLn "     -----------------"
      putStrLn $ show transTestProg3E

