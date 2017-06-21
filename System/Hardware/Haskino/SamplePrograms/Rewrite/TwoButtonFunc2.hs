{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
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
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc1
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFuncE

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

