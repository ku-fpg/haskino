{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonLet
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
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonLetE

twoButtonProg1 :: Arduino ()
twoButtonProg1 = do
    let myWrite p b = do
        delayMillis 100
        digitalWrite (1) (not b)
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do
        a <- do
            delayMillis 100
            a' <- digitalRead (1)
            return (not a')
        myWrite 13 (a || False)
        delayMillis 1000


twoButtonProg2 :: Arduino ()
twoButtonProg2 = do
    let myRead p = do
        delayMillis 100
        a <- digitalRead (p+1)
        return (not a)
    let myWrite p b = do
        delayMillis 100
        digitalWrite (1) (not b)
    setPinMode 13 OUTPUT
    setPinMode 2 INPUT
    setPinMode 3 INPUT
    loop $ do
        a <- myRead 2
        myWrite 13 (a || False)
        delayMillis 1000


test1 :: Bool
test1 = (show twoButtonProg1) == (show twoButtonProg1E)

test2 :: Bool
test2 = (show twoButtonProg2) == (show twoButtonProg2E)

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
