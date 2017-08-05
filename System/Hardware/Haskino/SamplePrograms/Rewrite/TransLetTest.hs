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
-- Let test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransLetTest(transLetTest) where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransLetTestE

transTestProg1 :: Arduino ()
transTestProg1 = do
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

transTestProg2 :: Arduino ()
transTestProg2 = do
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
test1 = (show transTestProg1) == (show transTestProg1E)

test2 :: Bool
test2 = (show transTestProg2) == (show transTestProg2E)

transLetTest :: IO ()
transLetTest = do
  putStrLn "Let Translation Test"
  if test1
  then putStrLn "    *** Let Test1 Passed"
  else do
      putStrLn "    *** Let Test1 Failed"
      putStrLn $ show transTestProg1
      putStrLn "    -----------------"
      putStrLn $ show transTestProg1E
  if test2
  then putStrLn "    *** Let Test2 Passed"
  else do
      putStrLn "    *** Let Test2 Failed"
      putStrLn $ show transTestProg2
      putStrLn "    -----------------"
      putStrLn $ show transTestProg2E
