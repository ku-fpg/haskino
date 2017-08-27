{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransMultiTest2.hs
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- MultiModule test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransMultiTest2 where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransMultiTest1
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTestE

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

transMultiTest :: IO ()
transMultiTest = do
  putStrLn "MultiModule Translation Test"
  if test1
  then putStrLn "    *** MultiModule Test1 Passed"
  else do
      putStrLn "    *** MultiModule Test1 Failed"
      putStrLn $ show transTestProg1
      putStrLn "    -----------------"
      putStrLn $ show transTestProg1E
  if test2
  then putStrLn "    *** MultiModule Test2 Passed"
  else do
      putStrLn "    *** MultiModule Test2 Failed"
      putStrLn $ show transTestProg2
      putStrLn "    -----------------"
      putStrLn $ show transTestProg2E
  if test3
  then putStrLn "    *** MultiModule Test3 Passed"
  else do
      putStrLn "    *** MultiModule Test3 Failed"
      putStrLn $ show transTestProg3
      putStrLn "-----------------"
      putStrLn $ show transTestProg3E

