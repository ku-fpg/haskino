{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransTestProg
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Base test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransTest(transTest) where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransTestE

transTestProg :: Arduino ()
transTestProg = do
    let led = 13
    let button1 = 2
    let button2 = 3
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    loop $ do 
        a <- digitalRead button1
        b <- digitalRead button2
        c <- return (a || b)
        digitalWrite led c
        delayMillis 1000

test :: Bool
test = (show transTestProg) == (show transTestProgE)

transTest :: IO ()
transTest = do
  putStrLn "Base Translation Test"
  if test
  then putStrLn "    *** Base Test Passed"
  else do
      putStrLn "     *** Base Test Failed"
      putStrLn $ show transTestProg
      putStrLn "     -----------------"
      putStrLn $ show transTestProgE
