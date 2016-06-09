-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.CompileTest
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.CompileTest where

import Data.Boolean
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

myTask :: Arduino ()
myTask = do
  digitalWriteE 2 true
  digitalWriteE 3 false
  let l = lit [1,2,3,4]
  r <- newRemoteRef 0
  forInE l (\x -> modifyRemoteRef r (\a -> a + x))

myTest :: Arduino ()
myTest =  do
  r <- newRemoteRef true
  createTaskE 1 myTask
  a <- millisE
  loopE $ do
    setPinModeE 2 INPUT 
    setPinModeE 3 OUTPUT
    x <- readRemoteRef r
    digitalWriteE 4 x
    b <- millisE
    return ()

runTest :: IO ()
runTest = compileProgram myTest "myTest.ino"

