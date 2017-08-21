{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.semExample
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This is an example of using semaphores to communicate between two tasks.
-- One task gives a semaphore then delays for 2 seconds.  The other task
-- waits for the semaphore then blinks the led rapidly 3 times.
-------------------------------------------------------------------------------

module Main where

import Data.Boolean
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

blinkDelay :: Word32
blinkDelay = 125

taskDelay :: Word32
taskDelay = 2000

semId :: Word8
semId = 0

count :: Word32
count = 3

led :: Word8
led = 13

myTask1 :: Word8 -> Arduino ()
myTask1 led = do
    setPinMode led OUTPUT
    myTask1'
  where
    myTask1' :: Arduino ()
    myTask1' = do
        takeSem semId
        myTask1'' 0
        myTask1'

    myTask1'' :: Word32 -> Arduino ()
    myTask1'' x = do
        if x < count then do
            digitalWrite led true
            delayMillis blinkDelay
            digitalWrite led false
            delayMillis blinkDelay
            myTask1'' (x + 1)
        else return ()

myTask2 :: Arduino ()
myTask2 = do
    myTask2' 0
  where
    myTask2' :: Word8 -> Arduino ()
    myTask2' loopCount = do
        giveSem semId
        debug $ showB loopCount
        delayMillis taskDelay
        myTask2' (loopCount + 1)

initExample :: Arduino ()
initExample = do
    -- Create the tasks
    createTask 1 $ myTask1 led
    createTask 2 myTask2
    -- Schedule the tasks to start in 1 second, the second starting after the first
    scheduleTask 1 1000
    scheduleTask 2 1050

semExample :: IO ()
semExample = withArduino True "/dev/cu.usbmodem1421" $ do
    initExample

-- Execute this function to generate C code to be used with the runtime.
compile :: IO ()
compile = compileProgram initExample "semExample.ino"

main :: IO ()
main = semExample
-- main = compile

