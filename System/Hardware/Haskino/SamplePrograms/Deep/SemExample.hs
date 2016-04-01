-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.semExample
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This is an example of using semaphores to communicate between two tasks.
-- One task gives a semaphore then delays for 2 seconds.  The other task
-- waits for the semaphore then blinks the led rapidly 3 times.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.SemExample where

import Prelude hiding ((<*))
import Control.Concurrent   (threadDelay)
import Control.Monad.Trans (liftIO)
import Data.Boolean
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

blinkDelay :: Expr Word32
blinkDelay = 125

taskDelay :: Expr Word32
taskDelay = 2000

semId :: Expr Word8
semId = 0

myTask1 :: Expr Word8 -> Arduino ()
myTask1 led = do
    setPinModeE led OUTPUT
    i <- newRemoteRef $ lit (0 :: Word8)
    loopE $ do
        takeSemE semId
        writeRemoteRef i 0
        while i (\x -> x <* 3) (\x -> x + 1) $ do 
            digitalWriteE led true
            delayMillisE blinkDelay
            digitalWriteE led false
            delayMillisE blinkDelay

myTask2 :: Arduino ()
myTask2 = do
    loopCount <- newRemoteRef $ lit (0 :: Word8)
    loopE $ do
        giveSemE semId
        t <- readRemoteRef loopCount
        writeRemoteRef loopCount (t+1)
        debugE $ showE t
        delayMillisE taskDelay

initExample :: Arduino ()
initExample = do
    let led = 13
    -- Create the tasks
    createTaskE 1 (myTask1 led)
    createTaskE 2 myTask2
    -- Schedule the tasks to start in 1 second, the second starting after the first
    scheduleTaskE 1 1000
    scheduleTaskE 2 1050

semExample :: IO ()
semExample = withArduino True "/dev/cu.usbmodem1421" $ do
    initExample
    -- Query to confirm task creation
    tasks <- queryAllTasksE
    liftIO $ print tasks
    task1 <- queryTaskE 1
    liftIO $ print task1
    task2 <- queryTaskE 2
    liftIO $ print task2
    -- Wait for any debug messgaes from Arduino
    debugListen

semExampleProg :: IO ()
semExampleProg = withArduino True "/dev/cu.usbmodem1421" $ do
    initExample
    -- Program the boot tasks
    bootTaskE (lit [1,2])
    return ()

