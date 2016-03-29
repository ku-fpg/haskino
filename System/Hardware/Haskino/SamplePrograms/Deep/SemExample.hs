-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.semExample
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-- This version is done by creating a scheduled task on the Arduino which
-- blinks the LED on and off without host intervention.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.SemExample where

import Prelude hiding ((<*))
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
    i <- newRemoteRef $ lit (0 :: Word8)
    loopE $ do
        takeSem semId
        writeRemoteRef i 0
        while i (\x -> x <* 3) (\x -> x + 1) $ do 
            digitalWriteE led true
            delayMillisE blinkDelay
            digitalWriteE led false
            delayMillisE blinkDelay

myTask2 :: Arduino ()
myTask2 =
    loopE $ do
        giveSem semId
        delayMillisE taskDelay

semExample :: IO ()
semExample = withArduino True "/dev/cu.usbmodem1421" $ do
    let led = 13
    setPinModeE led OUTPUT
    -- Create the tasks
    createTaskE 1 (myTask1 led)
    createTaskE 2 myTask2
    -- Schedule the tasks to start in 1 second, the second starting after the first
    scheduleTaskE 1 1000
    scheduleTaskE 2 1050
    -- Query to confirm task creation
    tasks <- queryAllTasksE
    liftIO $ print tasks
    task1 <- queryTaskE 1
    liftIO $ print task1
    task2 <- queryTaskE 2
    liftIO $ print task2

