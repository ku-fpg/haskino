-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.ScheduledBlinkE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-- This version is done by creating a scheduled task on the Arduino which
-- blinks the LED on and off without host intervention.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.ScheduledBlinkE where

import Control.Concurrent   (threadDelay)
import Control.Monad.Trans (liftIO)
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

blinkDelay :: Expr Word32
blinkDelay = lit 1000

startDelay :: Expr Word32
startDelay = blinkDelay * lit 5

progDelay :: Int
progDelay = 10500 

-- Task which will execute on Arduino, blink on a second, off a second and
-- repeat
myTask :: Expr Word8 -> Arduino ()
myTask led = 
    loopE $ do 
        digitalWriteE led (lit True)
        delayMillisE blinkDelay
        digitalWriteE led (lit False)
        delayMillisE blinkDelay

scheduledBlinkE :: IO ()
scheduledBlinkE = withArduino True "/dev/cu.usbmodem1421" $ do
    let led = 13
    let tid = 1
    setPinModeE led OUTPUT
    -- Create the task which blinks with a 2 second period
    createTaskE tid (myTask led)
    -- Schedule the task to start in 5 seconds
    scheduleTaskE tid startDelay
    tasks <- queryAllTasksE
    liftIO $ print tasks
    -- Query to confirm task creation
    task <- queryTaskE tid
    liftIO $ print task
    -- Wait 10.5 seconds and delete the task
    -- Note, delayMillis cannot be used here, as it would prevent scheduled
    -- task from running on target.
    liftIO $ print "Delaying 10500 milliseconds"
    liftIO $ threadDelay (progDelay * 1000)
    deleteTaskE tid
    tasks <- queryAllTasksE
    liftIO $ print tasks

