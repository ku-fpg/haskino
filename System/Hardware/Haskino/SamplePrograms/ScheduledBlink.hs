-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ScheduledBlink
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-- This version is done by creating a scheduled task on the Arduino which
-- blinks the LED on and off without host intervention.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ScheduledBlink where

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino

-- Task which will execute on Arduino, blink on a second, off a second and
-- repeat
myTask :: Pin -> Arduino ()
myTask led = do
        digitalWrite led True
        delayMillis 1000
        digitalWrite led False
        delayMillis 1000
        return ()

scheduledBlink :: IO ()
scheduledBlink = withArduino True "/dev/cu.usbmodem1421" $ do
    let led = 13
    setPinMode led OUTPUT
    -- Create the task which blinks with a 2 second period
    createTask 1 (myTask led)
    -- Schedule the task to start in 5 seconds
    scheduleTask 1 5000
    tasks <- queryAllTasks
    liftIO $ print tasks
    -- Query to confirm task creation
    task <- queryTask 1
    liftIO $ print task
    -- Wait 10.5 seconds and delete the task
    delayMillis 10500
    deleteTask 1
    tasks <- queryAllTasks
    liftIO $ print tasks

