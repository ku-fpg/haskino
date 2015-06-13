-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.ScheduledBlink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-- This version is done by creating a scheduled task on the Arduino which
-- blinks the LED on and off without host intervention.
-------------------------------------------------------------------------------

module ScheduledBlink where

import Control.Monad (forever)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm

-- Task which will execute on Arduino, blink on a second, off a second and
-- repeat
myTask :: Pin -> Arduino ()
myTask led = do
        digitalWrite led True
        delay 1000
        digitalWrite led False
        delay 1000
        return ()

scheduledBlink :: IO ()
scheduledBlink = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = digital 13

    -- Set the pin mode to digital output
    send conn (setPinMode led OUTPUT)
    (tasks,task) <- send conn $ do
        -- Create the task which blinks with a 2 second period
        createTask 1 (myTask led)
        -- Schedule the task to start in 5 seconds
        scheduleTask 1 5000
        ts <- queryAllTasks
        -- Query to confirm task creation
        t <- queryTask 1
        return (ts,t)
    putStrLn $ show (tasks,task)

    -- Wait 10.5 seconds and delete the task
    send conn $ do
        delay 10500
        deleteTask 1
        return ()

