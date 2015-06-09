-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.SamplePrograms.Blink
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module ScheduledBlink where

import Control.Monad (forever)

import Data.Bits (shiftL)
import Data.Word (Word16)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Comm

-- Task which will execute on Arduino, blink on a second, off a second and
-- repeat
myTask :: Port -> Word16 -> Arduino ()
myTask port portVal = do
        digitalPortWrite port portVal
        delay 1000
        digitalPortWrite port 0
        delay 1000
        return ()

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    let led = DigitalPin 13
    let iled = getInternalPin conn led
    let port = pinPort iled
    let portVal = 1 `shiftL` (fromIntegral $ pinPortIndex iled)

    -- Set the pin mode to digital output
    send conn (setPinMode led OUTPUT)
    (tasks,task) <- send conn $ do
        -- Create the task which blinks with a 2 second period
        createTask 1 (myTask port portVal)
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

