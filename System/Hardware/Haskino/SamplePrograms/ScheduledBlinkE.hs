-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ScheduledBlinkE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-- This version is done by creating a scheduled task on the Arduino which
-- blinks the LED on and off without host intervention.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ScheduledBlinkE where

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

blinkDelay :: Expr Word32
blinkDelay = lit 1000

startDelay :: Expr Word32
startDelay = blinkDelay * lit 5

-- ToDo: Fix div
progDelay :: Expr Word32
progDelay = 10500 -- (blinkDelay * lit 10) + (lit 500)
-- progDelay = (blinkDelay * lit 21) `div` (lit 2)

-- Task which will execute on Arduino, blink on a second, off a second and
-- repeat
myTask :: Expr Word8 -> Arduino ()
myTask led = do digitalWriteE led (lit True)
                delayMillisE blinkDelay
                digitalWriteE led (lit False)
                delayMillisE blinkDelay

scheduledBlinkE :: IO ()
scheduledBlinkE = withArduino True "/dev/cu.usbmodem1421" $ do
    let led = lit 13
    let tid = lit 1
    setPinModeE led OUTPUT
    -- Create the task which blinks with a 2 second period
    -- ToDo: Fix E version of createTask
    createTask 1 (myTask led)
    -- Schedule the task to start in 5 seconds
    scheduleTaskE tid startDelay
    tasks <- queryAllTasks
    liftIO $ print tasks
    -- Query to confirm task creation
    task <- queryTaskE tid
    liftIO $ print task
    -- Wait 10.5 seconds and delete the task
    delayMillisE progDelay
    deleteTaskE tid
    tasks <- queryAllTasks
    liftIO $ print tasks

