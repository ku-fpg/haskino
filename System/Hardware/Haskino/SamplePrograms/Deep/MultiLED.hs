-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.MultiLED
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This is an example of using tasks to light several LEDs.  There are 3 LED's
-- One blinks with a period of 1 second, the 2nd with a period of 2 seconds,
-- and the third with a period of 4 seconds.  They all use the same monadic
-- task structure, it is simply parameterized over the pin number and delay.
-- They also are scheduled to start a differing times.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.MultiLED where

import Control.Monad.Trans (liftIO)
import Data.Boolean
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

ledTask :: Expr Word8 -> Expr Word32 -> Arduino ()
ledTask led delay = do
    setPinModeE led OUTPUT
    loopE $ do
        digitalWriteE led true
        delayMillisE delay
        digitalWriteE led false
        delayMillisE delay

initExample :: Arduino ()
initExample = do
    let led1 = 6
    let led2 = 7
    let led3 = 8
    -- Create the tasks
    createTaskE 1 $ ledTask led1  500
    createTaskE 2 $ ledTask led2 1000
    createTaskE 3 $ ledTask led3 2000
    -- Schedule the tasks to start in 1, 2, and 4 seconds second
    scheduleTaskE 1 1000
    scheduleTaskE 2 2000
    scheduleTaskE 3 4000

-- Execute this function to run program with firmware interpreter
ledExample :: IO ()
ledExample = withArduino True "/dev/cu.usbmodem1421" $ do
    initExample
    -- Query to confirm task creation
    tasks <- queryAllTasksE
    liftIO $ print tasks
    -- Wait for any debug messgaes from Arduino
    debugListen

-- Execute this function to generate C code to be used with the runtime.
compile :: IO ()
compile = compileProgram initExample "multiLED.ino"
