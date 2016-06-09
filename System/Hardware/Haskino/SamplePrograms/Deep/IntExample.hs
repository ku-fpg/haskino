-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.intExample
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- This is an example of using semaphores to communicate between two tasks.
-- One task gives a semaphore then delays for 2 seconds.  The other task
-- waits for the semaphore then blinks the led rapidly 3 times.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.IntExample where

import Prelude hiding ((<*))
import Control.Concurrent   (threadDelay)
import Control.Monad.Trans (liftIO)
import Data.Boolean
import Data.Boolean.Numbers
import Data.Word

import System.Hardware.Haskino

blinkDelay :: Expr Word32
blinkDelay = 125

semId :: Expr Word8
semId = 0

myTask :: Expr Word8 -> Arduino ()
myTask led = 
    loopE $ do
        takeSemE semId
        digitalWriteE led true
        delayMillisE blinkDelay
        digitalWriteE led false
        delayMillisE blinkDelay

intTask :: Arduino ()
intTask = giveSemE semId

initIntExample :: Arduino ()
initIntExample = do
    let led = 13
    setPinModeE led OUTPUT
    let button = 2
    setPinModeE button INPUT
    let myTaskId = 1
    let intTaskId = 2
    -- Create the tasks
    createTaskE myTaskId (myTask led)
    createTaskE intTaskId intTask
    -- Schedule the task to start in 50ms, the second starting after the first
    scheduleTaskE myTaskId 50
    attachIntE button intTaskId FALLING

intExample :: IO ()
intExample = withArduino True "/dev/cu.usbmodem1421" $ do
    initIntExample
    -- Query to confirm task creation
    tasks <- queryAllTasksE
    liftIO $ print tasks
    task1 <- queryTaskE 1
    liftIO $ print task1
    task2 <- queryTaskE 2
    liftIO $ print task2
