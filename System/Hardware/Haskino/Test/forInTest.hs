-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.ScheduledLCDE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Creates a scheduled task on the Arduino which alternates writing 'Rock',
-- 'Chalk' and 'Jayhawk' to the LCD screen every second and a half.
-- Note: This example requires a Mega2560 board, as the Uno boards do not have
-- enough RAM.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.ForInTest where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import Data.Boolean
import Data.Word

-- Task which will execute on Arduino, write an 'Rock' to the display, delay a
-- second, write a 'Chalk' to the display, delay a second, write a 'Jayhawk'
-- to the display and repeat
myTask :: Arduino ()
myTask = do
    loopE (do
        forInE (litString "This is") (\w -> debugE $ showE w)
        return ())

forInTest :: IO ()
forInTest = withArduino True "/dev/cu.usbmodem1421" $ do
    -- Create the task which writes to the LCD
    myTask
    debugListen
        

        
