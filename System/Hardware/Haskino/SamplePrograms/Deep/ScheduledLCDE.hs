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

module System.Hardware.Haskino.SamplePrograms.Deep.ScheduledLCDE where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.LCDE
import Data.Boolean

hitachi :: LCDController
hitachi = Hitachi44780 { lcdRS = 8
                     , lcdEN = 9
                     , lcdD4 = 4
                     , lcdD5 = 5
                     , lcdD6 = 6
                     , lcdD7 = 7
                     , lcdBL = Just 10
                     , lcdRows = 2
                     , lcdCols = 16
                     , dotMode5x10 = false
                     }

-- Task which will execute on Arduino, write an 'Rock' to the display, delay a
-- second, write a 'Chalk' to the display, delay a second, write a 'Jayhawk'
-- to the display and repeat
myTask :: Arduino ()
myTask = do
    lcd <- lcdRegisterE hitachi
    lcdBacklightOnE lcd
    loopE $ do
        lcdHomeE lcd
        lcdWriteE lcd $ litString "Rock   " 
        delayMillisE 1500   
        lcdHomeE lcd
        lcdWriteE lcd $ litString "Chalk  " 
        delayMillisE 1500   
        lcdHomeE lcd
        lcdWriteE lcd $ litString "Jayhawk" 
        delayMillisE 1500   

scheduledLCDE :: IO ()
scheduledLCDE = withArduino True "/dev/cu.usbmodem1421" $ do
    -- Create the task which writes to the LCD
    createTaskE 1 myTask
    -- Schedule the task to start in 1 second
    scheduleTaskE 1 1000
    -- Query to confirm task creation
    task <- queryTask 1
    liftIO $ print task
        
scheduledLCDEProg :: IO ()
scheduledLCDEProg = withArduino True "/dev/cu.usbmodem1421" $ do
    createTaskE 1 myTask
    -- Program the task
    bootTaskE 1
    return ()
        
