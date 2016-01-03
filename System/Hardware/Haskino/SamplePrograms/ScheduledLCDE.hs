-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ScheduledLCDE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Creates a scheduled task on the Arduino which alternates writing 'Rock',
-- 'Chalk' and 'Jayhawk' to the LCD screen every second and a half.
-- Note: This example requires a Mega2560 board, as the Uno boards do not have
-- enough RAM.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ScheduledLCDE where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.LCDE

import Data.Boolean

hitachi :: LCDControllerE
hitachi = Hitachi44780E { lcdRSE = 8
                     , lcdENE = 9
                     , lcdD4E = 4
                     , lcdBLE = Just 10
                     , lcdRowsE = 2
                     , lcdColsE = 16
                     , dotMode5x10E = false
                     }

-- Task which will execute on Arduino, write an 'A' to the display, delay a
-- second, write a 'B' to the display, delay a second, and repeat
myTask :: LCDE -> Arduino ()
myTask lcd = do
    lcdHome lcd
    lcdWrite lcd $ litString "Rock   " 
    delayMillisE 1500   
    --lcdHome lcd
    --lcdWrite lcd $ litString "Chalk  " 
    --delayMillisE 1500   
    --lcdHome lcd
    --lcdWrite lcd $ litString "Jayhawk" 
    --delayMillisE 1500   
{-
scheduledLCDE :: IO ()
scheduledLCDE = withArduino True "/dev/cu.usbmodem1421" $ do
        lcd <- lcdRegister hitachi
        lcdBacklightOn lcd
        -- Create the task which writes to the LCD
        createTaskE 1 (myTask lcd)
        -- Schedule the task to start in 5 seconds
        scheduleTaskE 1 5000
        -- Query to confirm task creation
        task <- queryTask 1
        liftIO $ print task
-}

scheduledLCDE :: IO ()
scheduledLCDE = withArduino True "/dev/cu.usbmodem1421" $ do
    lcd <- lcdRegister hitachi
    lcdBacklightOn lcd
    lcdHome lcd
    lcdWrite lcd $ litString "A" 
    delayMillisE 1000
