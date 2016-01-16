-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Shallow.ScheduledLCD
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Creates a scheduled task on the Arduino which alternates writing 'Rock',
-- 'Chalk' and 'Jayhawk' to the LCD screen every second and a half.
-- Note: This example requires a Mega2560 board, as the Uno boards do not have
-- enough RAM.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Shallow.ScheduledLCD where

import Control.Monad.Trans (liftIO)

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.LCD

hitachi :: LCDController
hitachi = Hitachi44780 { lcdRS = 8
                     , lcdEN = 9
                     , lcdD4 = 4
                     , lcdD5 = 5
                     , lcdD6 = 6
                     , lcdD7 = 7
                     , lcdBL   = Just 10
                     , lcdRows = 2
                     , lcdCols = 16
                     , dotMode5x10 = False
                     }

-- Task which will execute on Arduino, write an 'Rock' to the display, delay a
-- second, write a 'Chalk' to the display, delay a second, write a 'Jayhawk'
-- to the display and repeat
myTask :: LCD -> Arduino ()
myTask lcd = do
    lcdHome lcd
    lcdWrite lcd "Rock   " 
    delayMillis 1500   
    lcdHome lcd
    lcdWrite lcd "Chalk  " 
    delayMillis 1500   
    lcdHome lcd
    lcdWrite lcd "Jayhawk" 
    delayMillis 1500   

scheduledLCD :: IO ()
scheduledLCD = withArduino True "/dev/cu.usbmodem1421" $ do
        lcd <- lcdRegister hitachi
        lcdBacklightOn lcd
        -- Create the task which writes to the LCD
        createTask 1 (myTask lcd)
        -- Schedule the task to start in 1 seconds
        scheduleTask 1 1000
        -- Query to confirm task creation
        task <- queryTask 1
        liftIO $ print task
