-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.ScheduledLCD
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Creates a scheduled task on the Arduino which alternates writing 'Rock',
-- 'Chalk' and 'Jayhawk' to the LCD screen every second and a half.
-- Note: This example requires a Mega2560 board, as the Uno boards do not have
-- enough RAM.
-------------------------------------------------------------------------------

module System.Hardware.DeepArduino.SamplePrograms.ScheduledLCD where

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.LCD

hitachi :: LCDController
hitachi = Hitachi44780 { lcdRS = digital 8
                     , lcdEN = digital 9
                     , lcdD4 = digital 4
                     , lcdD5 = digital 5
                     , lcdD6 = digital 6
                     , lcdD7 = digital 7
                     , lcdBL   = Just (digital 10 )
                     , lcdRows = 2
                     , lcdCols = 16
                     , dotMode5x10 = False
                     }

-- Task which will execute on Arduino, write an 'A' to the display, delay a
-- second, write a 'B' to the display, delay a second, and repeat
myTask :: LCD -> Arduino ()
myTask lcd = do
    lcdHome lcd
    lcdWrite lcd "Rock   " 
    delay 1500   
    lcdHome lcd
    lcdWrite lcd "Chalk  " 
    delay 1500   
    lcdHome lcd
    lcdWrite lcd "Jayhawk" 
    delay 1500   

scheduledLCD :: IO ()
scheduledLCD = withArduino True "/dev/cu.usbmodem1421" $ do
        lcd <- lcdRegister hitachi
        lcdBacklightOn lcd
        -- Create the task which writes to the LCD
        createTask 1 (myTask lcd)
        -- Schedule the task to start in 5 seconds
        scheduleTask 1 5000
