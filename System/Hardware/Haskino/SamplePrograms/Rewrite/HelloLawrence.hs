{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
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

module Main where
-- module System.Hardware.Haskino.SamplePrograms.Rewrite.HelloLawrence where

import System.Hardware.Haskino
import System.Hardware.Haskino.Utils
import System.Hardware.Haskino.SamplePrograms.Rewrite.LCD
import Data.Boolean
import Data.Word

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
    lcd <- lcdRegister hitachi
    lcdBacklightOn lcd
    lcdHome lcd
    lcdWriteChar lcd 0x40

main :: IO ()
main = compileProgram myTask "theTest.ino"
