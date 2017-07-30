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

import Prelude hiding ((<*))
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

myTask2 :: Arduino ()
myTask2 = do
    loopE (do
        let count = lit (3::Word16)
        whileE 0 (\x -> x <* count) (\x -> do 
            whileE 0 (\y -> y <* count) (\y -> do
                analogWriteE 8 (x + y)
                return (y-1)
                )
            return (x-1)))

whileTest :: IO ()
whileTest = withArduino True "/dev/cu.usbmodem1421" $ do
    -- Create the task which writes to the LCD
    myTask2
    debugListen

compile :: IO ()
compile = compileProgram myTask "forInTest.ino"
        

        
