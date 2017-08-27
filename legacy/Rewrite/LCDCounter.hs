{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.LCDCounter
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Simple LCD counter application
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import System.Hardware.Haskino.SamplePrograms.Rewrite.LCD
import Data.Int
import Data.Word

-- | The OSepp LCD Shield is a 16x2 LCD using a Hitachi Controller
-- Furthermore, it has backlight, and 5 buttons. The hook-up is
-- quite straightforward, using our existing Hitachi44780 controller
-- as an example. More information on this shield can be found at:
--
--     <http://osepp.com/products/shield-arduino-compatible/16x2-lcd-display-keypad-shield/>
-- Another shield that appears to be the same exact configuration is
-- the SainSmart LCD Keypad Shield. More information on this shield can be found at:
--     <http://www.sainsmart.com/sainsmart-1602-lcd-keypad-shield-for-arduino-duemilanove-uno-mega2560-mega1280.html>
data Osepp = Osepp {
                     oseppLcd     :: LCD
                   , button       :: Word8
                   }

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

-- | There are 5 keys on the OSepp shield.
data Key = KeyNone
         | KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect
  deriving (Enum)

keyValue :: Key -> Word8
keyValue k = fromIntegral $ fromEnum k

getKey :: Word8 -> Arduino Word8
getKey button = do
    key <- analogKey
    waitRelease
    delayMillis 100
    return key
      where
        analogKey :: Arduino Word8
        analogKey = do
            v <- analogRead button
            case v of
              _ | v < 30  -> return (keyValue KeyRight)
              _ | v < 180 -> return (keyValue KeyUp)
              _ | v < 360 -> return (keyValue KeyDown)
              _ | v < 535 -> return (keyValue KeyLeft)
              _ | v < 760 -> return (keyValue KeySelect)
              _           -> analogKey

        waitRelease :: Arduino ()
        waitRelease = do
            v <- analogRead button
            if v < 760 then waitRelease else return ()

-- | Program which maintains an integer counter, and displays the counter value
--   on the LCD.  Pressing the Up button increments the counter, pressing the
--   Down button decrements it
--
counterProg :: Arduino ()
counterProg = do
    lcd <- lcdRegister hitachi
    lcdBacklightOn lcd
    counterLoop lcd 0

counterLoop :: LCD -> Word8 -> Arduino ()
counterLoop lcd button = do
    counterLoop' 0
      where
        counterLoop' :: Int32 -> Arduino ()
        counterLoop' count = do
          lcdClear lcd
          lcdHome lcd
          lcdWrite lcd $ showB count
          key <- getKey button
          if key == keyValue KeyUp
          then counterLoop' $ count + 1
          else if key == keyValue KeyDown
               then counterLoop' $ count - 1
               else counterLoop' count

lcdCounter :: IO ()
lcdCounter = withArduino True "/dev/cu.usbmodem1421" $ do
    counterProg

compile :: IO ()
compile = compileProgram counterProg "lcdCounter.ino"

main :: IO ()
main = compile
