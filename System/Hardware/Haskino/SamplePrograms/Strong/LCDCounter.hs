-----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Strong.LCDCounter
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Simple LCD counter application
--
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Strong.LCDCounter where

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.LCD
import Control.Monad.State  (liftIO)
import Data.IORef
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
osepp :: LCDController
osepp = Hitachi44780 { lcdRS = 8
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
data Key = KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect
         | KeyNone
  deriving (Eq)

-- | Initialize the shield. This is essentially simply registering the
-- lcd with the Haskino library. In addition, we return two values to
-- the user:
--
--   * A value to reference the lcd
--
--   * A function to read (if any) key-pressed
initOSepp :: Arduino (LCD, Arduino Key)
initOSepp = do lcd <- lcdRegister osepp
               let button = 0
               setPinMode button INPUT
               -- Analog values obtained from OSEPP site, seems reliable
               let valToKey :: Word16 -> Key
                   valToKey v = if (v < 30) then KeyRight
                                  else if (v < 150) then KeyUp
                                    else if (v < 360) then KeyDown
                                      else if (v < 535) then KeyLeft
                                        else if (v < 760) then KeySelect
                                          else KeyNone
                   readButton = do val <- analogRead button
                                   return $ valToKey val
                   getKeyDown = do key <- readButton
                                   if key == KeyNone then getKeyDown else return key
                   getKeyUp   = do key <- readButton
                                   if key /= KeyNone then getKeyUp else return key
                   getKey = do key <- getKeyDown
                               getKeyUp
                               delayMillis 100
                               return key
               return (lcd, getKey)


-- | Program which maintains an integer counter, and displays the counter value 
--   on the LCD.  Pressing the Up button increments the counter, pressing the 
--   Down button decrements it
-- 
counterProg :: Arduino ()
counterProg = do
      (lcd, getKey) <- initOSepp
      cref <- liftIO $ newIORef 0
      lcdBacklightOn lcd
      lcdWrite lcd $ show 0
      loop $ do
          key <- getKey
          case key of
              KeyRight  -> return ()
              KeyLeft   -> return ()
              KeyUp     -> liftIO $ modifyIORef cref (\x -> x + 1)
              KeyDown   -> liftIO $ modifyIORef cref (\x -> x - 1)
              KeySelect -> return ()
          count <- liftIO $ readIORef cref
          lcdClear lcd
          lcdHome lcd
          lcdWrite lcd $ show count

lcdCounter :: IO ()
lcdCounter = withArduino False "/dev/cu.usbmodem1421" $ do
                counterProg
