-----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Deep.LCDCounterE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Simple LCD counter application
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Deep.LCDCounterE where

import System.Hardware.Haskino
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Parts.LCDE
import Prelude hiding ( (<*) )
import Data.Boolean
import Data.IORef
import Data.Int
import Data.Word
import Control.Monad.State  (liftIO)

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
data Key = KeyNone
         | KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect
  deriving (Enum)

keyValue :: Key -> Expr Word8
keyValue k = fromIntegral $ fromEnum k

-- | Initialize the shield. This is essentially simply registering the
-- lcd with the Haskino library. In addition, we return two values to
-- the user:
--
--   * A value to reference the lcd
--
--   * A function to read (if any) key-pressed
initOSepp :: Arduino (LCDE, Arduino (Expr Word8))
initOSepp = do lcd <- lcdRegisterE osepp
               let button = 0
               setPinModeE button INPUT
               keyRef <- newRemoteRef (keyValue KeyNone)
               -- Analog values obtained from OSEPP site, seems reliable
               let valToKey :: Expr Word16 -> Expr Word8
                   valToKey v = ifB (v <* 30)  (keyValue KeyRight)
                                  (ifB (v <* 150) (keyValue KeyUp)
                                    (ifB (v <* 360) (keyValue KeyDown)
                                      (ifB (v <* 535) (keyValue KeyLeft)
                                        (ifB (v <* 760) (keyValue KeySelect)
                                          (keyValue KeyNone)))))
                   readButton = do val <- analogReadE button
                                   return $ valToKey val
                   getKey = do writeRemoteRef keyRef (keyValue KeyNone)
                               -- wait for key press
                               while keyRef (\x -> x ==* (keyValue KeyNone)) id $ do
                                   readButton >>= writeRemoteRef keyRef
                               key <- readRemoteRef keyRef
                               -- wait for key release
                               while keyRef (\x -> x ==* key) id $ do
                                   readButton >>= writeRemoteRef keyRef
                               delayMillisE 100
                               return key
               return (lcd, getKey)

-- | Program which maintains an integer counter, and displays the counter value 
--   on the LCD.  Pressing the Up button increments the counter, pressing the 
--   Down button decrements it
-- 
counterProg :: Arduino ()
counterProg = do
      (lcd, getKey) <- initOSepp
      let zero :: Expr Int32
          zero = 0
      cref <- newRemoteRef zero
      lcdBacklightOnE lcd
      lcdWriteE lcd $ showE zero
      loopE $ do
          key <- getKey
          ifThenElse (key ==* (keyValue KeyUp))
              (modifyRemoteRef cref (\x -> x + 1)) (return ())
          ifThenElse (key ==* (keyValue KeyDown))
              (modifyRemoteRef cref (\x -> x - 1)) (return ())
          count <- readRemoteRef cref
          lcdClearE lcd
          lcdHomeE lcd
          lcdWriteE lcd $ showE count

lcdCounterE :: IO ()
lcdCounterE = withArduino True "/dev/cu.usbmodem1421" $ do
      createTaskE 1 counterProg
      -- Schedule the task to start in 1 second
      scheduleTaskE 1 1000
      -- Query to confirm task creation
      task <- queryTask 1
      liftIO $ print task

lcdCounterEProg :: IO ()
lcdCounterEProg = withArduino True "/dev/cu.usbmodem1421" $ do
      createTaskE 1 counterProg
      -- Program the task
      bootTaskE (lit [1::Word8])
      return ()

