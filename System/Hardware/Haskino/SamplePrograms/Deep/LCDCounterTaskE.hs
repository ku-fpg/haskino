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
import System.Hardware.Haskino.Utils
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

semId :: Expr Word8
semId = 0

keyTask :: RemoteRef Word16 -> Expr Word8 -> Arduino ()
keyTask ref button = do
    let readButton :: RemoteRef Word16 -> Arduino ()
        readButton r = do
            val <- analogReadE button
            writeRemoteRef r val
    bounceRef <- newRemoteRef $ lit (0::Word16)
    loopE $ do
        writeRemoteRef ref 760
        -- wait for key press
        while ref (\x -> x >=* 760) id $ do
            readButton ref
            delayMillisE 50
        giveSemE semId
        delayMillisE 0
        debugE $ lit $ stringToBytes "After give"
        writeRemoteRef bounceRef 0
        -- wait for key release
        while bounceRef (\x -> x <* 760) id $ do
            readButton bounceRef
            debugE $ lit $ stringToBytes "In Loop"
            delayMillisE 50
        delayMillisE 50

-- | Program which maintains an integer counter, and displays the counter value 
--   on the LCD.  Pressing the Up button increments the counter, pressing the 
--   Down button decrements it
-- 
mainTask :: RemoteRef Word16 -> Arduino ()
mainTask ref = do
    lcd <- lcdRegisterE osepp
    let zero :: Expr Int32
        zero = 0
    cref <- newRemoteRef zero
    lcdBacklightOnE lcd
    lcdWriteE lcd $ showE zero
    loopE $ do
        debugE $ lit $ stringToBytes "Wait"
        takeSemE semId
        debugE $ lit $ stringToBytes "Go"
        key <- readRemoteRef ref
        debugE $ showE key
        ifThenElse (key >=* 30 &&* key <* 150)
            (modifyRemoteRef cref (\x -> x + 1)) (return ())
        ifThenElse (key >=* 150 &&* key <* 360)
            (modifyRemoteRef cref (\x -> x - 1)) (return ())
        count <- readRemoteRef cref
        lcdClearE lcd
        lcdHomeE lcd
        lcdWriteE lcd $ showE count

lcdCounterInit :: Arduino ()
lcdCounterInit =  do
      let button = 0
      setPinModeE button INPUT
      taskRef <- newRemoteRef $ lit (0::Word16)
      createTaskE 1 $ mainTask taskRef
      createTaskE 2 $ keyTask taskRef button
      -- Schedule the task to start immediately
      scheduleTaskE 1 0
      scheduleTaskE 2 0
      debugListen

lcdCounterE :: IO ()
lcdCounterE = withArduino True "/dev/cu.usbmodem1421" $ do
      lcdCounterInit
