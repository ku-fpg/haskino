-----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.SamplePrograms.NumGuess
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Simple number guessing game on the OSEPP Keyboard shield.
--
-- /Thanks to David Palmer for lending me his OSEPP shield to play with!/
-------------------------------------------------------------------------------

module System.Hardware.KansasAmber.SamplePrograms.NumGuess where

import System.Hardware.KansasAmber
import System.Hardware.KansasAmber.Parts.LCD

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
osepp = Hitachi44780 { lcdRS = digital 8
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

-- | There are 5 keys on the OSepp shield.
data Key = KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect

-- | Initialize the shield. This is essentially simply registering the
-- lcd with the KansasAmber library. In addition, we return two values to
-- the user:
--
--   * A function to control the back-light
--
--   * A function to read (if any) key-pressed
initOSepp :: Arduino (LCD, Arduino (Maybe Key))
initOSepp = do lcd <- lcdRegister osepp
               let button = analog 0
               setPinMode button ANALOG
               -- Analog values obtained from OSEPP site, seems reliable
               let threshHolds = [ (KeyRight,   30)
                                 , (KeyUp,     150)
                                 , (KeyDown,   360)
                                 , (KeyLeft,   535)
                                 , (KeySelect, 760)
                                 ]
                   readButton = do val <- analogRead button
                                   let walk []            = Nothing
                                       walk ((k, t):keys)
                                         | val < t        = Just k
                                         | True           = walk keys
                                   return $ walk threshHolds
               return (lcd, readButton)

-- | Number guessing game, as a simple LCD demo. User thinks of a number
-- between @0@ and @1000@, and the Arduino guesses it.
numGuess :: LCD -> Arduino (Maybe Key) -> Arduino ()
numGuess lcd readKey = game
  where home  = lcdHome      lcd
        write = lcdWrite     lcd
        clear = lcdClear     lcd
        go    = lcdSetCursor lcd
        at (r, c) s = go (c, r) >> write s
        getKey = do mbK <- readKey
                    case mbK of
                      Nothing -> getKey
                      Just k  -> do delay 500 -- stabilize by waiting 0.5s
                                    return k
        game = do clear
                  home
                  lcdBacklightOn lcd
                  at (0, 2) "KansasAmber!"
                  at (1, 0) "# Guessing game"
                  delay 2000
                  guess 1 0 1000
        newGame = getKey >> game
        guess :: Int -> Int -> Int -> Arduino ()
        guess rnd l h
          | h == l = do clear
                        at (0, 0) $ "It must be: " ++ show h
                        at (1, 0) $ "Guess no: " ++ show rnd
                        newGame
          | h < l = do clear
                       at (0, 0) "You lied!"
                       newGame
          | True  = do clear
                       let g = (l+h) `div` 2
                       at (0, 0) $ "(" ++ show rnd ++ ") Is it " ++ show g ++ "?"
                       k <- getKey
                       case k of
                         KeyUp     -> guess (rnd+1) (g+1) h
                         KeyDown   -> guess (rnd+1) l (g-1)
                         KeySelect -> do at (1, 0) $ "Got it in " ++ show rnd ++ "!"
                                         newGame
                         _         -> do at (1, 0) "Use up/down/select only.."
                                         delay 1000
                                         guess rnd l h

-- | Entry to the classing number guessing game. Simply initialize the
-- shield and call our game function.
guessGame :: IO ()
guessGame = withArduino False "/dev/cu.usbmodem1421" $ do
                 (lcd, readButton) <- initOSepp
                 numGuess lcd readButton
