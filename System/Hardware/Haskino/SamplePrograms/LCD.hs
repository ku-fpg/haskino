-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.LCD
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Basic demo of an Hitachi HD44780 LCD
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.LCD where

import Control.Monad.Trans (liftIO)
import Data.Char           (isSpace)
import Numeric             (showHex)

import System.Hardware.Haskino
import System.Hardware.Haskino.Parts.LCD

-- | Connections for a basic hitachi controller.
-- See <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller> for
-- pin layout. For this demo, simply connect the LCD pins to the Arduino
-- as follows:
--
--  * LCD pin @01@ to GND
--
--  * LCD pin @02@ to +5V
--
--  * LCD pin @03@ to a 10K potentiometer's viper
--
--  * LCD pin @04@ to Arduino pin @12@
--
--  * LCD pin @05@ to GND
--
--  * LCD pin @06@ to Arduino pin @11@
--
--  * LCD pin @11@ to Arduino pin @5@
--
--  * LCD pin @12@ to Arduino pin @4@
--
--  * LCD pin @13@ to Arduino pin @3@
--
--  * LCD pin @14@ to Arduino pin @2@
--
--  * [If backlight is needed] LCD pin @15@ to +5V
--
--  * [If backlight is needed] LCD pin @16@ to GND via 220ohm resistor
--
--  <<http://http://github.com/ku-fpg/arduino-lab/raw/master/System/Hardware/Haskino/SamplePrograms/Schematics/LCD.png>>


hitachiDigital :: LCDController
-- Connections:                    ARDUINO        Hitachi   Description
--------------------------------   -----------    ---------  ----------------
hitachiDigital = 
          Hitachi44780 { lcdRS   = 8  --             4      Register-select
                       , lcdEN   = 9  --             6      Enable
                       , lcdD4   = 4  --            11      Data 4
                       , lcdD5   = 5  --            12      Data 5
                       , lcdD6   = 6  --            13      Data 6
                       , lcdD7   = 7  --            14      Data 7
                       , lcdBL   = Just ( 10 )      --    Backlight Control
                       -- Other config variables for the display
                       , lcdRows     = 2    -- 2 rows
                       , lcdCols     = 16    -- of 16 columns
                       , dotMode5x10 = False -- Using the standard 5x8 dots
                       }

hitachiI2C = 
          I2CHitachi44780 {
                         address     = 0x27  -- I2C address of the device
                       , lcdRows     = 4     -- 2 rows
                       , lcdCols     = 20    -- of 16 columns
                       , dotMode5x10 = False -- Using the standard 5x8 dots
                       }

-- | The happy glyph. See 'lcdCreateSymbol' for details on how to create new ones.
happy :: [String]
happy = [ "     "
        , "@   @"
        , "     "
        , "     "
        , "@   @"
        , " @@@ "
        , "     "
        , "     "
        ]

-- | The sad glyph. See 'lcdCreateSymbol' for details on how to create new ones.
sad :: [String]
sad = [ "     "
      , "@   @"
      , "     "
      , "     "
      , "     "
      , " @@@ "
      , "@   @"
      , "     "
      ]

lcdDemoDigital :: IO ()
lcdDemoDigital = runlcdDemo hitachiDigital

lcdDemoI2C :: IO ()
lcdDemoI2C = runlcdDemo hitachiI2C

-- | Access the LCD connected to Arduino, making it show messages
-- we read from the user and demonstrate other LCD control features offered
-- by Haskino.
runlcdDemo :: LCDController -> IO ()
runlcdDemo h = withArduino False "/dev/cu.usbmodem1421" $ do
              lcd <- lcdRegister h
              happySymbol <- lcdCreateSymbol lcd happy
              sadSymbol   <- lcdCreateSymbol lcd sad
              lcdBacklightOn lcd
              lcdHome lcd
              liftIO $ do putStrLn "Hitachi controller demo.."
                          putStrLn ""
                          putStrLn "Looking for an example? Try the following sequence:"
                          putStrLn "    cursor 5 0"
                          putStrLn "    happy"
                          putStrLn "    write _"
                          putStrLn "    happy"
                          putStrLn "    flash 5"
                          putStrLn ""
                          putStrLn "Type ? to see all available commands."
              let repl = do liftIO $ putStr "LCD> "
                            m <- liftIO getLine
                            case words m of
                              []       -> repl
                              ["quit"] -> return ()
                              (cmd:_)    -> case cmd `lookup` commands of
                                              Nothing        -> do liftIO $ putStrLn $ "Unknown command '" ++ cmd ++ "', type ? for help."
                                                                   repl
                                              Just (_, _, c) -> do c lcd (dropWhile isSpace (drop (length cmd) m)) (happySymbol, sadSymbol)
                                                                   repl
              repl
  where help = liftIO $ do let (cmds, args, hlps) = unzip3 $ ("quit", "", "Quit the demo") : [(c, a, h) | (c, (a, h, _)) <- commands]
                               clen = 1 + maximum (map length cmds)
                               alen = 8 + maximum (map length args)
                               pad l s = take l (s ++ repeat ' ')
                               line (c, a, h) = putStrLn $ pad clen c ++ pad alen a ++ h
                           mapM_ line $ zip3 cmds args hlps
        arg0 f _   [] _ = f
        arg0 _ _   a  _ = liftIO $ putStrLn $ "Unexpected arguments: " ++ show a
        arg1 f lcd [] _ = f lcd
        arg1 _ _   a  _ = liftIO $ putStrLn $ "Unexpected arguments: " ++ show a
        arg2 f lcd a  _ = f lcd a
        arg3            = id
        grabNums n a f  = case [v | [(v, "")] <- map reads (words a)] of
                            vs | length vs /= n -> liftIO $ putStrLn $ "Need " ++ show n ++ " numeric parameter" ++ if n == 1 then "." else "s."
                            vs                  -> f vs
        symbol isHappy lcd _ (h, s) = lcdWriteSymbol lcd (if isHappy then h else s)
        cursor lcd a = grabNums 2 a (\[col, row] -> lcdSetCursor lcd (col, row))
        flash  lcd a = grabNums 1 a (\[n] -> lcdFlash lcd n 500)
        code   lcd a = grabNums 1 a (\[n] -> do lcdClear lcd
                                                lcdHome lcd
                                                lcdWriteSymbol lcd (lcdInternalSymbol n)
                                                lcdWrite lcd $ " (Code: 0x" ++ showHex n "" ++ ")")
        scroll toLeft lcd a = grabNums 1 a (\[n] -> do let scr | toLeft = lcdScrollDisplayLeft
                                                               | True   = lcdScrollDisplayRight
                                                       sequence_ $ concat $ replicate n [scr lcd, delayMillis 500])
        commands = [ ("?",           ("",        "Display this help message",   arg0 help))
                   , ("clear",       ("",        "Clear the LCD screen",        arg1 lcdClear))
                   , ("write",       ("string",  "Write to the LCD",            arg2 lcdWrite))
                   , ("home",        ("",        "Move cursor to home",         arg1 lcdHome))
                   , ("cursor",      ("col row", "Move cursor to col row",      arg2 cursor))
                   , ("scrollOff",   ("",        "Turn off auto-scroll",        arg1 lcdAutoScrollOff))
                   , ("scrollOn",    ("",        "Turn on auto-scroll",         arg1 lcdAutoScrollOn))
                   , ("scrollLeft",  ("n",       "Scroll left by n chars",      arg2 (scroll True)))
                   , ("scrollRight", ("n",       "Scroll right by n char",      arg2 (scroll False)))
                   , ("leftToRight", ("",        "Set left to right direction", arg1 lcdLeftToRight))
                   , ("rightToLeft", ("",        "Set left to right direction", arg1 lcdRightToLeft))
                   , ("blinkOn",     ("",        "Set blinking ON",             arg1 lcdBlinkOn))
                   , ("blinkOff",    ("",        "Set blinking ON",             arg1 lcdBlinkOff))
                   , ("cursorOn",    ("",        "Display the cursor",          arg1 lcdCursorOn))
                   , ("cursorOff",   ("",        "Do not display the cursor",   arg1 lcdCursorOff))
                   , ("displayOn",   ("",        "Turn the display on",         arg1 lcdDisplayOn))
                   , ("displayOff",  ("",        "Turn the display off",        arg1 lcdDisplayOff))
                   , ("backlightOn", ("",        "Turn the backlight on",       arg1 lcdBacklightOn))
                   , ("backlightOff",("",        "Turn the backlight off",      arg1 lcdBacklightOff))
                   , ("flash",       ("n",       "Flash the display n times",   arg2 flash))
                   , ("happy",       ("",        "Draw a smiling face",         arg3 (symbol True)))
                   , ("sad",         ("",        "Draw a sad face",             arg3 (symbol False)))
                   , ("code",        ("n",       "Write symbol with code n",    arg2 code))
                   ]
