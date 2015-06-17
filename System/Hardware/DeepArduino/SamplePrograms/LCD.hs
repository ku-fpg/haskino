-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.SamplePrograms.LCD
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Basic demo of an Hitachi HD44780 LCD
-------------------------------------------------------------------------------

module System.Hardware.Arduino.SamplePrograms.LCD where

import Control.Monad.Trans (liftIO)
import Data.Char           (isSpace)
import Numeric             (showHex)

import System.Hardware.DeepArduino
import System.Hardware.DeepArduino.Parts.LCD

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
--  <<http://github.com/LeventErkok/hArduino/raw/master/System/Hardware/Arduino/SamplePrograms/Schematics/LCD.png>>
hitachi :: LCDController
-- Connections:                    ARDUINO        Hitachi   Description
--------------------------------   -----------    ---------  ----------------
hitachi = Hitachi44780 { lcdRS   = digital  8  --     4      Register-select
                       , lcdEN   = digital  9  --     6      Enable
                       , lcdD4   = digital  4  --    11      Data 4
                       , lcdD5   = digital  5  --    12      Data 5
                       , lcdD6   = digital  6  --    13      Data 6
                       , lcdD7   = digital  7  --    14      Data 7
                       , lcdBL   = Just (digital 10 ) --     Backlight Control
                       -- Other config variables for the display
                       , lcdRows     = 2    -- 2 rows
                       , lcdCols     = 16    -- of 16 columns
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

-- | Access the LCD connected to Arduino, making it show messages
-- we read from the user and demonstrate other LCD control features offered
-- by hArduino.
lcdDemo :: IO ()
lcdDemo = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    lcd <- lcdRegister conn hitachi
    happySymbol <- lcdCreateSymbol conn lcd happy
    sadSymbol <- lcdCreateSymbol conn lcd sad
    lcdBacklightOn conn lcd
    lcdHome conn lcd
    putStrLn "Hitachi controller demo.."
    putStrLn ""
    putStrLn "Looking for an example? Try the following sequence:"
    putStrLn "    cursor 5 0"
    putStrLn "    happy"
    putStrLn "    write _"
    putStrLn "    happy"
    putStrLn "    flash 5"
    putStrLn ""
    putStrLn "Type ? to see all available commands."
    let help = do let (cmds, args, hlps) = unzip3 $ ("quit", "", "Quit the demo") : [(c, a, h) | (c, (a, h, _)) <- commands]
                      clen = 1 + maximum (map length cmds)
                      alen = 8 + maximum (map length args)
                      pad l s = take l (s ++ repeat ' ')
                      line (c, a, h) = putStrLn $ pad clen c ++ pad alen a ++ h
                  mapM_ line $ zip3 cmds args hlps
        arg0 f _   [] _ = f
        arg0 _ _   a  _ = putStrLn $ "Unexpected arguments: " ++ show a
        arg1 f lcd [] _ = f lcd
        arg1 _ _   a  _ = putStrLn $ "Unexpected arguments: " ++ show a
        arg2 f lcd a  _ = f lcd a
        arg3            = id
        grabNums n a f  = case [v | [(v, "")] <- map reads (words a)] of
                            vs | length vs /= n -> putStrLn $ "Need " ++ show n ++ " numeric parameter" ++ if n == 1 then "." else "s."
                            vs                  -> f vs
        symbol isHappy lcd _ (h, s) = lcdWriteSymbol conn lcd (if isHappy then h else s)
        cursor lcd a = grabNums 2 a (\[col, row] -> lcdSetCursor conn lcd (col, row))
        flash  lcd a = grabNums 1 a (\[n] -> lcdFlash conn lcd n 500)
        code   lcd a = grabNums 1 a (\[n] -> do lcdClear conn lcd
                                                lcdHome conn lcd
                                                lcdWriteSymbol conn lcd (lcdInternalSymbol n)
                                                lcdWrite conn lcd $ " (Code: 0x" ++ showHex n "" ++ ")")
        scroll toLeft lcd a = grabNums 1 a (\[n] -> do let scr | toLeft = lcdScrollDisplayLeft conn
                                                               | True   = lcdScrollDisplayRight conn
                                                       sequence_ $ concat $ replicate n [scr lcd, send conn $ delay 500])
        commands = [ ("?",           ("",        "Display this help message",   arg0 help))
                   , ("clear",       ("",        "Clear the LCD screen",        arg1 (lcdClear conn)))
                   , ("write",       ("string",  "Write to the LCD",            arg2 (lcdWrite conn)))
                   , ("home",        ("",        "Move cursor to home",         arg1 (lcdHome conn)))
                   , ("cursor",      ("col row", "Move cursor to col row",      arg2 cursor))
                   , ("scrollOff",   ("",        "Turn off auto-scroll",        arg1 (lcdAutoScrollOff conn)))
                   , ("scrollOn",    ("",        "Turn on auto-scroll",         arg1 (lcdAutoScrollOn conn)))
                   , ("scrollLeft",  ("n",       "Scroll left by n chars",      arg2 (scroll True)))
                   , ("scrollRight", ("n",       "Scroll right by n char",      arg2 (scroll False)))
                   , ("leftToRight", ("",        "Set left to right direction", arg1 (lcdLeftToRight conn)))
                   , ("rightToLeft", ("",        "Set left to right direction", arg1 (lcdRightToLeft conn)))
                   , ("blinkOn",     ("",        "Set blinking ON",             arg1 (lcdBlinkOn conn)))
                   , ("blinkOff",    ("",        "Set blinking ON",             arg1 (lcdBlinkOff conn)))
                   , ("cursorOn",    ("",        "Display the cursor",          arg1 (lcdCursorOn conn)))
                   , ("cursorOff",   ("",        "Do not display the cursor",   arg1 (lcdCursorOff conn)))
                   , ("displayOn",   ("",        "Turn the display on",         arg1 (lcdDisplayOn conn)))
                   , ("displayOff",  ("",        "Turn the display off",        arg1 (lcdDisplayOff conn)))
                   , ("backlightOn", ("",        "Turn the backlight on",       arg1 (lcdBacklightOn conn)))
                   , ("backlightOff",("",        "Turn the backlight off",      arg1 (lcdBacklightOff conn)))
                   , ("flash",       ("n",       "Flash the display n times",   arg2 flash))
                   , ("happy",       ("",        "Draw a smiling face",         arg3 (symbol True)))
                   , ("sad",         ("",        "Draw a sad face",             arg3 (symbol False)))
                   , ("code",        ("n",       "Write symbol with code n",    arg2 code)) ]
        repl = do putStr "LCD> "
                  m <- getLine
                  case words m of
                    []       -> repl
                    ["quit"] -> do
                        lcdBacklightOff conn lcd
                        closeArduino conn
                    (cmd:_)    -> case cmd `lookup` commands of
                                    Nothing        -> do putStrLn $ "Unknown command '" ++ cmd ++ "', type ? for help."
                                                         repl
                                    Just (_, _, c) -> do c lcd (dropWhile isSpace (drop (length cmd) m)) (happySymbol, sadSymbol)
                                                         repl
    repl

