-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Parts.LCD
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- LCD (Liquid Crystal Display) parts supported by hArduino. The Haskell code
-- below has partly been implemented following the Arduino LiquidCrystal project
-- source code: <http://code.google.com/p/arduino/source/browse/trunk/libraries/LiquidCrystal/>
--
-- The Hitachi44780 data sheet is at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>
--
-- For an example program using this library, see "System.Hardware.Arduino.SamplePrograms.LCD".
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module System.Hardware.Arduino.Parts.LCD(
  -- * LCD types and registration
  LCD, LCDController(..), lcdRegister
  -- * Writing text on the LCD
  , lcdClear, lcdWrite
  -- * Moving the cursor
  , lcdHome, lcdSetCursor
  -- * Scrolling
  , lcdAutoScrollOn, lcdAutoScrollOff
  , lcdScrollDisplayLeft, lcdScrollDisplayRight
  -- * Display properties
  , lcdLeftToRight, lcdRightToLeft
  , lcdBlinkOn, lcdBlinkOff
  , lcdCursorOn, lcdCursorOff
  , lcdDisplayOn, lcdDisplayOff
  -- * Accessing internal symbols,
  , LCDSymbol, lcdInternalSymbol, lcdWriteSymbol
  -- Creating custom symbols
  , lcdCreateSymbol
  -- * Misc helpers
  , lcdFlash
  )  where

import Control.Concurrent  (modifyMVar, withMVar)
import Control.Monad       (when)
import Control.Monad.State (gets, liftIO)
import Data.Bits           (testBit, (.|.), (.&.), setBit, clearBit, shiftL, bit)
import Data.Char           (ord, isSpace)
import Data.Maybe          (fromMaybe)
import Data.Word           (Word8)

import qualified Data.Map as M

import System.Hardware.Arduino.Data
import System.Hardware.Arduino.Firmata

import qualified System.Hardware.Arduino.Utils as U

---------------------------------------------------------------------------------------
-- Low level interface, not available to the user
---------------------------------------------------------------------------------------

-- | Commands understood by Hitachi
data Cmd = LCD_INITIALIZE
         | LCD_INITIALIZE_END
         | LCD_FUNCTIONSET
         | LCD_DISPLAYCONTROL Word8
         | LCD_CLEARDISPLAY
         | LCD_ENTRYMODESET   Word8
         | LCD_RETURNHOME
         | LCD_SETDDRAMADDR   Word8
         | LCD_CURSORSHIFT    Word8
         | LCD_SETCGRAMADDR   Word8

-- | Convert a command to a data-word
getCmdVal :: LCDController -> Cmd -> Word8
getCmdVal Hitachi44780{lcdRows, dotMode5x10} = get
  where multiLine -- bit 3
          | lcdRows > 1 = 0x08 :: Word8
          | True        = 0x00 :: Word8
        dotMode   -- bit 2
          | dotMode5x10 = 0x04 :: Word8
          | True        = 0x00 :: Word8
        displayFunction = multiLine .|. dotMode
        get LCD_INITIALIZE         = 0x33
        get LCD_INITIALIZE_END     = 0x32
        get LCD_FUNCTIONSET        = 0x20 .|. displayFunction
        get (LCD_DISPLAYCONTROL w) = 0x08 .|. w
        get LCD_CLEARDISPLAY       = 0x01
        get (LCD_ENTRYMODESET w)   = 0x04 .|. w
        get LCD_RETURNHOME         = 0x02
        get (LCD_SETDDRAMADDR w)   = 0x80 .|. w
        get (LCD_CURSORSHIFT w)    = 0x10 .|. 0x08 .|. w   -- NB. LCD_DISPLAYMOVE (0x08) hard coded here
        get (LCD_SETCGRAMADDR w)   = 0x40 .|. w `shiftL` 3

-- | Initialize the LCD. Follows the data sheet <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>,
-- page 46; figure 24.
initLCD :: LCD -> LCDController -> Arduino ()
initLCD lcd c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7} = do
    debug "Starting the LCD initialization sequence"
    mapM_ (`setPinMode` OUTPUT) [lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7]
    -- Wait for 50ms, data-sheet says at least 40ms for 2.7V version, so be safe
    delay 50
    sendCmd c LCD_INITIALIZE
    delay 5
    sendCmd c LCD_INITIALIZE_END
    sendCmd c LCD_FUNCTIONSET
    lcdCursorOff lcd
    lcdBlinkOff lcd
    lcdLeftToRight lcd
    lcdAutoScrollOff lcd
    lcdHome lcd
    lcdClear lcd
    lcdDisplayOn lcd

-- | Get the controller associated with the LCD
getController :: LCD -> Arduino LCDController
getController lcd = do
  bs  <- gets boardState
  err <- gets bailOut
  liftIO $ withMVar bs $ \bst -> case lcd `M.lookup` lcds bst of
                                   Nothing -> err ("hArduino: Cannot locate " ++ show lcd) []
                                   Just ld -> return $ lcdController ld

-- | Send a command to the LCD controller
sendCmd :: LCDController -> Cmd -> Arduino ()
sendCmd c = transmit False c . getCmdVal c

-- | Send 4-bit data to the LCD controller
sendData :: LCDController -> Word8 -> Arduino ()
sendData lcd n = do debug $ "Transmitting LCD data: " ++ U.showByte n
                    transmit True lcd n

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process.
pulseEnable :: LCDController -> Arduino ()
pulseEnable Hitachi44780{lcdEN} = do
  debug "Sending LCD pulseEnable"
  digitalWrite lcdEN False
  delay 1
  digitalWrite lcdEN True
  delay 1
  digitalWrite lcdEN False
  delay 1

-- | Transmit data down to the LCD
transmit :: Bool -> LCDController -> Word8 -> Arduino ()
transmit mode c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdD5, lcdD6, lcdD7} val = do
  digitalWrite lcdRS mode
  digitalWrite lcdEN False
  let [b7, b6, b5, b4, b3, b2, b1, b0] = [val `testBit` i | i <- [7, 6 .. 0]]
  -- Send down the first 4 bits
  digitalWrite lcdD4 b4
  digitalWrite lcdD5 b5
  digitalWrite lcdD6 b6
  digitalWrite lcdD7 b7
  pulseEnable c
  -- Send down the remaining batch
  digitalWrite lcdD4 b0
  digitalWrite lcdD5 b1
  digitalWrite lcdD6 b2
  digitalWrite lcdD7 b3
  pulseEnable c

-- | Helper function to simplify library programming, not exposed to the user.
withLCD :: LCD -> String -> (LCDController -> Arduino a) -> Arduino a
withLCD lcd what action = do
        debug what
        c <- getController lcd
        action c

---------------------------------------------------------------------------------------
-- High level interface, exposed to the user
---------------------------------------------------------------------------------------

-- | Register an LCD controller. When registration is complete, the LCD will be initialized so that:
--
--   * Set display ON (Use 'lcdDisplayOn' / 'lcdDisplayOff' to change.)
--
--   * Set cursor OFF (Use 'lcdCursorOn' / 'lcdCursorOff' to change.)
--
--   * Set blink OFF  (Use 'lcdBlinkOn' / 'lcdBlinkOff' to change.)
--
--   * Clear display (Use 'lcdClear' to clear, 'lcdWrite' to display text.)
--
--   * Set entry mode left to write (Use 'lcdLeftToRight' / 'lcdRightToLeft' to control.)
--
--   * Set autoscrolling OFF (Use 'lcdAutoScrollOff' / 'lcdAutoScrollOn' to control.)
--
--   * Put the cursor into home position (Use 'lcdSetCursor' or 'lcdHome' to move around.)
lcdRegister :: LCDController -> Arduino LCD
lcdRegister controller = do
  bs <- gets boardState
  lcd <- liftIO $ modifyMVar bs $ \bst -> do
                    let n = M.size $ lcds bst
                        ld = LCDData { lcdDisplayMode    = 0
                                     , lcdDisplayControl = 0
                                     , lcdGlyphCount     = 0
                                     , lcdController     = controller
                                     }
                    return (bst {lcds = M.insert (LCD n) ld (lcds bst)}, LCD n)
  case controller of
     Hitachi44780{} -> initLCD lcd controller
  return lcd

-- | Write a string on the LCD at the current cursor position
lcdWrite :: LCD -> String -> Arduino ()
lcdWrite lcd m = withLCD lcd ("Writing " ++ show m ++ " to LCD") $ \c -> mapM_ (sendData c) m'
   where m' = map (\ch -> fromIntegral (ord ch) .&. 0xFF) m

-- | Clear the LCD
lcdClear :: LCD -> Arduino ()
lcdClear lcd = withLCD lcd "Sending clearLCD" $ \c ->
                 do sendCmd c LCD_CLEARDISPLAY
                    delay 2 -- give some time to make sure LCD is really cleared

-- | Send the cursor to home position
lcdHome :: LCD -> Arduino ()
lcdHome lcd = withLCD lcd "Sending the cursor home" $ \c ->
                do sendCmd c LCD_RETURNHOME
                   delay 2

-- | Set the cursor location. The pair of arguments is the new column and row numbers
-- respectively:
--
--   * The first value is the column, the second is the row. (This is counter-intuitive, but
--     is in line with what the standard Arduino programmers do, so we follow the same convention.)
--
--   * Counting starts at 0 (both for column and row no)
--
--   * If the new location is out-of-bounds of your LCD, we will put it the cursor to the closest
--     possible location on the LCD.
lcdSetCursor :: LCD -> (Int, Int) -> Arduino ()
lcdSetCursor lcd (givenCol, givenRow) = withLCD lcd ("Sending the cursor to Row: " ++ show givenRow ++ " Col: " ++ show givenCol) set
  where set c@Hitachi44780{lcdRows, lcdCols} = sendCmd c (LCD_SETDDRAMADDR offset)
              where align :: Int -> Int -> Word8
                    align i m
                      | i < 0  = 0
                      | i >= m = fromIntegral $ m-1
                      | True   = fromIntegral i
                    col = align givenCol lcdCols
                    row = align givenRow lcdRows
                    -- The magic row-offsets come from various web sources
                    -- I don't follow the logic in these numbers, but it seems to work
                    rowOffsets = [(0, 0), (1, 0x40), (2, 0x14), (3, 0x54)]
                    offset = col + fromMaybe 0x54 (row `lookup` rowOffsets)

-- | Scroll the display to the left by 1 character. Project idea: Using a tilt sensor, scroll the contents of the display
-- left/right depending on the tilt. 
lcdScrollDisplayLeft :: LCD -> Arduino ()
lcdScrollDisplayLeft lcd = withLCD lcd "Scrolling display to the left by 1" $ \c -> sendCmd c (LCD_CURSORSHIFT lcdMoveLeft)
  where lcdMoveLeft = 0x00

-- | Scroll the display to the right by 1 character
lcdScrollDisplayRight :: LCD -> Arduino ()
lcdScrollDisplayRight lcd = withLCD lcd "Scrolling display to the right by 1" $ \c -> sendCmd c (LCD_CURSORSHIFT lcdMoveRight)
  where lcdMoveRight = 0x04

-- | Display characteristics helper, set the new control/mode and send
-- appropriate commands if anything changed
updateDisplayData :: String -> (Word8 -> Word8, Word8 -> Word8) -> LCD -> Arduino ()
updateDisplayData what (f, g) lcd = do
   debug what
   bs  <- gets boardState
   err <- gets bailOut
   (  LCDData {lcdDisplayControl = oldC, lcdDisplayMode = oldM}
    , LCDData {lcdDisplayControl = newC, lcdDisplayMode = newM, lcdController = c})
        <- liftIO $ modifyMVar bs $ \bst ->
                       case lcd `M.lookup` lcds bst of
                         Nothing -> err ("hArduino: Cannot locate " ++ show lcd) []
                         Just ld@LCDData{lcdDisplayControl, lcdDisplayMode}
                            -> do let ld' = ld { lcdDisplayControl = f lcdDisplayControl
                                               , lcdDisplayMode    = g lcdDisplayMode
                                               }
                                  return (bst{lcds = M.insert lcd ld' (lcds bst)}, (ld, ld'))
   when (oldC /= newC) $ sendCmd c (LCD_DISPLAYCONTROL newC)
   when (oldM /= newM) $ sendCmd c (LCD_ENTRYMODESET   newM)

-- | Update the display control word
updateDisplayControl :: String -> (Word8 -> Word8) -> LCD -> Arduino ()
updateDisplayControl what f = updateDisplayData what (f, id)

-- | Update the display mode word
updateDisplayMode :: String -> (Word8 -> Word8) -> LCD -> Arduino ()
updateDisplayMode what g = updateDisplayData what (id, g)

-- | Various control masks for the Hitachi44780
data Hitachi44780Mask = LCD_BLINKON              -- ^ bit @0@ Controls whether cursor blinks
                      | LCD_CURSORON             -- ^ bit @1@ Controls whether cursor is on
                      | LCD_DISPLAYON            -- ^ bit @2@ Controls whether display is on
                      | LCD_ENTRYSHIFTINCREMENT  -- ^ bit @0@ Controls left/right scroll
                      | LCD_ENTRYLEFT            -- ^ bit @1@ Controls left/right entry mode

-- | Convert the mask value to the bit no
maskBit :: Hitachi44780Mask -> Int
maskBit LCD_BLINKON             = 0
maskBit LCD_CURSORON            = 1
maskBit LCD_DISPLAYON           = 2
maskBit LCD_ENTRYSHIFTINCREMENT = 0
maskBit LCD_ENTRYLEFT           = 1

-- | Clear by the mask
clearMask :: Hitachi44780Mask -> Word8 -> Word8
clearMask m w = w `clearBit` maskBit m

-- | Set by the mask
setMask :: Hitachi44780Mask -> Word8 -> Word8
setMask m w = w `setBit` maskBit m

-- | Do not blink the cursor
lcdBlinkOff :: LCD -> Arduino ()
lcdBlinkOff = updateDisplayControl "Turning blinking off" (clearMask LCD_BLINKON)

-- | Blink the cursor
lcdBlinkOn :: LCD -> Arduino ()
lcdBlinkOn = updateDisplayControl "Turning blinking on" (setMask LCD_BLINKON)

-- | Hide the cursor. Note that a blinking cursor cannot be hidden, you must first
-- turn off blinking.
lcdCursorOff :: LCD -> Arduino ()
lcdCursorOff = updateDisplayControl "Not showing the cursor" (clearMask LCD_CURSORON)

-- | Show the cursor
lcdCursorOn :: LCD -> Arduino ()
lcdCursorOn = updateDisplayControl "Showing the cursor" (setMask LCD_CURSORON)

-- | Turn the display off. Note that turning the display off does not mean you are
-- powering it down. It simply means that the characters will not be shown until
-- you turn it back on using 'lcdDisplayOn'. (Also, the contents will /not/ be
-- forgotten when you call this function.) Therefore, this function is useful
-- for temporarily hiding the display contents.
lcdDisplayOff :: LCD -> Arduino ()
lcdDisplayOff = updateDisplayControl "Turning display off" (clearMask LCD_DISPLAYON)

-- | Turn the display on
lcdDisplayOn :: LCD -> Arduino ()
lcdDisplayOn = updateDisplayControl "Turning display on" (setMask LCD_DISPLAYON)

-- | Set writing direction: Left to Right
lcdLeftToRight :: LCD -> Arduino ()
lcdLeftToRight = updateDisplayMode "Setting left-to-right entry mode" (setMask LCD_ENTRYLEFT)

-- | Set writing direction: Right to Left
lcdRightToLeft :: LCD -> Arduino ()
lcdRightToLeft = updateDisplayMode "Setting right-to-left entry mode" (clearMask LCD_ENTRYLEFT)

-- | Turn on auto-scrolling. In the context of the Hitachi44780 controller, this means that
-- each time a letter is added, all the text is moved one space to the left. This can be
-- confusing at first: It does /not/ mean that your strings will continuously scroll:
-- It just means that if you write a string whose length exceeds the column-count
-- of your LCD, then you'll see the tail-end of it. (Of course, this will create a scrolling
-- effect as the string is being printed character by character.)
--
-- Having said that, it is easy to program a scrolling string program: Simply write your string
-- by calling 'lcdWrite', and then use the 'lcdScrollDisplayLeft' and 'lcdScrollDisplayRight' functions
-- with appropriate delays to simulate the scrolling.
lcdAutoScrollOn :: LCD -> Arduino ()
lcdAutoScrollOn = updateDisplayMode "Setting auto-scroll ON" (setMask LCD_ENTRYSHIFTINCREMENT)

-- | Turn off auto-scrolling. See the comments for 'lcdAutoScrollOn' for details. When turned
-- off (which is the default), you will /not/ see the characters at the end of your strings that
-- do not fit into the display.
lcdAutoScrollOff :: LCD -> Arduino ()
lcdAutoScrollOff = updateDisplayMode "Setting auto-scroll OFF" (clearMask LCD_ENTRYSHIFTINCREMENT)

-- | Flash contents of the LCD screen
lcdFlash :: LCD
         -> Int  -- ^ Flash count
         -> Int  -- ^ Delay amount (in milli-seconds)
         -> Arduino ()
lcdFlash lcd n d = sequence_ $ concat $ replicate n [lcdDisplayOff lcd, delay d, lcdDisplayOn lcd, delay d]

-- | An abstract symbol type for user created symbols
newtype LCDSymbol = LCDSymbol Word8

-- | Create a custom symbol for later display. Note that controllers
-- have limited capability for such symbols, typically storing no more
-- than 8. The behavior is undefined if you create more symbols than your
-- LCD can handle.
--
-- The input is a simple description of the glyph, as a list of precisely 8
-- strings, each of which must have 5 characters. Any space character is
-- interpreted as a empty pixel, any non-space is a full pixel, corresponding
-- to the pixel in the 5x8 characters we have on the LCD.  For instance, here's
-- a happy-face glyph you can use:
--
-- >
-- >   [ "     "
-- >   , "@   @"
-- >   , "     "
-- >   , "     "
-- >   , "@   @"
-- >   , " @@@ "
-- >   , "     "
-- >   , "     "
-- >   ]
-- >
lcdCreateSymbol :: LCD -> [String] -> Arduino LCDSymbol
lcdCreateSymbol lcd glyph
  | length glyph /= 8 || any (/= 5) (map length glyph)
  = die "hArduino: lcdCreateSymbol: Invalid glyph description: must be 8x5!" ("Received:" : glyph)
  | True
  = do bs  <- gets boardState
       err <- gets bailOut
       (i, c) <- liftIO $ modifyMVar bs $ \bst ->
                    case lcd `M.lookup` lcds bst of
                      Nothing -> err ("hArduino: Cannot locate " ++ show lcd) []
                      Just ld@LCDData{lcdGlyphCount, lcdController}
                              -> do let ld' = ld { lcdGlyphCount = lcdGlyphCount + 1 }
                                    return (bst{lcds = M.insert lcd ld' (lcds bst)}, (lcdGlyphCount, lcdController))
       sendCmd c (LCD_SETCGRAMADDR i)
       let cvt :: String -> Word8
           cvt s = foldr (.|.) 0 [bit p | (ch, p) <- zip (reverse s) [0..], not (isSpace ch)]
       mapM_ (sendData c . cvt) glyph
       return $ LCDSymbol i

-- | Display a user created symbol on the LCD. (See 'lcdCreateSymbol' for details.)
lcdWriteSymbol :: LCD -> LCDSymbol -> Arduino ()
lcdWriteSymbol lcd (LCDSymbol i) = withLCD lcd ("Writing custom symbol " ++ show i ++ " to LCD") $ \c -> sendData c i

-- | Access an internally stored symbol, one that is not available via its ASCII equivalent. See
-- the Hitachi datasheet for possible values: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>, Table 4 on page 17.
--
-- For instance, to access the symbol right-arrow:
--
--   * Locate it in the above table: Right-arrow is at the second-to-last row, 7th character from left.
--
--   * Check the upper/higher bits as specified in the table: For Right-arrow, upper bits are @0111@ and the
--     lower bits are @1110@; which gives us the code @01111110@, or @0x7E@.
--
--   * So, right-arrow can be accessed by symbol code 'lcdInternalSymbol' @0x7E@, which will give us a 'LCDSymbol' value
--   that can be passed to the 'lcdWriteSymbol' function. The code would look like this: @lcdWriteSymbol lcd (lcdInternalSymbol 0x7E)@.
lcdInternalSymbol :: Word8 -> LCDSymbol
lcdInternalSymbol = LCDSymbol
