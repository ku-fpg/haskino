-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Parts.LCD
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- LCD (Liquid Crystal Display) parts supported by Haskino. The Haskell code
-- below has partly been implemented following the Arduino LiquidCrystal project
-- source code: <http://code.google.com/p/arduino/source/browse/trunk/libraries/LiquidCrystal/>
--
-- The Hitachi44780 data sheet is at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>
--
-- For an example program using this library, see "System.Hardware.Arduino.SamplePrograms.LCD".
-------------------------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

module System.Hardware.Haskino.Parts.LCDE(
  -- * LCD types and registration
  LCDE, LCDController(..), lcdRegisterE
  -- * Writing text on the LCD
  , lcdClearE, lcdWriteE, lcdWriteCharE
  -- * Moving the cursor
  , lcdHomeE, lcdSetCursorE
  -- * Scrolling
  , lcdAutoScrollOnE, lcdAutoScrollOffE
  , lcdScrollDisplayLeftE, lcdScrollDisplayRightE
  -- * Display properties
  , lcdLeftToRightE, lcdRightToLeftE
  , lcdBlinkOnE, lcdBlinkOffE
  , lcdCursorOnE, lcdCursorOffE
  , lcdDisplayOnE, lcdDisplayOffE
  -- * Accessing internal symbols,
  -- , LCDSymbol, lcdInternalSymbol, lcdWriteSymbol
  -- Creating custom symbols
  -- , lcdCreateSymbol
  -- * Misc helpers
  , lcdFlashE, lcdBacklightOnE, lcdBacklightOffE
  )  where

import Control.Monad       (when)
import Control.Monad.State (gets, liftIO)
-- import Data.Bits           (testBit, (.|.), (.&.), setBit, clearBit, shiftL, bit, complement)
import Data.Char           (ord, isSpace)
import Data.Maybe          (fromMaybe, isJust)
import Data.Word           (Word8, Word32)
import qualified Data.Bits as B
import Data.Boolean
import Data.Boolean.Bits

import qualified Data.Map as M

import System.Hardware.Haskino.Comm
import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Protocol

import qualified System.Hardware.Haskino.Utils as U

---------------------------------------------------------------------------------------
-- Low level interface, not available to the user
---------------------------------------------------------------------------------------

-- | Commands understood by Hitachi
data Cmd = LCD_INITIALIZE
         | LCD_INITIALIZE_END
         | LCD_FUNCTIONSET
         | LCD_DISPLAYCONTROL (Expr Word8)
         | LCD_CLEARDISPLAY
         | LCD_ENTRYMODESET   (Expr Word8)
         | LCD_RETURNHOME
         | LCD_SETDDRAMADDR   (Expr Word8)
         | LCD_CURSORSHIFT    (Expr Word8)
         | LCD_SETCGRAMADDR   (Expr Word8)
         | LCD_NOOP

-- | Convert a command to a data-word
getCmdVal :: LCDController -> Cmd -> Expr Word8
getCmdVal c cmd = get cmd
  where multiLine -- bit 3
          | (lcdRows c) > 1 = 0x08 :: Word8
          | True        = 0x00 :: Word8
        dotMode   -- bit 2
          | (dotMode5x10 c) = 0x04 :: Word8
          | True        = 0x00 :: Word8
        displayFunction = multiLine B..|. dotMode
        get :: Cmd -> Expr Word8
        get LCD_NOOP               = 0x00
        get LCD_INITIALIZE         = 0x33
        get LCD_INITIALIZE_END     = 0x32
        get LCD_FUNCTIONSET        = lit $ 0x20 B..|. displayFunction
        get (LCD_DISPLAYCONTROL w) = 0x08 .|. w
        get LCD_CLEARDISPLAY       = 0x01
        get (LCD_ENTRYMODESET w)   = 0x04 .|. w
        get LCD_RETURNHOME         = 0x02
        get (LCD_SETDDRAMADDR w)   = 0x80 .|. w
        get (LCD_CURSORSHIFT w)    = 0x10 .|. 0x08 .|. w   -- NB. LCD_DISPLAYMOVE (0x08) hard coded here
        get (LCD_SETCGRAMADDR w)   = 0x40 .|. w `shiftL` 3

-- | Initialize the LCD. Follows the data sheet <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>,
-- page 46; figure 24.
initLCD :: LCDE -> Arduino ()
initLCD lcd = do
    let c = lcdControllerE lcd
    debug "Starting the LCD initialization sequence"
    case c of 
        Hitachi44780{} -> initLCDDigital c
        I2CHitachi44780{} -> i2cConfig
    -- Wait for 50ms, data-sheet says at least 40ms for 2.7V version, so be safe
    delayMillisE 50
    sendCmd lcd c LCD_INITIALIZE
    delayMillisE 5
    sendCmd lcd c LCD_INITIALIZE_END
    sendCmd lcd c LCD_FUNCTIONSET
    lcdCursorOffE lcd
    lcdBlinkOffE lcd
    lcdLeftToRightE lcd
    lcdAutoScrollOffE lcd
    lcdHomeE lcd
    lcdClearE lcd
    lcdDisplayOnE lcd

-- | Initialize the LCD. Follows the data sheet <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>,
-- page 46; figure 24.
initLCDDigital :: LCDController -> Arduino ()
initLCDDigital c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdBL} = do
    if isJust lcdBL then let Just p = lcdBL in setPinModeE (lit p) OUTPUT else return ()
    mapM_ (`setPinModeE` OUTPUT) [lit lcdRS, lit lcdEN, lit lcdD4, 
                                  lit $ lcdD4 + 1, lit $ lcdD4 + 2, lit $ lcdD4 + 3]

-- | Send a command to the LCD controller
sendCmd :: LCDE -> LCDController -> Cmd -> Arduino ()
sendCmd lcd c = transmit false lcd c . getCmdVal c

-- | Send 4-bit data to the LCD controller
sendData :: LCDE-> LCDController -> Expr Word8 -> Arduino ()
sendData lcd c n = do debug $ "Transmitting LCD data: " ++ show n
                      transmit true lcd c n

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process - Done with Digtial writes
pulseEnableDig :: LCDController -> Arduino ()
pulseEnableDig Hitachi44780{lcdEN} = do
  debug "Sending LCD pulseEnable"
  digitalWriteE (lit lcdEN) false
  delayMillisE 1
  digitalWriteE (lit lcdEN) true
  delayMillisE 1
  digitalWriteE (lit lcdEN) false
  delayMillisE 1

-- | Transmit data down to the LCD
transmit :: Expr Bool -> LCDE -> LCDController -> Expr Word8 -> Arduino ()
transmit mode lcd c val = do
  case c of
    Hitachi44780{}    -> transmitDig mode c val
    -- I2CHitachi44780{} -> transmitI2C mode lcd c val

-- | Transmit data down to the LCD digital writes
transmitDig :: Expr Bool -> LCDController -> Expr Word8 -> Arduino ()
transmitDig mode c@Hitachi44780{lcdRS, lcdEN, lcdD4} val = do
  digitalWriteE (lit lcdRS) mode
  digitalWriteE (lit lcdEN) false
  -- Send down the first 4 bits
  digitalPortWriteE (lit lcdD4) (val `shiftR` 4) 0x0F
  pulseEnableDig c
  -- Send down the remaining batch
  digitalPortWriteE (lit lcdD4) (val .&. 0x0F) 0x0F
  pulseEnableDig c

{-
data LCD_I2C_Bits =
  LCD_I2C_BACKLIGHT | 
  LCD_I2C_ENABLE |
  LCD_I2C_RS

lcdI2CBitsToVal :: LCD_I2C_Bits -> Word8
lcdI2CBitsToVal LCD_I2C_BACKLIGHT = 8
lcdI2CBitsToVal LCD_I2C_ENABLE    = 4
lcdI2CBitsToVal LCD_I2C_RS        = 1

-- | Transmit data down to the I2CLCD using I2C writes
transmitI2C :: Expr Bool -> LCDE -> LCDController -> Expr Word8 -> Arduino ()
transmitI2C mode lcd c@I2CHitachi44780{address} val = do
    lcdd <- liftIO $ readMVar (lcdState lcd)
    let bl = if lcdBacklightState lcdd 
                then lcdI2CBitsToVal LCD_I2C_BACKLIGHT
                else 0
        lors = lo .|. rs .|. bl
        hirs = hi .|. rs .|. bl
    i2cWrite address [hirs]
    pulseEnableI2C c hirs
    i2cWrite address [lors]
    pulseEnableI2C c lors
  where rs = if mode then lcdI2CBitsToVal LCD_I2C_RS else 0
        lo =  (val `shiftL` 4) .&. 0xF0    -- lower four bits
        hi =  val .&. 0xF0                 -- upper four bits

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process - Done with I2C writes
pulseEnableI2C :: LCDController -> Word8 -> Arduino ()
pulseEnableI2C c@I2CHitachi44780{address} d = do
    i2cWrite address [d .|. en]
    delayMillisE 1
    i2cWrite address [d .&. (complement en)]
    delayMillisE 1
  where
    en = lcdI2CBitsToVal LCD_I2C_ENABLE
-}

-- | Helper function to simplify library programming, not exposed to the user.
withLCD :: LCDE -> String -> (LCDController -> Arduino a) -> Arduino a
withLCD lcd what action = do
        let c = lcdControllerE lcd 
        debug what
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
lcdRegisterE :: LCDController -> Arduino LCDE
lcdRegisterE controller = do
    mode <- newRemoteRef 0
    control <- newRemoteRef 0
    count <- newRemoteRef 0
    backlight <- newRemoteRef true
    let ld = LCDDataE { lcdDisplayModeE    = mode
                      , lcdDisplayControlE = control
                      , lcdGlyphCountE     = count
                      , lcdBacklightStateE = backlight
                      }
    let c = LCDE { lcdControllerE = controller
                 , lcdStateE = ld
                 }
    initLCD c
    return c

-- | Turn backlight on if there is one, otherwise do nothing
lcdBacklightOnE :: LCDE -> Arduino ()
lcdBacklightOnE lcd = lcdBacklight lcd true

-- | Turn backlight off if there is one, otherwise do nothing
lcdBacklightOffE :: LCDE -> Arduino ()
lcdBacklightOffE lcd = lcdBacklight lcd false

-- | Turn backlight on/off if there is one, otherwise do nothing
lcdBacklight :: LCDE -> Expr Bool -> Arduino ()
lcdBacklight lcd on = do
   let lcdc = lcdControllerE lcd
   case lcdc of 
      Hitachi44780{} -> do
        let bl = lcdBL lcdc
        if isJust bl 
            then let Just p = bl in digitalWriteE (lit p) on
            else return()
{-
      I2CHitachi44780E{} -> do
        let lcds = lcdState lcd
        liftIO $ modifyMVar lcds $ \lcdst -> do
            let lcdst' =  lcdst { lcdBacklightState = on}
            return (lcdst', lcdst')
        -- Send a noop so backlight state line gets updated
        sendCmd lcd lcdc LCD_NOOP
-}

-- | Write a string on the LCD at the current cursor position
lcdWriteE :: LCDE -> Expr [Word8] -> Arduino ()
lcdWriteE lcd ws = withLCD lcd ("Writing " ++ show ws ++ " to LCD") $ \c -> writeWs c
   where writeWs c = forInE ws (\w -> sendData lcd c w)

-- | Write a string on the LCD at the current cursor position
lcdWriteCharE :: LCDE -> Expr Word8 -> Arduino ()
lcdWriteCharE lcd w = withLCD lcd ("Writing " ++ show w ++ " to LCD") $ \c -> sendData lcd c w

-- | Clear the LCD
lcdClearE :: LCDE -> Arduino ()
lcdClearE lcd = withLCD lcd "Sending clearLCD" $ \c ->
                 do sendCmd lcd c LCD_CLEARDISPLAY
                    delayMillisE 2 -- give some time to make sure LCD is really cleared

-- | Send the cursor to home position
lcdHomeE :: LCDE -> Arduino ()
lcdHomeE lcd = withLCD lcd "Sending the cursor home" $ \c ->
                 do sendCmd lcd c LCD_RETURNHOME
                    delayMillisE 2

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
lcdSetCursorE :: LCDE -> Expr Word8 -> Expr Word8 -> Arduino ()
lcdSetCursorE lcd givenCol givenRow = withLCD lcd ("Sending the cursor to Row: " ++ show givenRow ++ " Col: " ++ show givenCol) set
  where set c = sendCmd lcd c (LCD_SETDDRAMADDR offset)
              where align :: Expr Word8 -> Expr Word8 -> Expr Word8
                    align i m = ifB (i >=* m) (m-1) i
                    col = align givenCol $ lit $ lcdCols c
                    row = align givenRow $ lit $ lcdRows c
                    -- The magic row-offsets come from various web sources
                    -- I don't follow the logic in these numbers, but it seems to work
                    offset = col + ifB (row ==* 0) 0
                                        (ifB (row ==* 1) 0x40 
                                              (ifB (row ==* 2) 0x14 0x54))

-- | Scroll the display to the left by 1 character. Project idea: Using a tilt sensor, scroll the contents of the display
-- left/right depending on the tilt. 
lcdScrollDisplayLeftE :: LCDE -> Arduino ()
lcdScrollDisplayLeftE lcd = withLCD lcd "Scrolling display to the left by 1" $ \c -> sendCmd lcd c (LCD_CURSORSHIFT lcdMoveLeft)
  where lcdMoveLeft = 0x00

-- | Scroll the display to the right by 1 character
lcdScrollDisplayRightE :: LCDE -> Arduino ()
lcdScrollDisplayRightE lcd = withLCD lcd "Scrolling display to the right by 1" $ \c -> sendCmd lcd c (LCD_CURSORSHIFT lcdMoveRight)
  where lcdMoveRight = 0x04

-- | Update the display control word
updateDisplayControl :: String -> (Expr Word8 -> Expr Word8) -> LCDE -> Arduino ()
updateDisplayControl what f lcd = do 
  let c = lcdControllerE lcd
  let lcds = lcdStateE lcd
  debug what
  modifyRemoteRef (lcdDisplayControlE lcds) f
  newC <- readRemoteRef (lcdDisplayControlE lcds)
  sendCmd lcd c (LCD_DISPLAYCONTROL newC)

-- | Update the display mode word
updateDisplayMode :: String -> (Expr Word8 -> Expr Word8) -> LCDE -> Arduino ()
updateDisplayMode what g lcd = do
  let c = lcdControllerE lcd
  let lcds = lcdStateE lcd
  debug what
  modifyRemoteRef (lcdDisplayModeE lcds) g
  newM <- readRemoteRef (lcdDisplayModeE lcds)
  sendCmd lcd c (LCD_ENTRYMODESET newM)

-- | Various control masks for the Hitachi44780
data Hitachi44780Mask = LCD_BLINKON              -- ^ bit @0@ Controls whether cursor blinks
                      | LCD_CURSORON             -- ^ bit @1@ Controls whether cursor is on
                      | LCD_DISPLAYON            -- ^ bit @2@ Controls whether display is on
                      | LCD_ENTRYSHIFTINCREMENT  -- ^ bit @0@ Controls left/right scroll
                      | LCD_ENTRYLEFT            -- ^ bit @1@ Controls left/right entry mode

-- | Convert the mask value to the bit no
maskBit :: Hitachi44780Mask -> Expr Word8
maskBit LCD_BLINKON             = 0
maskBit LCD_CURSORON            = 1
maskBit LCD_DISPLAYON           = 2
maskBit LCD_ENTRYSHIFTINCREMENT = 0
maskBit LCD_ENTRYLEFT           = 1

-- | Clear by the mask
clearMask :: Hitachi44780Mask -> Expr Word8 -> Expr Word8
clearMask m w = w `clearBit` (maskBit m)

-- | Set by the mask
setMask :: Hitachi44780Mask -> Expr Word8 -> Expr Word8
setMask m w = w `setBit` (maskBit m)

-- | Do not blink the cursor
lcdBlinkOffE :: LCDE -> Arduino ()
lcdBlinkOffE = updateDisplayControl "Turning blinking off" (clearMask LCD_BLINKON)

-- | Blink the cursor
lcdBlinkOnE :: LCDE -> Arduino ()
lcdBlinkOnE = updateDisplayControl "Turning blinking on" (setMask LCD_BLINKON)

-- | Hide the cursor. Note that a blinking cursor cannot be hidden, you must first
-- turn off blinking.
lcdCursorOffE :: LCDE -> Arduino ()
lcdCursorOffE = updateDisplayControl "Not showing the cursor" (clearMask LCD_CURSORON)

-- | Show the cursor
lcdCursorOnE :: LCDE -> Arduino ()
lcdCursorOnE = updateDisplayControl "Showing the cursor" (setMask LCD_CURSORON)

-- | Turn the display off. Note that turning the display off does not mean you are
-- powering it down. It simply means that the characters will not be shown until
-- you turn it back on using 'lcdDisplayOn'. (Also, the contents will /not/ be
-- forgotten when you call this function.) Therefore, this function is useful
-- for temporarily hiding the display contents.
lcdDisplayOffE :: LCDE -> Arduino ()
lcdDisplayOffE = updateDisplayControl "Turning display off" (clearMask LCD_DISPLAYON)

-- | Turn the display on
lcdDisplayOnE :: LCDE -> Arduino ()
lcdDisplayOnE = updateDisplayControl "Turning display on" (setMask LCD_DISPLAYON)

-- | Set writing direction: Left to Right
lcdLeftToRightE :: LCDE -> Arduino ()
lcdLeftToRightE = updateDisplayMode "Setting left-to-right entry mode" (setMask LCD_ENTRYLEFT)

-- | Set writing direction: Right to Left
lcdRightToLeftE :: LCDE -> Arduino ()
lcdRightToLeftE = updateDisplayMode "Setting right-to-left entry mode" (clearMask LCD_ENTRYLEFT)

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
lcdAutoScrollOnE :: LCDE -> Arduino ()
lcdAutoScrollOnE = updateDisplayMode "Setting auto-scroll ON" (setMask LCD_ENTRYSHIFTINCREMENT)

-- | Turn off auto-scrolling. See the comments for 'lcdAutoScrollOn' for details. When turned
-- off (which is the default), you will /not/ see the characters at the end of your strings that
-- do not fit into the display.
lcdAutoScrollOffE :: LCDE -> Arduino ()
lcdAutoScrollOffE = updateDisplayMode "Setting auto-scroll OFF" (clearMask LCD_ENTRYSHIFTINCREMENT)

-- | Flash contents of the LCD screen
lcdFlashE :: LCDE
         -> Int  -- ^ Flash count
         -> Expr Word32  -- ^ Delay amount (in milli-seconds)
         -> Arduino ()
lcdFlashE lcd n d = sequence_ $ concat $ replicate n [lcdDisplayOffE lcd, delayMillisE d, lcdDisplayOnE lcd, delayMillisE d]

{-

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
lcdCreateSymbolE :: LCDE -> [[Word8]] -> Arduino LCDSymbol
lcdCreateSymbolE lcd glyph
  | length glyph /= 8 || any (/= 5) (map length glyph)
  = do die "Haskino: lcdCreateSymbol: Invalid glyph description: must be 8x5!" ("Received:" : glyph)
       return $ LCDSymbol 255
  | True
  = do let c = lcdController lcd 
       let lcds = lcdState lcd
       i <- liftIO $ modifyMVar lcds $ \lcdst -> 
              do let lcdst' = lcdst { lcdGlyphCount = (lcdGlyphCount lcdst) + 1 }
                 return (lcdst', lcdGlyphCount lcdst)
       let create = do
            sendCmd lcd c (LCD_SETCGRAMADDR i)
            let cvt :: String -> Word8
                cvt s = foldr (.|.) 0 [bit p | (ch, p) <- zip (reverse s) [0..], not (isSpace ch)]
            mapM_ (sendData lcd c . cvt) glyph
       create
       return $ LCDSymbol i

-- | Display a user created symbol on the LCD. (See 'lcdCreateSymbol' for details.)
lcdWriteSymbolE :: LCD -> LCDSymbol -> Arduino ()
lcdWriteSymbolE lcd (LCDSymbol i) = withLCD lcd ("Writing custom symbol " ++ show i ++ " to LCD") $ \c -> sendData lcd c i

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
lcdInternalSymbolE :: Word8 -> LCDSymbol
lcdInternalSymbolE = LCDSymbol

-}

