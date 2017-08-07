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

module System.Hardware.Haskino.SamplePrograms.Rewrite.LCD(
  -- * LCD types and registration
  LCD, LCDController(..), lcdRegister
  -- * Writing text on the LCD
  , lcdClear, lcdWrite, lcdWriteChar
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
  , lcdFlash, lcdBacklightOn, lcdBacklightOff
  )  where

-- | LCD's connected to the board
data LCD = LCD {
                 lcdController   :: LCDController  -- ^ Actual controller
               , lcdState        :: LCDData        -- ^ State information
               }

-- | Hitachi LCD controller: See: <http://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller>.
-- We model only the 4-bit variant, with RS and EN lines only. (The most common Arduino usage.)
-- The data sheet can be seen at: <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>.
data LCDController =
    Hitachi44780 {
                       lcdRS       :: Pin  -- ^ Hitachi pin @ 4@: Register-select
                     , lcdEN       :: Pin  -- ^ Hitachi pin @ 6@: Enable
                     , lcdD4       :: Pin  -- ^ Hitachi pin @11@: Data line @4@
                     , lcdD5       :: Pin  -- ^ Hitachi pin @12@: Data line @5@
                     , lcdD6       :: Pin  -- ^ Hitachi pin @13@: Data line @6@
                     , lcdD7       :: Pin  -- ^ Hitachi pin @14@: Data line @7@
                     , lcdBL       :: Maybe Pin -- ^ Backlight control pin (if present)
                     , lcdRows     :: Word8  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Word8  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
    | I2CHitachi44780 {
                       address     :: Word8 -- ^ I2C Slave Address of LCD
                     , lcdRows     :: Word8  -- ^ Number of rows (typically 1 or 2, upto 4)
                     , lcdCols     :: Word8  -- ^ Number of cols (typically 16 or 20, upto 40)
                     , dotMode5x10 :: Bool -- ^ Set to True if 5x10 dots are used
                     }
                     deriving Show

-- | State of the LCD, a mere 8-bit word for the Hitachi
data LCDData = LCDData {
                  lcdDisplayMode    :: RemoteRef Word8         -- ^ Display mode (left/right/scrolling etc.)
                , lcdDisplayControl :: RemoteRef Word8         -- ^ Display control (blink on/off, display on/off etc.)
                , lcdGlyphCount     :: RemoteRef Word8         -- ^ Count of custom created glyphs (typically at most 8)
                , lcdBacklightState :: RemoteRef Bool
                }

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
         | LCD_DISPLAYCONTROL Word8
         | LCD_CLEARDISPLAY
         | LCD_ENTRYMODESET   Word8
         | LCD_RETURNHOME
         | LCD_SETDDRAMADDR   Word8
         | LCD_CURSORSHIFT    Word8
         | LCD_SETCGRAMADDR   Word8
         | LCD_NOOP

-- | Convert a command to a data-word
getCmdVal :: LCDController -> Cmd -> Word8
getCmdVal c cmd = get cmd
  where multiLine -- bit 3
          | (lcdRows c) > 1 = 0x08 :: Word8
          | True        = 0x00 :: Word8
        dotMode   -- bit 2
          | (dotMode5x10 c) = 0x04 :: Word8
          | True        = 0x00 :: Word8
        displayFunction = multiLine .|. dotMode
        get :: Cmd -> Expr Word8
        get LCD_NOOP               = 0x00
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
initLCD :: LCD -> Arduino ()
initLCD lcd = do
    let c = lcdController lcd
    case c of 
        Hitachi44780{} -> initLCDDigital c
        I2CHitachi44780{} -> i2cConfig
    -- Wait for 50ms, data-sheet says at least 40ms for 2.7V version, so be safe
    delayMillis 50
    sendCmd lcd LCD_INITIALIZE
    delayMillis 5
    sendCmd lcd LCD_INITIALIZE_END
    sendCmd lcd LCD_FUNCTIONSET
    lcdCursorOff lcd
    lcdBlinkOff lcd
    lcdLeftToRight lcd
    lcdAutoScrollOff lcd
    lcdHome lcd
    lcdClear lcd
    lcdDisplayOn lcd

-- | Initialize the LCD. Follows the data sheet <http://lcd-linux.sourceforge.net/pdfdocs/hd44780.pdf>,
-- page 46; figure 24.
initLCDDigital :: LCDController -> Arduino ()
initLCDDigital c@Hitachi44780{lcdRS, lcdEN, lcdD4, lcdBL} = do
    if isJust lcdBL then let Just p = lcdBL in setPinMode p OUTPUT else return ()
    setPinMode lcdRS OUTPUT
    setPinMode lcdEN OUTPUT
    setPinMode lcdD4 OUTPUT
    setPinMode lcdD5 OUTPUT
    setPinMode lcdD6 OUTPUT
    setPinMode lcdD7 OUTPUT

-- | Send a command to the LCD controller
sendCmd :: LCD -> LCDController -> Cmd -> Arduino ()
sendCmd lcd c = transmit False (lcdController lcd) . getCmdVal c

-- | Send 4-bit data to the LCD controller
sendData :: LCD -> Word8 -> Arduino ()
sendData lcd n = transmit True (lcdController lcd) n

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process - Done with Digtial writes
pulseEnableDig :: LCDController -> Arduino ()
pulseEnableDig Hitachi44780{lcdEN} = do
  digitalWrite lcdEN False
  delayMicros 1
  digitalWrite lcdEN True
  delayMicros 1
  digitalWrite lcdEN False
  delayMicros 2000

-- | Transmit data down to the LCD
transmit :: Bool -> LCD -> LCDController -> Word8 -> Arduino ()
transmit mode  val = do
  let cntrl = lcd c
  case cntrl c of
    Hitachi44780{}    -> transmitDig mode cntrl val
    I2CHitachi44780{} -> transmitI2C mode cntrl val

-- | Transmit data down to the LCD digital writes
transmitDig :: Bool -> LCDController -> Word8 -> Arduino ()
transmitDig mode c@Hitachi44780{lcdRS, lcdEN, lcdD4} val = do
  digitalWrite lcdRS mode
  digitalWrite lcdEN false
  -- Send down the first 4 bits
  digitalPortWrite lcdD4 (val `shiftR` 4) 0x0F
  pulseEnableDig c
  -- Send down the remaining batch
  digitalPortWriteE lcdD4 (val .&. 0x0F) 0x0F
  pulseEnableDig c

data LCD_I2C_Bits =
  LCD_I2C_BACKLIGHT | 
  LCD_I2C_ENABLE |
  LCD_I2C_RS

lcdI2CBitsToVal :: LCD_I2C_Bits -> Word8
lcdI2CBitsToVal LCD_I2C_BACKLIGHT = 8
lcdI2CBitsToVal LCD_I2C_ENABLE    = 4
lcdI2CBitsToVal LCD_I2C_RS        = 1

-- | Transmit data down to the I2CLCD using I2C writes
transmitI2C :: Bool -> LCD -> Word8 -> Arduino ()
transmitI2C mode lcd  val = do
    let c@I2CHitachi44780{address} = lcdController lcd
    let lcds = lcdState lcd
    bls <- readRemoteRef (lcdBacklightStateE lcds)
    let bl = ifB bls (lcdI2CBitsToVal LCD_I2C_BACKLIGHT) 0 
        lors = lo .|. rs .|. bl
        hirs = hi .|. rs .|. bl
    i2cWrite address $ pack [hirs]
    pulseEnableI2C c hirs
    i2cWrite address $ pack [lors]
    pulseEnableI2C c lors
  where rs = ifB mode (lcdI2CBitsToVal LCD_I2C_RS) 0
        lo =  (val `shiftL` 4) .&. 0xF0    -- lower four bits
        hi =  val .&. 0xF0                 -- upper four bits

-- | By controlling the enable-pin, indicate to the controller that
-- the data is ready for it to process - Done with I2C writes
pulseEnableI2C :: LCDController -> Word8 -> Arduino ()
pulseEnableI2C c@I2CHitachi44780{address} d = do
    i2cWrite address $ pack [d .|. en]
    delayMicros 1
    i2cWrite address $ pack [d .&. (complement en)]
    delayMillis 2
  where
    en = lcdI2CBitsToVal LCD_I2C_ENABLE

-- | Helper function to simplify library programming, not exposed to the user.
withLCD :: LCD -> String -> (LCDController -> Arduino a) -> Arduino a
withLCD lcd what action = do
        let c = lcdController lcd 
        debugE $ litString what
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
    mode <- newRemoteRef 0
    control <- newRemoteRef 0
    count <- newRemoteRef 0
    backlight <- newRemoteRef True
    let ld = LCDData { lcdDisplayMode    = mode
                     , lcdDisplayControl = control
                     , lcdGlyphCount     = count
                     , lcdBacklightState = backlight
                     }
    let c = LCD { lcdController = controller
                , lcdState = ld
                }
    initLCD c
    return c

-- | Turn backlight on if there is one, otherwise do nothing
lcdBacklightOnE :: LCD -> Arduino ()
lcdBacklightOnE lcd = lcdBacklight lcd True

-- | Turn backlight off if there is one, otherwise do nothing
lcdBacklightOffE :: LCD -> Arduino ()
lcdBacklightOffE lcd = lcdBacklight lcd False

-- | Turn backlight on/off if there is one, otherwise do nothing
lcdBacklight :: LCD -> Bool -> Arduino ()
lcdBacklight lcd on = do
   let lcdc = lcdController lcd
   case lcdc of 
      Hitachi44780{} -> do
        let bl = lcdBL lcdc
        if isJust bl 
            then let Just p = bl in digitalWrite p on
            else return()
      I2CHitachi44780{} -> do
        let lcds = lcdState lcd
        writeRemoteRef (lcdBacklightState lcds) on
        -- Send a noop so backlight state line gets updated
        sendCmd lcd lcdc LCD_NOOP

-- | Write a string on the LCD at the current cursor position
lcdWrite :: LCD -> [Word8] -> Arduino ()
lcdWrite lcd ws = withLCD lcd ("Writing " ++ show ws ++ " to LCD") $ \c -> writeWs c
   where writeWs c = forInE ws (\w -> sendData lcd c w)

-- | Write a string on the LCD at the current cursor position
lcdWriteChar :: LCD -> Word8 -> Arduino ()
lcdWriteChar lcd w = sendData lcd w

-- | Clear the LCD
lcdClear :: LCD -> Arduino ()
lcdClear lcd = do sendCmd lcd LCD_CLEARDISPLAY
                  delayMicros 200 -- give some time to make sure LCD is really cleared

-- | Send the cursor to home position
lcdHome :: LCD -> Arduino ()
lcdHome lcd = do sendCmd lcd LCD_RETURNHOME
                 delayMicros 200

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
lcdSetCursor :: LCD -> Word8 -> Word8 -> Arduino ()
lcdSetCursor lcd givenCol givenRow = withLCD lcd ("Sending the cursor to Row: " ++ show givenRow ++ " Col: " ++ show givenCol) set
  where set c = sendCmd lcd c (LCD_SETDDRAMADDR offset)
              where align :: Word8 -> Word8 -> Word8
                    align i m = ifB (i >= m) (m-1) i
                    col = align givenCol $ lcdCols c
                    row = align givenRow $ lcdRows c
                    -- The magic row-offsets come from various web sources
                    -- I don't follow the logic in these numbers, but it seems to work
                    offset = col + ifB (row ==* 0) 0
                                        (ifB (row ==* 1) 0x40 
                                              (ifB (row ==* 2) 0x14 0x54))

-- | Scroll the display to the left by 1 character. Project idea: Using a tilt sensor, scroll the contents of the display
-- left/right depending on the tilt. 
lcdScrollDisplayLeft :: LCD -> Arduino ()
lcdScrollDisplayLeft lcd = sendCmd lcd c (LCD_CURSORSHIFT lcdMoveLeft)
  where lcdMoveLeft = 0x00

-- | Scroll the display to the right by 1 character
lcdScrollDisplayRight :: LCDE -> Arduino ()
lcdScrollDisplayRight lcd = sendCmd lcd c (LCD_CURSORSHIFT lcdMoveRight)
  where lcdMoveRight = 0x04

-- | Update the display control word
updateDisplayControl :: (Word8 -> Word8) -> LCD -> Arduino ()
updateDisplayControl f lcd = do 
  let c = lcdController lcd
  let lcds = lcdState lcd
  modifyRemoteRef (lcdDisplayControl lcds) f
  newC <- readRemoteRef (lcdDisplayControlE lcds)
  sendCmd lcd (LCD_DISPLAYCONTROL newC)

-- | Update the display mode word
updateDisplayMode :: (Word8 -> Word8) -> LCD -> Arduino ()
updateDisplayMode g lcd = do
  let c = lcdControllerE lcd
  let lcds = lcdStateE lcd
  modifyRemoteRef (lcdDisplayMode lcds) g
  newM <- readRemoteRef (lcdDisplayMode lcds)
  sendCmd lcd (LCD_ENTRYMODESET newM)

-- | Various control masks for the Hitachi44780
data Hitachi44780Mask = LCD_BLINKON              -- ^ bit @0@ Controls whether cursor blinks
                      | LCD_CURSORON             -- ^ bit @1@ Controls whether cursor is on
                      | LCD_DISPLAYON            -- ^ bit @2@ Controls whether display is on
                      | LCD_ENTRYSHIFTINCREMENT  -- ^ bit @0@ Controls left/right scroll
                      | LCD_ENTRYLEFT            -- ^ bit @1@ Controls left/right entry mode

-- | Convert the mask value to the bit no
maskBit :: Hitachi44780Mask -> Word8
maskBit LCD_BLINKON             = 0
maskBit LCD_CURSORON            = 1
maskBit LCD_DISPLAYON           = 2
maskBit LCD_ENTRYSHIFTINCREMENT = 0
maskBit LCD_ENTRYLEFT           = 1

-- | Clear by the mask
clearMask :: Hitachi44780Mask -> Word8 -> Word8
clearMask m w = w `clearBit` (maskBit m)

-- | Set by the mask
setMask :: Hitachi44780Mask -> Word8 -> Word8
setMask m w = w `setBit` (maskBit m)

-- | Do not blink the cursor
lcdBlinkOff :: LCD -> Arduino ()
lcdBlinkOff = updateDisplayControl (clearMask LCD_BLINKON)

-- | Blink the cursor
lcdBlinkOn :: LCD -> Arduino ()
lcdBlinkOn = updateDisplayControl (setMask LCD_BLINKON)

-- | Hide the cursor. Note that a blinking cursor cannot be hidden, you must first
-- turn off blinking.
lcdCursorOff :: LCD -> Arduino ()
lcdCursorOff = updateDisplayControl (clearMask LCD_CURSORON)

-- | Show the cursor
lcdCursorOn :: LCD -> Arduino ()
lcdCursorOn = updateDisplayControl (setMask LCD_CURSORON)

-- | Turn the display off. Note that turning the display off does not mean you are
-- powering it down. It simply means that the characters will not be shown until
-- you turn it back on using 'lcdDisplayOn'. (Also, the contents will /not/ be
-- forgotten when you call this function.) Therefore, this function is useful
-- for temporarily hiding the display contents.
lcdDisplayOff :: LCD -> Arduino ()
lcdDisplayOff = updateDisplayControl (clearMask LCD_DISPLAYON)

-- | Turn the display on
lcdDisplayOn :: LCD -> Arduino ()
lcdDisplayOn = updateDisplayControl (setMask LCD_DISPLAYON)

-- | Set writing direction: Left to Right
lcdLeftToRight :: LCD -> Arduino ()
lcdLeftToRight = updateDisplayMode (setMask LCD_ENTRYLEFT)

-- | Set writing direction: Right to Left
lcdRightToLeft :: LCD -> Arduino ()
lcdRightToLeft = updateDisplayMode (clearMask LCD_ENTRYLEFT)

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
lcdAutoScrollOn = updateDisplayMode (setMask LCD_ENTRYSHIFTINCREMENT)

-- | Turn off auto-scrolling. See the comments for 'lcdAutoScrollOn' for details. When turned
-- off (which is the default), you will /not/ see the characters at the end of your strings that
-- do not fit into the display.
lcdAutoScrollOff :: LCD -> Arduino ()
lcdAutoScrollOff = updateDisplayMode (clearMask LCD_ENTRYSHIFTINCREMENT)

-- | Flash contents of the LCD screen
lcdFlashE :: LCD
         -> Int  -- ^ Flash count
         -> Word32  -- ^ Delay amount (in milli-seconds)
         -> Arduino ()
lcdFlashE lcd n d = sequence_ $ concat $ replicate n [lcdDisplayOffE lcd, delayMillisE d, lcdDisplayOnE lcd, delayMillisE d]

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
lcdCreateSymE :: LCD -> [Word8] -> Arduino LCDSymbolE
lcdCreateSymE lcd glyph =
    do let c = lcdControllerE lcd 
       let lcds = lcdStateE lcd
       i <- readRemoteRef (lcdGlyphCountE lcds)
       modifyRemoteRef (lcdGlyphCountE lcds) (\x -> x + 1)
       sendCmd lcd c (LCD_SETCGRAMADDR i)
       forInE glyph (\w -> sendData lcd c w)
       return $ LCDSymbolE i

lcdCreateSymbolE :: LCDE -> [String] -> Arduino LCDSymbolE
lcdCreateSymbolE lcd glyph
  | length glyph /= 8 || any (/= 5) (map length glyph)
  = do die "Haskino: lcdCreateSymbol: Invalid glyph description: must be 8x5!" ("Received:" : glyph)
       return $ LCDSymbolE 255
  | True
  = do let cvt :: String -> Expr Word8
           cvt s = lit $ foldr (B..|.) 0 [B.bit p | (ch, p) <- zip (reverse s) [0..], not (isSpace ch)]
       let rawGlyph = pack $ map cvt glyph
       lcdCreateSymE lcd rawGlyph

-- | Display a user created symbol on the LCD. (See 'lcdCreateSymbol' for details.)
lcdWriteSymbol :: LCD -> LCDSymbol -> Arduino ()
lcdWriteSymbol lcd (LCDSymbol i) = lcdWriteChar lcd i

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
lcdInternalSymbolE :: Expr Word8 -> LCDSymbolE
lcdInternalSymbolE = LCDSymbolE
