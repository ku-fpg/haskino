-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal utilities
-------------------------------------------------------------------------------
module System.Hardware.DeepArduino.Utils where

import Data.Bits          ((.|.), shiftL, (.&.), shiftR)
import Data.Char          (isAlphaNum, isAscii, isSpace, chr)
import Data.IORef         (newIORef, readIORef, writeIORef)
import Data.List          (intercalate)
import Data.Word          (Word8, Word16, Word32)
import Data.Time          (getCurrentTime, utctDayTime)
import Numeric            (showHex, showIntAtBase)

import qualified Data.ByteString            as B 
import Data.ByteString.Base16 (encode)

-- | A simple printer that can keep track of sequence numbers. Used for debugging purposes.
mkDebugPrinter :: Bool -> IO (String -> IO ())
mkDebugPrinter False = return (const (return ()))
mkDebugPrinter True  = do
        cnt <- newIORef (1::Int)
        let f s = do i <- readIORef cnt
                     writeIORef cnt (i+1)
                     tick <- utctDayTime `fmap` getCurrentTime
                     let precision = 1000000 :: Integer
                         micro = round . (fromIntegral precision *) . toRational $ tick
                     putStrLn $ "[" ++ show i ++ ":" ++ show (micro :: Integer) ++ "] DeepArduino: " ++ s
        return f

-- | Show a byte in a visible format.
showByte :: Word8 -> String
showByte i | isVisible = [c]
           | i <= 0xf  = '0' : showHex i ""
           | True      = showHex i ""
  where c = chr $ fromIntegral i
        isVisible = isAscii c && isAlphaNum c && isSpace c

-- | Show a list of bytes
showByteList :: [Word8] -> String
showByteList bs =  "[" ++ intercalate ", " (map showByte bs) ++ "]"

-- | Show a number as a binary value
showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 (head . show) n ""

-- | Turn a lo/hi encoded Arduino string constant into a Haskell string
getString :: [Word8] -> String
getString = map (chr . fromIntegral) . fromArduinoBytes

-- | Turn a lo/hi encoded Arduino sequence into a bunch of words, again weird
-- encoding.
fromArduinoBytes :: [Word8] -> [Word8]
fromArduinoBytes []         = []
fromArduinoBytes [x]        = [x]  -- shouldn't really happen
fromArduinoBytes (l:h:rest) = c : fromArduinoBytes rest
  where c = h `shiftL` 7 .|. l -- first seven bit comes from l; then extra stuff is in h

-- | Turn a normal byte into a lo/hi Arduino byte. If you think this encoding
-- is just plain weird, you're not alone. (I suspect it has something to do
-- with error-correcting low-level serial communication of the past.)
toArduinoBytes :: Word8 -> [Word8]
toArduinoBytes w = [lo, hi]
  where lo =  w             .&. 0x7F   -- first seven bits
        hi = (w `shiftR` 7) .&. 0x7F   -- one extra high-bit

-- | Convert a word to it's bytes, as would be required by Arduino comms
word32ToBytes :: Word32 -> [Word8]
word32ToBytes i = map fromIntegral [(i `shiftR` 24) .&. 0xFF, (i `shiftR` 16) .&. 0xFF, (i `shiftR`  8) .&. 0xFF, i .&. 0xFF]

-- | Inverse conversion for word32ToBytes
bytesToWord32 :: (Word8, Word8, Word8, Word8) -> Word32
bytesToWord32 (a, b, c, d) = fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d

-- | Convert a word to it's bytes, as would be required by Arduino comms
word16ToBytes :: Word16 -> [Word8]
word16ToBytes i = map fromIntegral [(i `shiftR`  8) .&. 0xFF, i .&. 0xFF]

-- | Inverse conversion for word32ToBytes
bytesToWord16 :: (Word8, Word8) -> Word16
bytesToWord16 (a, b) = fromIntegral a `shiftL` 8 .|. fromIntegral b

-- | Convert a word to it's bytes, as would be required by Arduino comms
word16ToArduinoBytes :: Word16 -> [Word8]
word16ToArduinoBytes i = map fromIntegral [i .&. 0x7F, (i `shiftR`  7) .&. 0x7F]

-- | Convert words to it's bytes, as would be required by Arduino comms
words16ToArduinoBytes :: [Word16] -> [Word8]
words16ToArduinoBytes ws = concat $ map word16ToArduinoBytes ws

-- | Convert a word to it's bytes, as would be required by Arduino comms
word32ToArduinoBytes :: Word32 -> [Word8]
word32ToArduinoBytes i = map fromIntegral [i .&. 0x7F, (i `shiftR` 7) .&. 0x7F, (i `shiftR` 14) .&. 0x7F, (i `shiftR` 21) .&. 0x7F, (i `shiftR`  28) .&. 0x0F]

-- | Convert a sequence of 7 bit bytes into an array of 16 bit data
arduinoBytesToWords16 :: [Word8] -> [Word16]
arduinoBytesToWords16 []         = []
arduinoBytesToWords16 [x]        = arduinoBytesToWords16 [x,0]  -- shouldn't really happen
arduinoBytesToWords16 (l:h:rest) = c : arduinoBytesToWords16 rest
  where c = fromIntegral h `shiftL` 7 .|. fromIntegral l -- first seven bit comes from l; then extra stuff is in h

-- | Convert a sequence of 8 bit bytes to a sequence of 7 bit bytes
arduinoEncoded :: B.ByteString -> B.ByteString 
arduinoEncoded bs = arduinoEncoded' bs 0 0
  where
    arduinoEncoded' :: B.ByteString -> Int -> Word8 -> B.ByteString 
    arduinoEncoded' bs shift prev = 
      case B.uncons bs of
        Nothing                 -> if shift == 0 then B.empty else B.singleton prev
        Just (h,t) | shift == 6 -> B.cons (((h `shiftL` shift) .&. 0x7f) .|. prev) 
                                    (B.cons (h `shiftR` 1) (arduinoEncoded' t 0 0))
        Just (h,t)              -> B.cons (((h `shiftL` shift) .&. 0x7f) .|. prev) 
                                    (arduinoEncoded' t ((shift + 1) `mod` 7) (h `shiftR` (7 - shift)))

-- | Convert a sequence of 7 bit bytes to a sequence of 8 bit bytes
arduinoDecoded :: [Word8] -> [Word8]
arduinoDecoded bs = arduinoDecoded' bs 0
  where
    arduinoDecoded' :: [Word8] -> Int -> [Word8]
    arduinoDecoded' []          _     = []
    arduinoDecoded' [x]         shift = [x `shiftR` shift]
    arduinoDecoded' (x:x1:rest) shift = ((x `shiftR` shift) .|. ((x1 `shiftL` (7 - shift)) .&. 0xFF)) : 
                                        (arduinoDecoded' (x1:rest) ((shift + 1) `mod` 7)) 
