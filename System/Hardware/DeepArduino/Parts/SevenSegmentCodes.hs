-------------------------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Parts.SevenSegmentCodes
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Character to 7-segment display conversion.
-------------------------------------------------------------------------------------------------

module System.Hardware.Arduino.Parts.SevenSegmentCodes(char2SS) where

import Data.Word (Word8)

-- | Convert a character to a bit-pattern, suitable for display on a seven-segment display. 
-- Note that most characters are just not representable in a 7-segment display, in which
-- case we map it to 'Nothing'. However, some substitutions are done, for instance '(' is
-- displayed the same as '['.
--
-- The return value is a 'Word8', although only 7-bits are used; the least significant bit will
-- always be 0. With the traditional coding, the bits correspond to segments ABCDEFG0, i.e.,
-- most-significant-bit will be for segment A, next for segment B, and so on.
char2SS :: Char -> Maybe Word8
char2SS = (`lookup` tbl)
 where tbl = [ ('"',  0x44), ('\'', 0x40), ('(',  0x9C), (')',  0xF0), ('-',  0x02), ('0',  0xFC), ('1',  0x60), ('2',  0xDA), ('3',  0xF2), ('4',  0x66), ('5',  0xB6)
             , ('6',  0xBE), ('7',  0xE0), ('8',  0xFE), ('9',  0xF6), ('=',  0x12), ('?',  0xCA), ('A',  0xEE), ('B',  0x3E), ('C',  0x9C), ('D',  0x7A), ('E',  0x9E)
             , ('F',  0x8E), ('G',  0xBC), ('H',  0x6E), ('I',  0x60), ('J',  0x78), ('L',  0x1C), ('N',  0x2A), ('O',  0xFC), ('P',  0xCE), ('R',  0x0A), ('S',  0xB6)
             , ('T',  0x1E), ('U',  0x7C), ('Y',  0x76), ('[',  0x9C), (']',  0xF0), ('_',  0x10), ('a',  0xFA), ('b',  0x3E), ('c',  0x1A), ('d',  0x7A), ('e',  0xDE)
             , ('f',  0x8E), ('g',  0xBC), ('h',  0x2E), ('i',  0x20), ('j',  0x78), ('l',  0x1C), ('n',  0x2A), ('o',  0x3A), ('p',  0xCE), ('q',  0xE7), ('r',  0x0A)
             , ('s',  0xB6), ('t',  0x1E), ('u',  0x38), ('y',  0x76), (' ',  0x00)
             ]
