{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.Bits
-- Copyright   :  (c) The University of Kansas 2015
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ToDo:
-- Stability   :  experimental
-- Portability :  portable
--
-- A generalized version of the class hirarchy for bitwise operations. All
-- functions that would break a potential deep embedding are removed
-- or generalized to support deep embeddings.
--
-----------------------------------------------------------------------------

module Data.Boolean.Bits (
  BitsB(
    (.&.), (.|.), xor,
    complement,
    shift,
    rotate,
    zeroBits,
    bit,
    setBit,
    clearBit,
    complementBit,
    testBit,
    bitSize,
    isSigned,
    shiftL, shiftR,
    unsafeShiftL, unsafeShiftR,
    rotateL, rotateR,
    popCount
  ),
) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

import Data.Boolean
import Data.Boolean.Numbers

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

-- | The 'Bits' class defines bitwise operations over integral types.
--
-- * Bits are numbered from 0 with bit 0 being the least
--   significant bit.
class EqB a => BitsB a where
    {-# MINIMAL (.&.), (.|.), xor, complement,
                (shift | (shiftL, shiftR)),
                (rotate | (rotateL, rotateR)),
                bitSize, isSigned, testBit, bit, popCount #-}
    -- | The accociated int type of the number.
    type IntOf a

    -- | Bitwise and
    (.&.) :: a -> a -> a

    -- | Bitwise or
    (.|.) :: a -> a -> a

    -- | Bitwise xor
    xor :: a -> a -> a

    {-| Reverse all the bits in the argument -}
    complement        :: a -> a

    {-| @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive,
        or right by @-i@ bits otherwise.
        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        An instance can define either this unified 'shift' or 'shiftL' and
        'shiftR', depending on which is more convenient for the type in
        question. -}
    shift             :: a -> IntOf a -> a

    {-| @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive,
        or right by @-i@ bits otherwise.

        For unbounded types like 'Integer', 'rotate' is equivalent to 'shift'.

        An instance can define either this unified 'rotate' or 'rotateL' and
        'rotateR', depending on which is more convenient for the type in
        question. -}
    rotate            :: a -> IntOf a -> a

    {-
    -- Rotation can be implemented in terms of two shifts, but care is
    -- needed for negative values.  This suggested implementation assumes
    -- 2's-complement arithmetic.  It is commented out because it would
    -- require an extra context (Ord a) on the signature of 'rotate'.
    x `rotate`  i | i<0 && isSigned x && x<0
                         = let left = i+bitSize x in
                           ((x `shift` i) .&. complement ((-1) `shift` left))
                           .|. (x `shift` left)
                  | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
                  | i==0 = x
                  | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))
    -}

    -- | 'zeroBits' is the value with all bits unset.
    --
    -- The following laws ought to hold (for all valid bit indices @/n/@):
    --
    --   * @'clearBit' 'zeroBits' /n/ == 'zeroBits'@
    --   * @'setBit'   'zeroBits' /n/ == 'bit' /n/@
    --   * @'testBit'  'zeroBits' /n/ == False@
    --   * @'popCount' 'zeroBits'   == 0@
    --
    -- This method uses @'clearBit' ('bit' 0) 0@ as its default
    -- implementation (which ought to be equivalent to 'zeroBits' for
    -- types which possess a 0th bit).
    --
    -- @since 4.7.0.0
    zeroBits :: a

    -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
    --
    -- Can be implemented using `bitDefault' if @a@ is also an
    -- instance of 'Num'.
    --
    -- See also 'zeroBits'.
    bit               :: IntOf a -> a

    -- | @x \`setBit\` i@ is the same as @x .|. bit i@
    setBit            :: a -> IntOf a -> a

    -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
    clearBit          :: a -> IntOf a -> a

    -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
    complementBit     :: a -> IntOf a -> a

    -- | Return 'True' if the @n@th bit of the argument is 1
    --
    -- Can be implemented using `testBitDefault' if @a@ is also an
    -- instance of 'Num'.
    testBit           :: a -> IntOf a -> Bool

    {-| Return the number of bits in the type of the argument.  The actual
        value of the argument is ignored.  The function 'bitSize' is
        undefined for types that do not have a fixed bitsize, like 'Integer'.
        -}
    bitSize           :: a -> IntOf a

    {-| Return 'True' if the argument is a signed type.  The actual
        value of the argument is ignored -}
    isSigned          :: a -> Bool

    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i

    {-| Shift the argument left by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'shiftR' or the unified
        'shift', depending on which is more convenient for the type in
        question. -}
    shiftL            :: a -> IntOf a -> a

    {-| Shift the argument left by the specified number of bits.  The
        result is undefined for negative shift amounts and shift amounts
        greater or equal to the 'bitSize'.

        Defaults to 'shiftL' unless defined explicitly by an instance.

        @since 4.5.0.0 -}
    unsafeShiftL            :: a -> IntOf a -> a

    {-| Shift the first argument right by the specified number of bits. The
        result is undefined for negative shift amounts and shift amounts
        greater or equal to the 'bitSize'.

        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        An instance can define either this and 'shiftL' or the unified
        'shift', depending on which is more convenient for the type in
        question. -}
    shiftR            :: a -> IntOf a -> a

    {-| Shift the first argument right by the specified number of bits, which
        must be non-negative an smaller than the number of bits in the type.

        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        Defaults to 'shiftR' unless defined explicitly by an instance.

        @since 4.5.0.0 -}
    unsafeShiftR            :: a -> IntOf a -> a

    {-| Rotate the argument left by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'rotateR' or the unified
        'rotate', depending on which is more convenient for the type in
        question. -}
    rotateL           :: a -> IntOf a -> a

    {-| Rotate the argument right by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'rotateL' or the unified
        'rotate', depending on which is more convenient for the type in
        question. -}
    rotateR           :: a -> IntOf a -> a

    {-| Return the number of set bits in the argument.  This number is
        known as the population count or the Hamming weight.

        Can be implemented using `popCountDefault' if @a@ is also an
        instance of 'Num'.

        @since 4.5.0.0 -}
    popCount          :: a -> IntOf a
