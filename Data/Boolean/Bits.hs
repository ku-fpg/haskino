{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

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
    IntOf,
    (.&.), (.|.), xor,
    complement,
    bit,
    setBit,
    clearBit,
    complementBit,
    testBit,
    bitSize,
    isSigned,
    shiftL, shiftR,
    rotateL, rotateR
  ),
) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

import Data.Boolean
import Data.Boolean.Numbers

infixl 8 `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

-- | The 'BitsB' class defines bitwise operations over integral types.
--
-- * Bits are numbered from 0 with bit 0 being the least
--   significant bit.
class EqB a => BitsB a where
    {-# MINIMAL (.&.), (.|.), xor, complement,
                shiftL, shiftR,
                bitSize, isSigned, bit #-}
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
    testBit           :: a -> IntOf a -> BooleanOf a

    testBit x i = x .&. bit i ==* bit i

    {-| Return the number of bits in the type of the argument.  The actual
        value of the argument is ignored.  The function 'bitSize' is
        undefined for types that do not have a fixed bitsize, like 'Integer'.
        -}
    bitSize           :: a -> IntOf a

    {-| Return 'True' if the argument is a signed type.  The actual
        value of the argument is ignored -}
    isSigned          :: a -> BooleanOf a

    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i

    {-| Shift the argument left by the specified number of bits
        (which must be non-negative).
    -}
    shiftL            :: a -> IntOf a -> a

    {-| Shift the first argument right by the specified number of bits. The
        result is undefined for negative shift amounts and shift amounts
        greater or equal to the 'bitSize'.

        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.
    -}
    shiftR            :: a -> IntOf a -> a

    {-| Rotate the argument left by the specified number of bits
        (which must be non-negative).
    -}
    rotateL           :: NumB (IntOf a) => a -> IntOf a -> a

    x `rotateL` i = (x `shiftL` i) .|. (x `shiftR` ((bitSize x)-i))

    {-| Rotate the argument right by the specified number of bits
        (which must be non-negative).
    -}
    rotateR           :: NumB (IntOf a) => a -> IntOf a -> a

    x `rotateR` i = (x `shiftR` i) .|. (x `shiftL` ((bitSize x)-i))
