-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Expr
--                Based on System.Hardware.Arduino.Expr
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Underlying data structures
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, GADTs, StandaloneDeriving, TypeFamilies #-}

module System.Hardware.Haskino.Expr where

import       Data.Bits as DB
import       Data.Word (Word8, Word16, Word32)
import       Data.Boolean as B
import       Data.Boolean.Numbers as BN
import       Data.Boolean.Bits as BB

data RemoteRef a where
    RemoteRefB   :: Int -> RemoteRef Bool
    RemoteRefW8  :: Int -> RemoteRef Word8
    RemoteRefW16 :: Int -> RemoteRef Word16
    RemoteRefW32 :: Int -> RemoteRef Word32
    RemoteRefL8  :: Int -> RemoteRef [Word8]

deriving instance Show a => Show (RemoteRef a)

data Expr a where
  LitB      :: Bool -> Expr Bool
  Lit8      :: Word8 -> Expr Word8
  Lit16     :: Word16 -> Expr Word16
  Lit32     :: Word32 -> Expr Word32
  LitList8  :: [Word8] -> Expr [Word8]
  RefB      :: Int -> Expr Bool
  Ref8      :: Int -> Expr Word8
  Ref16     :: Int -> Expr Word16
  Ref32     :: Int -> Expr Word32
  RefList8  :: Int -> Expr [Word8]
  RemBindB  :: Int -> Expr Bool
  RemBind8  :: Int -> Expr Word8
  RemBind16 :: Int -> Expr Word16
  RemBind32 :: Int -> Expr Word32
  RemBindList8 :: Int -> Expr [Word8]
  FromInt8  :: Expr Word32 -> Expr Word8
  FromInt16 :: Expr Word32 -> Expr Word16
  ToInt8    :: Expr Word8  -> Expr Word32
  ToInt16   :: Expr Word16 -> Expr Word32
  NotB      :: Expr Bool -> Expr Bool
  AndB      :: Expr Bool -> Expr Bool -> Expr Bool
  OrB       :: Expr Bool -> Expr Bool -> Expr Bool
  Neg8      :: Expr Word8 -> Expr Word8
  Sign8     :: Expr Word8 -> Expr Word8
  Add8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  Sub8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  Mult8     :: Expr Word8 -> Expr Word8 -> Expr Word8
  Div8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  Rem8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  And8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  Or8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  Xor8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  Comp8     :: Expr Word8 -> Expr Word8
  ShfL8     :: Expr Word8 -> Expr Word8 -> Expr Word8
  ShfR8     :: Expr Word8 -> Expr Word8 -> Expr Word8
  Eq8       :: Expr Word8 -> Expr Word8 -> Expr Bool
  Less8     :: Expr Word8 -> Expr Word8 -> Expr Bool
  If8       :: Expr Bool  -> Expr Word8 -> Expr Word8 -> Expr Word8
  Bit8      :: Expr Word8 -> Expr Word8
  SetB8     :: Expr Word8 -> Expr Word8 -> Expr Word8
  ClrB8     :: Expr Word8 -> Expr Word8 -> Expr Word8
  Neg16     :: Expr Word16 -> Expr Word16
  Sign16    :: Expr Word16 -> Expr Word16
  Add16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  Sub16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  Mult16    :: Expr Word16 -> Expr Word16 -> Expr Word16
  Div16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  Rem16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  And16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  Or16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  Xor16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  Comp16    :: Expr Word16 -> Expr Word16
  ShfL16    :: Expr Word16 -> Expr Word8 -> Expr Word16
  ShfR16    :: Expr Word16 -> Expr Word8 -> Expr Word16
  Eq16      :: Expr Word16 -> Expr Word16 -> Expr Bool
  Less16    :: Expr Word16 -> Expr Word16 -> Expr Bool
  If16      :: Expr Bool   -> Expr Word16 -> Expr Word16 -> Expr Word16
  Bit16     :: Expr Word8  -> Expr Word16
  SetB16    :: Expr Word16 -> Expr Word8 -> Expr Word16
  ClrB16    :: Expr Word16 -> Expr Word8 -> Expr Word16
  Neg32     :: Expr Word32 -> Expr Word32
  Sign32    :: Expr Word32 -> Expr Word32
  Add32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  Sub32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  Mult32    :: Expr Word32 -> Expr Word32 -> Expr Word32
  Div32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  Rem32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  And32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  Or32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  Xor32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  Comp32    :: Expr Word32 -> Expr Word32
  ShfL32    :: Expr Word32 -> Expr Word8 -> Expr Word32
  ShfR32    :: Expr Word32 -> Expr Word8 -> Expr Word32
  Eq32      :: Expr Word32 -> Expr Word32 -> Expr Bool
  Less32    :: Expr Word32 -> Expr Word32 -> Expr Bool
  If32      :: Expr Bool   -> Expr Word32 -> Expr Word32 -> Expr Word32
  Bit32     :: Expr Word8  -> Expr Word32
  SetB32    :: Expr Word32 -> Expr Word8 -> Expr Word32
  ClrB32    :: Expr Word32 -> Expr Word8 -> Expr Word32
  ElemList8 :: Expr [Word8] -> Expr Word8   -> Expr Word8
  LenList8  :: Expr [Word8] -> Expr Word8
  ConsList8 :: Expr Word8   -> Expr [Word8] -> Expr [Word8]
  ApndList8 :: Expr [Word8] -> Expr [Word8] -> Expr [Word8]
  PackList8 :: [Expr Word8] -> Expr [Word8]
  EqL8      :: Expr [Word8] -> Expr [Word8] -> Expr Bool
  LessL8    :: Expr [Word8] -> Expr [Word8] -> Expr Bool
  IfL8      :: Expr Bool  -> Expr [Word8] -> Expr [Word8] -> Expr [Word8]

deriving instance Show a => Show (Expr a)

class ExprB a where
    lit     :: a -> Expr a
    remBind :: Int -> Expr a

instance ExprB Word8 where
    lit = Lit8
    remBind = RemBind8

instance ExprB Word16 where
    lit = Lit16
    remBind = RemBind16

instance ExprB Word32 where
    lit = Lit32
    remBind = RemBind32

instance ExprB Bool where
    lit = LitB
    remBind = RemBindB

instance ExprB [Word8] where
    lit = LitList8
    remBind = RemBindList8

-- ToDo:  Add BitsB class for and, or, xor, complement and shifts
-- ToDo:  Add fromInteger/toInteger properly to do typing on Arduino

instance B.Boolean (Expr Bool) where
  true  = LitB True
  false = LitB False
  notB  = NotB
  (&&*) = AndB
  (||*) = OrB

instance Num (Expr Word8) where
  (+) x y = Add8 x y
  (-) x y = Sub8 x y
  (*) x y = Mult8 x y
  negate x = Neg8 x
  abs x  = x
  signum x = Sign8 x
  fromInteger x = Lit8 $ fromInteger x

type instance BooleanOf (Expr Word8) = Expr Bool

instance B.EqB (Expr Word8) where
  (==*) = Eq8

instance B.OrdB (Expr Word8) where
  (<*) = Less8

instance B.IfB (Expr Word8) where
  ifB = If8

instance BN.NumB (Expr Word8) where
  type IntegerOf (Expr Word8) = Expr Word32
  fromIntegerB e = FromInt8 e

instance BN.IntegralB (Expr Word8) where
  div = Div8
  rem = Rem8
  quot = Div8
  mod = Rem8
  toIntegerB e = ToInt8 e

instance BB.BitsB (Expr Word8) where
  type IntOf (Expr Word8) = Expr Word8
  (.&.) = And8
  (.|.) = Or8
  xor = Xor8
  complement = Comp8
  shiftL = ShfL8
  shiftR = ShfR8
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 8)
  bit = Bit8
  setBit = SetB8
  clearBit = ClrB8
--  testBit = (\x i -> x .&. bit i ==* bit i)

instance  Num (Expr Word16) where
  (+) x y = Add16 x y
  (-) x y = Sub16 x y
  (*) x y = Mult16 x y
  negate x = Neg16 x
  abs x  = x
  signum x = Sign16 x
  fromInteger x = Lit16 $ fromInteger x

type instance BooleanOf (Expr Word16) = Expr Bool

instance B.EqB (Expr Word16) where
  (==*) = Eq16

instance B.OrdB (Expr Word16) where
  (<*) = Less16

instance B.IfB (Expr Word16) where
  ifB = If16

instance BN.NumB (Expr Word16) where
  type IntegerOf (Expr Word16) = (Expr Word32)
  fromIntegerB e = FromInt16 e

instance BN.IntegralB (Expr Word16) where
  div = Div16
  rem = Rem16
  quot = Div16
  mod = Rem16
  toIntegerB e = ToInt16 e

instance BB.BitsB (Expr Word16) where
  type IntOf (Expr Word16) = Expr Word8
  (.&.) = And16
  (.|.) = Or16
  xor = Xor16
  complement = Comp16
  shiftL = ShfL16
  shiftR = ShfR16
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 16)
  bit = Bit16
  setBit = SetB16
  clearBit = ClrB16
--  testBit = (\x i -> x .&. bit i ==* bit i)

instance  Num (Expr Word32) where
  (+) x y = Add32 x y
  (-) x y = Sub32 x y
  (*) x y = Mult32 x y
  negate x = Neg32 x
  abs x  = x
  signum x = Sign32 x
  fromInteger x = Lit32 $ fromInteger x

type instance BooleanOf (Expr Word32) = Expr Bool

instance B.EqB (Expr Word32) where
  (==*) = Eq32

instance B.OrdB (Expr Word32) where
  (<*) = Less32

instance B.IfB (Expr Word32) where
  ifB = If32

instance BN.NumB (Expr Word32) where
  type IntegerOf (Expr Word32) = (Expr Word32)
  fromIntegerB e = e

instance BN.IntegralB (Expr Word32) where
  div = Div32
  rem = Rem32
  quot = Div32
  mod = Rem32
  toIntegerB e = e

instance BB.BitsB (Expr Word32) where
  type IntOf (Expr Word32) = Expr Word8
  (.&.) = And32
  (.|.) = Or32
  xor = Xor32
  complement = Comp32
  shiftL = ShfL32
  shiftR = ShfR32
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 32)
  bit = Bit32
  setBit = SetB32
  clearBit = ClrB32
--  testBit = (\x i -> x .&. bit i ==* bit i)

type instance BooleanOf (Expr [Word8]) = Expr Bool

instance B.EqB (Expr [Word8]) where
  (==*) = EqL8

instance B.OrdB (Expr [Word8]) where
  (<*) = LessL8

instance B.IfB (Expr [Word8]) where
  ifB = IfL8

infixl 9 !!*
infixl 5 *:, ++*

(!!*) :: Expr [Word8] -> Expr Word8 -> Expr Word8
(!!*) l i = ElemList8 l i

(*:) :: Expr Word8 -> Expr [Word8] -> Expr [Word8]
(*:) n l = ConsList8 n l

(++*) :: Expr [Word8] -> Expr [Word8] -> Expr [Word8]
(++*) l1 l2 = ApndList8 l1 l2

-- ToDo: overload length
len :: Expr [Word8] -> Expr Word8
len l = LenList8 l

pack :: [Expr Word8] -> Expr [Word8]
pack l = PackList8 l

-- | Haskino Firmware expresions, see:tbd 
data ExprType = EXPR_BOOL
              | EXPR_WORD8
              | EXPR_WORD16
              | EXPR_WORD32
              | EXPR_LIST8

data ExprOp = EXPR_LIT
            | EXPR_REF
            | EXPR_NOT
            | EXPR_AND
            | EXPR_OR
            | EXPR_XOR
            | EXPR_NEG
            | EXPR_SIGN
            | EXPR_ADD
            | EXPR_SUB
            | EXPR_MULT
            | EXPR_DIV
            | EXPR_REM
            | EXPR_COMP
            | EXPR_SHFL
            | EXPR_SHFR
            | EXPR_EQ
            | EXPR_LESS
            | EXPR_IF
            | EXPR_BIT
            | EXPR_SETB
            | EXPR_CLRB
            | EXPR_TSTB
            | EXPR_BIND
            | EXPR_ELEM
            | EXPR_LEN
            | EXPR_CONS
            | EXPR_APND
            | EXPR_PACK
            | EXPR_FINT
            | EXPR_TINT

-- | Compute the numeric value of a command
exprTypeVal :: ExprType -> Word8
exprTypeVal EXPR_BOOL   = 0x01
exprTypeVal EXPR_WORD8  = 0x02
exprTypeVal EXPR_WORD16 = 0x03
exprTypeVal EXPR_WORD32 = 0x04
exprTypeVal EXPR_LIST8  = 0x05

exprOpVal :: ExprOp -> Word8
exprOpVal EXPR_LIT  = 0x00
exprOpVal EXPR_REF  = 0x01
exprOpVal EXPR_NOT  = 0x02
exprOpVal EXPR_AND  = 0x03
exprOpVal EXPR_OR   = 0x04
exprOpVal EXPR_XOR  = 0x05
exprOpVal EXPR_NEG  = 0x06
exprOpVal EXPR_SIGN = 0x07
exprOpVal EXPR_ADD  = 0x08
exprOpVal EXPR_SUB  = 0x09
exprOpVal EXPR_MULT = 0x0A
exprOpVal EXPR_DIV  = 0x0B
exprOpVal EXPR_REM  = 0x0C
exprOpVal EXPR_COMP = 0x0D
exprOpVal EXPR_SHFL = 0x0E
exprOpVal EXPR_SHFR = 0x0F
exprOpVal EXPR_EQ   = 0x10
exprOpVal EXPR_LESS = 0x11
exprOpVal EXPR_IF   = 0x12
exprOpVal EXPR_BIT  = 0x13
exprOpVal EXPR_SETB = 0x14
exprOpVal EXPR_CLRB = 0x15
exprOpVal EXPR_TSTB = 0x16
exprOpVal EXPR_BIND = 0x17
exprOpVal EXPR_ELEM = 0x18
exprOpVal EXPR_LEN  = 0x19
exprOpVal EXPR_CONS = 0x1A
exprOpVal EXPR_APND = 0x1B
exprOpVal EXPR_PACK = 0x1C
exprOpVal EXPR_FINT = 0x1D
exprOpVal EXPR_TINT = 0x1E

exprCmdVal :: ExprType -> ExprOp -> Word8
exprCmdVal t o = exprTypeVal t `DB.shiftL` 5 DB..|. exprOpVal o
