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

import qualified Data.Bits as DB
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
  LitB         :: Bool -> Expr Bool
  LitW8        :: Word8 -> Expr Word8
  LitW16       :: Word16 -> Expr Word16
  LitW32       :: Word32 -> Expr Word32
  LitList8     :: [Word8] -> Expr [Word8]
  RefB         :: Int -> Expr Bool
  RefW8        :: Int -> Expr Word8
  RefW16       :: Int -> Expr Word16
  RefW32       :: Int -> Expr Word32
  RefList8     :: Int -> Expr [Word8]
  RemBindB     :: Int -> Expr Bool
  RemBindW8    :: Int -> Expr Word8
  RemBindW16   :: Int -> Expr Word16
  RemBindW32   :: Int -> Expr Word32
  RemBindList8 :: Int -> Expr [Word8]
  FromIntW8    :: Expr Word32 -> Expr Word8
  FromIntW16   :: Expr Word32 -> Expr Word16
  ToIntW8      :: Expr Word8  -> Expr Word32
  ToIntW16     :: Expr Word16 -> Expr Word32
  NotB         :: Expr Bool -> Expr Bool
  AndB        :: Expr Bool -> Expr Bool -> Expr Bool
  OrB         :: Expr Bool -> Expr Bool -> Expr Bool
  EqB         :: Expr Bool -> Expr Bool -> Expr Bool
  LessB       :: Expr Bool -> Expr Bool -> Expr Bool
  IfB         :: Expr Bool  -> Expr Bool -> Expr Bool -> Expr Bool
  NegW8       :: Expr Word8 -> Expr Word8
  SignW8      :: Expr Word8 -> Expr Word8
  AddW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  SubW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  MultW8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  DivW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  RemW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  AndW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  OrW8        :: Expr Word8 -> Expr Word8 -> Expr Word8
  XorW8       :: Expr Word8 -> Expr Word8 -> Expr Word8
  CompW8      :: Expr Word8 -> Expr Word8
  ShfLW8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  ShfRW8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  EqW8        :: Expr Word8 -> Expr Word8 -> Expr Bool
  LessW8      :: Expr Word8 -> Expr Word8 -> Expr Bool
  IfW8        :: Expr Bool  -> Expr Word8 -> Expr Word8 -> Expr Word8
  TestBW8     :: Expr Word8 -> Expr Word8 -> Expr Bool
  SetBW8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  ClrBW8      :: Expr Word8 -> Expr Word8 -> Expr Word8
  NegW16      :: Expr Word16 -> Expr Word16
  SignW16     :: Expr Word16 -> Expr Word16
  AddW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  SubW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  MultW16     :: Expr Word16 -> Expr Word16 -> Expr Word16
  DivW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  RemW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  AndW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  OrW16       :: Expr Word16 -> Expr Word16 -> Expr Word16
  XorW16      :: Expr Word16 -> Expr Word16 -> Expr Word16
  CompW16     :: Expr Word16 -> Expr Word16
  ShfLW16     :: Expr Word16 -> Expr Word8 -> Expr Word16
  ShfRW16     :: Expr Word16 -> Expr Word8 -> Expr Word16
  EqW16       :: Expr Word16 -> Expr Word16 -> Expr Bool
  LessW16     :: Expr Word16 -> Expr Word16 -> Expr Bool
  IfW16       :: Expr Bool   -> Expr Word16 -> Expr Word16 -> Expr Word16
  TestBW16    :: Expr Word16  -> Expr Word8 -> Expr Bool
  SetBW16     :: Expr Word16 -> Expr Word8 -> Expr Word16
  ClrBW16     :: Expr Word16 -> Expr Word8 -> Expr Word16
  NegW32      :: Expr Word32 -> Expr Word32
  SignW32     :: Expr Word32 -> Expr Word32
  AddW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  SubW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  MultW32     :: Expr Word32 -> Expr Word32 -> Expr Word32
  DivW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  RemW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  AndW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  OrW32       :: Expr Word32 -> Expr Word32 -> Expr Word32
  XorW32      :: Expr Word32 -> Expr Word32 -> Expr Word32
  CompW32     :: Expr Word32 -> Expr Word32
  ShfLW32     :: Expr Word32 -> Expr Word8 -> Expr Word32
  ShfRW32     :: Expr Word32 -> Expr Word8 -> Expr Word32
  EqW32       :: Expr Word32 -> Expr Word32 -> Expr Bool
  LessW32     :: Expr Word32 -> Expr Word32 -> Expr Bool
  IfW32       :: Expr Bool   -> Expr Word32 -> Expr Word32 -> Expr Word32
  TestBW32    :: Expr Word32 -> Expr Word8 -> Expr Bool
  SetBW32     :: Expr Word32 -> Expr Word8 -> Expr Word32
  ClrBW32     :: Expr Word32 -> Expr Word8 -> Expr Word32
  ElemList8   :: Expr [Word8] -> Expr Word8   -> Expr Word8
  LenList8    :: Expr [Word8] -> Expr Word8
  ConsList8   :: Expr Word8   -> Expr [Word8] -> Expr [Word8]
  ApndList8   :: Expr [Word8] -> Expr [Word8] -> Expr [Word8]
  PackList8   :: [Expr Word8] -> Expr [Word8]
  EqL8        :: Expr [Word8] -> Expr [Word8] -> Expr Bool
  LessL8      :: Expr [Word8] -> Expr [Word8] -> Expr Bool
  IfL8        :: Expr Bool  -> Expr [Word8] -> Expr [Word8] -> Expr [Word8]

deriving instance Show a => Show (Expr a)

class ExprB a where
    lit     :: a -> Expr a
    remBind :: Int -> Expr a

instance ExprB Word8 where
    lit = LitW8
    remBind = RemBindW8

instance ExprB Word16 where
    lit = LitW16
    remBind = RemBindW16

instance ExprB Word32 where
    lit = LitW32
    remBind = RemBindW32

instance ExprB Bool where
    lit = LitB
    remBind = RemBindB

instance ExprB [Word8] where
    lit = LitList8
    remBind = RemBindList8

instance B.Boolean (Expr Bool) where
  true  = LitB True
  false = LitB False
  notB  = NotB
  (&&*) = AndB
  (||*) = OrB

type instance BooleanOf (Expr Bool) = Expr Bool

instance B.EqB (Expr Bool) where
  (==*) = EqB

instance B.OrdB (Expr Bool) where
  (<*) = LessB

instance B.IfB (Expr Bool) where
  ifB = IfB

instance Num (Expr Word8) where
  (+) x y = AddW8 x y
  (-) x y = SubW8 x y
  (*) x y = MultW8 x y
  negate x = NegW8 x
  abs x  = x
  signum x = SignW8 x
  fromInteger x = LitW8 $ fromInteger x

type instance BooleanOf (Expr Word8) = Expr Bool

instance B.EqB (Expr Word8) where
  (==*) = EqW8

instance B.OrdB (Expr Word8) where
  (<*) = LessW8

instance B.IfB (Expr Word8) where
  ifB = IfW8

instance BN.NumB (Expr Word8) where
  type IntegerOf (Expr Word8) = Expr Word32
  fromIntegerB e = FromIntW8 e

instance BN.IntegralB (Expr Word8) where
  div = DivW8
  rem = RemW8
  quot = DivW8
  mod = RemW8
  toIntegerB e = ToIntW8 e

instance BB.BitsB (Expr Word8) where
  type IntOf (Expr Word8) = Expr Word8
  (.&.) = AndW8
  (.|.) = OrW8
  xor = XorW8
  complement = CompW8
  shiftL = ShfLW8
  shiftR = ShfRW8
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 8)
  bit = \i -> 1 `shiftL` i
  setBit = SetBW8
  clearBit = ClrBW8
  testBit = TestBW8 

instance  Num (Expr Word16) where
  (+) x y = AddW16 x y
  (-) x y = SubW16 x y
  (*) x y = MultW16 x y
  negate x = NegW16 x
  abs x  = x
  signum x = SignW16 x
  fromInteger x = LitW16 $ fromInteger x

type instance BooleanOf (Expr Word16) = Expr Bool

instance B.EqB (Expr Word16) where
  (==*) = EqW16

instance B.OrdB (Expr Word16) where
  (<*) = LessW16

instance B.IfB (Expr Word16) where
  ifB = IfW16

instance BN.NumB (Expr Word16) where
  type IntegerOf (Expr Word16) = (Expr Word32)
  fromIntegerB e = FromIntW16 e

instance BN.IntegralB (Expr Word16) where
  div = DivW16
  rem = RemW16
  quot = DivW16
  mod = RemW16
  toIntegerB e = ToIntW16 e

instance BB.BitsB (Expr Word16) where
  type IntOf (Expr Word16) = Expr Word8
  (.&.) = AndW16
  (.|.) = OrW16
  xor = XorW16
  complement = CompW16
  shiftL = ShfLW16
  shiftR = ShfRW16
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 16)
  bit = \i -> 1 `shiftL` i
  setBit = SetBW16
  clearBit = ClrBW16
  testBit = TestBW16

instance  Num (Expr Word32) where
  (+) x y = AddW32 x y
  (-) x y = SubW32 x y
  (*) x y = MultW32 x y
  negate x = NegW32 x
  abs x  = x
  signum x = SignW32 x
  fromInteger x = LitW32 $ fromInteger x

type instance BooleanOf (Expr Word32) = Expr Bool

instance B.EqB (Expr Word32) where
  (==*) = EqW32

instance B.OrdB (Expr Word32) where
  (<*) = LessW32

instance B.IfB (Expr Word32) where
  ifB = IfW32

instance BN.NumB (Expr Word32) where
  type IntegerOf (Expr Word32) = (Expr Word32)
  fromIntegerB e = e

instance BN.IntegralB (Expr Word32) where
  div = DivW32
  rem = RemW32
  quot = DivW32
  mod = RemW32
  toIntegerB e = e

instance BB.BitsB (Expr Word32) where
  type IntOf (Expr Word32) = Expr Word8
  (.&.) = AndW32
  (.|.) = OrW32
  xor = XorW32
  complement = CompW32
  shiftL = ShfLW32
  shiftR = ShfRW32
  isSigned = (\_ -> lit False)
  bitSize = (\_ -> lit 32)
  bit = \i -> 1 `shiftL` i
  setBit = SetBW32
  clearBit = ClrBW32
  testBit = TestBW32 

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
              | EXPR_INT8
              | EXPR_INT16
              | EXPR_INT32

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
            | EXPR_TSTB
            | EXPR_SETB
            | EXPR_CLRB
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
exprTypeVal EXPR_BOOL   = 0x00
exprTypeVal EXPR_WORD8  = 0x01
exprTypeVal EXPR_WORD16 = 0x02
exprTypeVal EXPR_WORD32 = 0x03
exprTypeVal EXPR_LIST8  = 0x04
exprTypeVal EXPR_INT8   = 0x05
exprTypeVal EXPR_INT16  = 0x06
exprTypeVal EXPR_INT32  = 0x07

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
exprOpVal EXPR_TSTB = 0x13
exprOpVal EXPR_SETB = 0x14
exprOpVal EXPR_CLRB = 0x15
exprOpVal EXPR_BIND = 0x16
exprOpVal EXPR_ELEM = 0x17
exprOpVal EXPR_LEN  = 0x18
exprOpVal EXPR_CONS = 0x19
exprOpVal EXPR_APND = 0x1A
exprOpVal EXPR_PACK = 0x1B
exprOpVal EXPR_FINT = 0x1C
exprOpVal EXPR_TINT = 0x1D

exprCmdVal :: ExprType -> ExprOp -> Word8
exprCmdVal t o = exprTypeVal t `DB.shiftL` 5 DB..|. exprOpVal o
