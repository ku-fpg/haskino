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

import       Data.Bits ((.|.), shiftL)
import       Data.Word (Word8, Word16, Word32)
import       Data.Boolean as B
import       Data.Boolean.Numbers as BN

data RemoteRef a where
    RemoteRefB   :: Int -> RemoteRef Bool
    RemoteRefW8  :: Int -> RemoteRef Word8
    RemoteRefW16 :: Int -> RemoteRef Word16
    RemoteRefW32 :: Int -> RemoteRef Word32

remoteRefToExpr :: RemoteRef a -> Expr a
remoteRefToExpr r = case r of
                         RemoteRefB i -> RefB i
                         RemoteRefW8 i -> Ref8 i
                         RemoteRefW16 i -> Ref16 i
                         RemoteRefW32 i -> Ref32 i

deriving instance Show a => Show (RemoteRef a)

type BoolE   = Expr Bool
type Word8E  = Expr Word8
type Word16E = Expr Word16
type Word32E = Expr Word32
type StringE = Expr String

data Expr a where
  LitB      :: Bool -> BoolE
  Lit8      :: Word8 -> Word8E
  Lit16     :: Word16 -> Word16E
  Lit32     :: Word32 -> Word32E
  RefB      :: Int -> BoolE
  Ref8      :: Int -> Word8E
  Ref16     :: Int -> Word16E
  Ref32     :: Int -> Word32E
  NotB      :: BoolE -> BoolE
  AndB      :: BoolE -> BoolE -> BoolE
  OrB       :: BoolE -> BoolE -> BoolE
  Neg8      :: Word8E -> Word8E
  Sign8     :: Word8E -> Word8E
  Add8      :: Word8E -> Word8E -> Word8E
  Sub8      :: Word8E -> Word8E -> Word8E
  Mult8     :: Word8E -> Word8E -> Word8E
  Div8      :: Word8E -> Word8E -> Word8E
  Rem8      :: Word8E -> Word8E -> Word8E
  And8      :: Word8E -> Word8E -> Word8E
  Or8       :: Word8E -> Word8E -> Word8E
  Xor8      :: Word8E -> Word8E -> Word8E
  Comp8     :: Word8E -> Word8E
  ShfL8     :: Word8E -> Word8E -> Word8E
  ShfR8     :: Word8E -> Word8E -> Word8E
  Eq8       :: Word8E -> Word8E -> BoolE
  Less8     :: Word8E -> Word8E -> BoolE
  If8       :: BoolE -> Word8E -> Word8E -> Word8E
  Neg16     :: Word16E -> Word16E
  Sign16    :: Word16E -> Word16E
  Add16     :: Word16E -> Word16E -> Word16E
  Sub16     :: Word16E -> Word16E -> Word16E
  Mult16    :: Word16E -> Word16E -> Word16E
  Div16     :: Word16E -> Word16E -> Word16E
  Rem16     :: Word16E -> Word16E -> Word16E
  And16     :: Word16E -> Word16E -> Word16E
  Or16      :: Word16E -> Word16E -> Word16E
  Xor16     :: Word16E -> Word16E -> Word16E
  Comp16    :: Word16E -> Word16E
  ShfL16    :: Word16E -> Word8E -> Word16E
  ShfR16    :: Word16E -> Word8E -> Word16E
  Eq16      :: Word16E -> Word16E -> BoolE
  Less16    :: Word16E -> Word16E -> BoolE
  If16      :: BoolE -> Word16E -> Word16E -> Word16E
  Neg32     :: Word32E -> Word32E
  Sign32    :: Word32E -> Word32E
  Add32     :: Word32E -> Word32E -> Word32E
  Sub32     :: Word32E -> Word32E -> Word32E
  Mult32    :: Word32E -> Word32E -> Word32E
  Div32     :: Word32E -> Word32E -> Word32E
  Rem32     :: Word32E -> Word32E -> Word32E
  And32     :: Word32E -> Word32E -> Word32E
  Or32      :: Word32E -> Word32E -> Word32E
  Xor32     :: Word32E -> Word32E -> Word32E
  Comp32    :: Word32E -> Word32E
  ShfL32    :: Word32E -> Word8E -> Word32E
  ShfR32    :: Word32E -> Word8E -> Word32E
  Eq32      :: Word32E -> Word32E -> BoolE
  Less32    :: Word32E -> Word32E -> BoolE
  If32      :: BoolE -> Word32E -> Word32E -> Word32E

deriving instance Show a => Show (Expr a)

class LiteralB a where
    lit  :: a -> Expr a

instance LiteralB Word8 where
    lit = Lit8

instance LiteralB Word16 where
    lit = Lit16

instance LiteralB Word32 where
    lit = Lit32

instance LiteralB Bool where
    lit = LitB

-- ToDo:  Add BitsB class for and, or, xor, complement and shifts
-- ToDo:  Add fromInteger/toInteger properly to do typing on Arduino

instance B.Boolean BoolE where
  true  = LitB True
  false = LitB False
  notB  = NotB
  (&&*) = AndB
  (||*) = OrB

instance Num Word8E where
  (+) x y = Add8 x y
  (-) x y = Sub8 x y
  (*) x y = Mult8 x y
  negate x = Neg8 x
  abs x  = x
  signum x = Sign8 x
  fromInteger x = Lit8 $ fromInteger x

type instance BooleanOf Word8E = BoolE

instance B.EqB Word8E where
  (==*) = Eq8

instance B.OrdB Word8E where
  (<*) = Less8

instance B.IfB Word8E where
  ifB = If8

instance BN.NumB Word8E where
  type IntegerOf Word8E = Word8
  fromIntegerB x = Lit8 x

instance BN.IntegralB Word8E where
  div = Div8
  rem = Rem8
  quot = Div8
  mod = Rem8
  toIntegerB x = case x of
                      Lit8 n -> n

instance  Num Word16E where
  (+) x y = Add16 x y
  (-) x y = Sub16 x y
  (*) x y = Mult16 x y
  negate x = Neg16 x
  abs x  = x
  signum x = Sign16 x
  fromInteger x = Lit16 $ fromInteger x

type instance BooleanOf Word16E = BoolE

instance B.EqB Word16E where
  (==*) = Eq16

instance B.OrdB Word16E where
  (<*) = Less16

instance B.IfB Word16E where
  ifB = If16

instance BN.NumB Word16E where
  type IntegerOf Word16E = Word16
  fromIntegerB x = Lit16 x

instance BN.IntegralB Word16E where
  div = Div16
  rem = Rem16
  quot = Div16
  mod = Rem16
  toIntegerB x = case x of
                    Lit16 n -> n

instance  Num Word32E where
  (+) x y = Add32 x y
  (-) x y = Sub32 x y
  (*) x y = Mult32 x y
  negate x = Neg32 x
  abs x  = x
  signum x = Sign32 x
  fromInteger x = Lit32 $ fromInteger x

type instance BooleanOf Word32E = BoolE

instance B.EqB Word32E where
  (==*) = Eq32

instance B.OrdB Word32E where
  (<*) = Less32

instance B.IfB Word32E where
  ifB = If32

instance BN.NumB Word32E where
  type IntegerOf Word32E = Word32
  fromIntegerB x = Lit32 x

instance BN.IntegralB Word32E where
  div = Div32
  rem = Rem32
  quot = Div32
  mod = Rem32
  toIntegerB x = case x of
                    Lit32 n -> n

-- | Haskino Firmware expresions, see:tbd 
data ExprType = EXPR_BOOL
              | EXPR_WORD8
              | EXPR_WORD16
              | EXPR_WORD32

data ExprOp = EXPR_LIT
            | EXPR_REF
            | EXPR_PROC
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

-- | Compute the numeric value of a command
exprTypeVal :: ExprType -> Word8
exprTypeVal EXPR_BOOL   = 0x01
exprTypeVal EXPR_WORD8  = 0x02
exprTypeVal EXPR_WORD16 = 0x03
exprTypeVal EXPR_WORD32 = 0x04

exprOpVal :: ExprOp -> Word8
exprOpVal EXPR_LIT  = 0x00
exprOpVal EXPR_REF  = 0x01
exprOpVal EXPR_PROC = 0x02
exprOpVal EXPR_NOT  = 0x03
exprOpVal EXPR_AND  = 0x04
exprOpVal EXPR_OR   = 0x05
exprOpVal EXPR_XOR  = 0x06
exprOpVal EXPR_NEG  = 0x07
exprOpVal EXPR_SIGN = 0x08
exprOpVal EXPR_ADD  = 0x09
exprOpVal EXPR_SUB  = 0x0A
exprOpVal EXPR_MULT = 0x0B
exprOpVal EXPR_DIV  = 0x0C
exprOpVal EXPR_REM  = 0x0D
exprOpVal EXPR_COMP = 0x0E
exprOpVal EXPR_SHFL = 0x0F
exprOpVal EXPR_SHFR = 0x10
exprOpVal EXPR_EQ   = 0x11
exprOpVal EXPR_LESS = 0x12
exprOpVal EXPR_IF   = 0x13

exprCmdVal :: ExprType -> ExprOp -> Word8
exprCmdVal t o = exprTypeVal t `shiftL` 5 .|. exprOpVal o
