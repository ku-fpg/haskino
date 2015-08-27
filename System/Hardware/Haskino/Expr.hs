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

import       Data.Word (Word8, Word16, Word32)
import       Data.Boolean as B
import       Data.Boolean.Numbers as BN

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
  VarB      :: String -> BoolE
  Var8      :: String -> Word8E
  Var16     :: String -> Word16E
  Var32     :: String -> Word32E
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
  ShfE8     :: Word8E -> Word8E -> Word8E
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

-- | Amber Firmware expresions, see:tbd 
data ExprCmd = EXPR_LITB 
                 | EXPR_LIT8
                 | EXPR_LIT16
                 | EXPR_LIT32
                 | EXPR_VARB
                 | EXPR_VAR8
                 | EXPR_VAR16
                 | EXPR_VAR32
                 | EXPR_NOTB
                 | EXPR_ANDB
                 | EXPR_ORB
                 | EXPR_NEG8
                 | EXPR_SIGN8
                 | EXPR_ADD8
                 | EXPR_SUB8
                 | EXPR_MULT8
                 | EXPR_DIV8
                 | EXPR_REM8
                 | EXPR_AND8
                 | EXPR_OR8
                 | EXPR_COMP8
                 | EXPR_SHFL8
                 | EXPR_SHFR8
                 | EXPR_XOR8
                 | EXPR_EQ8
                 | EXPR_LESS8
                 | EXPR_IF8
                 | EXPR_NEG16
                 | EXPR_SIGN16
                 | EXPR_ADD16
                 | EXPR_SUB16
                 | EXPR_MULT16
                 | EXPR_DIV16
                 | EXPR_REM16
                 | EXPR_AND16
                 | EXPR_OR16
                 | EXPR_XOR16
                 | EXPR_COMP16
                 | EXPR_SHFL16
                 | EXPR_SHFR16
                 | EXPR_EQ16
                 | EXPR_LESS16
                 | EXPR_IF16
                 | EXPR_NEG32
                 | EXPR_SIGN32
                 | EXPR_ADD32
                 | EXPR_SUB32
                 | EXPR_MULT32
                 | EXPR_DIV32
                 | EXPR_REM32
                 | EXPR_AND32
                 | EXPR_OR32
                 | EXPR_XOR32
                 | EXPR_COMP32
                 | EXPR_SHFL32
                 | EXPR_SHFR32
                 | EXPR_EQ32
                 | EXPR_LESS32
                 | EXPR_IF32
                deriving Show

-- | Compute the numeric value of a command
exprCmdVal :: ExprCmd -> Word8
exprCmdVal EXPR_LITB    = 0xC0
exprCmdVal EXPR_VARB    = 0xC1
exprCmdVal EXPR_NOTB    = 0xC2
exprCmdVal EXPR_ANDB    = 0xC3
exprCmdVal EXPR_ORB     = 0xC4
exprCmdVal EXPR_EQ8     = 0xC5
exprCmdVal EXPR_LESS8   = 0xC6
exprCmdVal EXPR_EQ16    = 0xC7
exprCmdVal EXPR_LESS16  = 0xC8
exprCmdVal EXPR_EQ32    = 0xC9
exprCmdVal EXPR_LESS32  = 0xCA
exprCmdVal EXPR_LIT8    = 0xD0
exprCmdVal EXPR_VAR8    = 0xD1
exprCmdVal EXPR_NEG8    = 0xD2
exprCmdVal EXPR_SIGN8   = 0xD3
exprCmdVal EXPR_ADD8    = 0xD4
exprCmdVal EXPR_SUB8    = 0xD5
exprCmdVal EXPR_MULT8   = 0xD6
exprCmdVal EXPR_DIV8    = 0xD7
exprCmdVal EXPR_REM8    = 0xD8
exprCmdVal EXPR_AND8    = 0xD9
exprCmdVal EXPR_OR8     = 0xDA
exprCmdVal EXPR_XOR8    = 0xDB
exprCmdVal EXPR_COMP8   = 0xDC
exprCmdVal EXPR_SHFL8   = 0xDD
exprCmdVal EXPR_SHFR8   = 0xDE
exprCmdVal EXPR_IF8     = 0xDF
exprCmdVal EXPR_LIT16   = 0xE0
exprCmdVal EXPR_VAR16   = 0xE1
exprCmdVal EXPR_NEG16   = 0xE2
exprCmdVal EXPR_SIGN16  = 0xE3
exprCmdVal EXPR_ADD16   = 0xE4
exprCmdVal EXPR_SUB16   = 0xE5
exprCmdVal EXPR_MULT16  = 0xE6
exprCmdVal EXPR_DIV16   = 0xE7
exprCmdVal EXPR_REM16   = 0xE8
exprCmdVal EXPR_AND16   = 0xE9
exprCmdVal EXPR_OR16    = 0xEA
exprCmdVal EXPR_XOR16   = 0xEB
exprCmdVal EXPR_COMP16  = 0xEC
exprCmdVal EXPR_SHFL16  = 0xED
exprCmdVal EXPR_SHFR16  = 0xEE
exprCmdVal EXPR_IF16    = 0xEF
exprCmdVal EXPR_LIT32   = 0xF0
exprCmdVal EXPR_VAR32   = 0xF1
exprCmdVal EXPR_NEG32   = 0xF2
exprCmdVal EXPR_SIGN32  = 0xF3
exprCmdVal EXPR_ADD32   = 0xF4
exprCmdVal EXPR_SUB32   = 0xF5
exprCmdVal EXPR_MULT32  = 0xF6
exprCmdVal EXPR_DIV32   = 0xF7
exprCmdVal EXPR_REM32   = 0xF8
exprCmdVal EXPR_AND32   = 0xF9
exprCmdVal EXPR_OR32    = 0xFA
exprCmdVal EXPR_XOR32   = 0xFB
exprCmdVal EXPR_COMP32  = 0xFC
exprCmdVal EXPR_SHFL32  = 0xFD
exprCmdVal EXPR_SHFR32  = 0xFE
exprCmdVal EXPR_IF32    = 0xFF
