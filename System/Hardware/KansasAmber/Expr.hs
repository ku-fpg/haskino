-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Expr
--                Based on System.Hardware.Arduino.Expr
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Underlying data structures
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, GADTs, StandaloneDeriving, TypeFamilies #-}

module System.Hardware.KansasAmber.Expr where

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
  VarB      :: String -> StringE
  Var8      :: String -> StringE
  Var16     :: String -> StringE
  Var32     :: String -> StringE
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
  Eq32      :: Word32E -> Word32E -> BoolE
  Less32    :: Word32E -> Word32E -> BoolE
  If32      :: BoolE -> Word32E -> Word32E -> Word32E

deriving instance Show a => Show (Expr a)

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

