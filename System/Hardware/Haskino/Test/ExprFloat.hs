-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprWord32
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr Word32
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Test.ExprFloat where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger )
import qualified Prelude as P
import System.Hardware.Haskino
import Data.Boolean
import Data.Boolean.Numbers
import Data.Boolean.Bits
import Data.Int
import Data.Word
import qualified Data.Bits as DB
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEvalFloat :: Expr Float -> Float
litEvalFloat (LitFloat f) = f

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_neg :: ArduinoConnection -> RemoteRef Float -> Float -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRef r $ negate (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_sign :: ArduinoConnection -> RemoteRef Float -> Float -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRef r $ signum (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_add :: ArduinoConnection -> RemoteRef Float -> Float -> Float -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) + (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_sub :: ArduinoConnection -> RemoteRef Float -> Float -> Float -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) - (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_mult :: ArduinoConnection -> RemoteRef Float -> Float -> Float -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) * (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_div :: ArduinoConnection -> RemoteRef Float -> Float -> NonZero Float -> Property
prop_div c r x (NonZero y) = monadicIO $ do
    let local = x / y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) / (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_from8 :: ArduinoConnection -> RemoteRef Float -> Word8 -> Property
prop_from8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_from16 :: ArduinoConnection -> RemoteRef Float -> Word16 -> Property
prop_from16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_from32 :: ArduinoConnection -> RemoteRef Float -> Word32 -> Property
prop_from32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_fromI8 :: ArduinoConnection -> RemoteRef Float -> Int8 -> Property
prop_fromI8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_fromI16 :: ArduinoConnection -> RemoteRef Float -> Int16 -> Property
prop_fromI16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_fromI32 :: ArduinoConnection -> RemoteRef Float -> Int32 -> Property
prop_fromI32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_ifb :: ArduinoConnection -> RemoteRef Float -> Bool -> Float -> Float -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) ==* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) /=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Float -> Float -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_arith :: ArduinoConnection -> RemoteRef Float -> 
              Float -> Float -> Float -> Float -> Float -> NonZero Float -> Property
prop_arith c r a b d e f (NonZero g) = monadicIO $ do
    let local = a * b + d * e - f / g
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) / (lit g) 
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

prop_bind :: ArduinoConnection -> RemoteRef Float -> Float -> Float -> Float -> Float -> Property
prop_bind c r a b d e = monadicIO $ do
    let local = a * b + d * e
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit a) 
        a' <- readRemoteRef r
        writeRemoteRef r $ a' * (lit b) 
        ab' <- readRemoteRef r
        writeRemoteRef r $ (lit d) 
        d' <- readRemoteRef r
        v <- readRemoteRef r
        writeRemoteRef r $ d' * (lit e) 
        de' <- readRemoteRef r
        writeRemoteRef r $ ab' + de' 
        v <- readRemoteRef r
        return v
    assert (local == litEvalFloat remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refF <- send conn $ newRemoteRef 0.0
    refB  <- send conn $ newRemoteRef (lit False)
    print "Negation Tests:"
    quickCheck (prop_neg conn refF)
    print "Signum Tests:"
    quickCheck (prop_sign conn refF)
    print "Addition Tests:"
    quickCheck (prop_add conn refF)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn refF)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn refF)
    print "Division Tests:"
    quickCheck (prop_div conn refF)
    print "From Word8 Tests:"
    quickCheck (prop_from8 conn refF)
    print "From Word16 Tests:"
    quickCheck (prop_from16 conn refF)
    print "From Word32 Tests:"
    quickCheck (prop_from32 conn refF)
    print "From Int8 Tests:"
    quickCheck (prop_fromI8 conn refF)
    print "From Int16 Tests:"
    quickCheck (prop_fromI16 conn refF)
    print "From Int32 Tests:"
    quickCheck (prop_fromI32 conn refF)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refF)
    print "Equal Tests:"
    quickCheck (prop_eq conn refB)
    print "Not Equal Tests:"
    quickCheck (prop_neq conn refB)
    print "Less Than Tests:"
    quickCheck (prop_lt conn refB)
    print "Greater Than Tests:"
    quickCheck (prop_gt conn refB)
    print "Less Than Equal Tests:"
    quickCheck (prop_lte conn refB)
    print "Greater Than Equal Tests:"
    quickCheck (prop_gte conn refB)
    print "Arithemtic Tests:"
    quickCheck (prop_arith conn refF)
    print "Bind Tests:"
    quickCheck (prop_bind conn refF)
    closeArduino conn
