-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprWord16
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr Word16
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Test.ExprWord16 where

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

litEval16 :: Expr Word16 -> Word16
litEval16 (LitW16 w) = w

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_neg :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRef r $ negate (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_sign :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRef r $ signum (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_add :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) + (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_sub :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) - (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_mult :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) * (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_div :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> NonZero Word16 -> Property
prop_div c r x (NonZero y) = monadicIO $ do
    let local = x `P.div` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `div` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_rem :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> NonZero Word16 -> Property
prop_rem c r x (NonZero y) = monadicIO $ do
    let local = x `P.rem` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `rem` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_quot :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> NonZero Word16 -> Property
prop_quot c r x (NonZero y) = monadicIO $ do
    let local = x `P.quot` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `quot` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_mod :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> NonZero Word16 -> Property
prop_mod c r x (NonZero y) = monadicIO $ do
    let local = x `P.mod` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `mod` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_comp :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRef r $ complement (lit x) 
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_and :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .&. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_or :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_or c r x y = monadicIO $ do
    let local = x DB..|. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .|. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_xor :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_xor c r x y = monadicIO $ do
    let local = x `DB.xor` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `xor` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_shiftL :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_shiftL c r x y = monadicIO $ do
    let local = x `DB.shiftL` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftL` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_shiftR :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_shiftR c r x y = monadicIO $ do
    let local = x `DB.shiftR` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftR` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_setBit :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_setBit c r x y = monadicIO $ do
    let local = x `DB.setBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `setBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_clearBit :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_clearBit c r x y = monadicIO $ do
    let local = x `DB.clearBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `clearBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_testBit :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word8 -> Property
prop_testBit c r x y = monadicIO $ do
    let local = x `DB.testBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `testBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_from8 :: ArduinoConnection -> RemoteRef Word16 -> Word8 -> Property
prop_from8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_from32 :: ArduinoConnection -> RemoteRef Word16 -> Word32 -> Property
prop_from32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_fromI8 :: ArduinoConnection -> RemoteRef Word16 -> Int8 -> Property
prop_fromI8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_fromI16 :: ArduinoConnection -> RemoteRef Word16 -> Int16 -> Property
prop_fromI16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_fromI32 :: ArduinoConnection -> RemoteRef Word16 -> Int32 -> Property
prop_fromI32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_ifb :: ArduinoConnection -> RemoteRef Word16 -> Bool -> Word16 -> Word16 -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) ==* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) /=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Word16 -> Word16 -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_arith :: ArduinoConnection -> RemoteRef Word16 -> 
              Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> NonZero Word16 -> Property
prop_arith c r a b d e f (NonZero g) = monadicIO $ do
    let local = a * b + d * e - f `P.div` g
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) `div` (lit g) 
        v <- readRemoteRef r
        return v
    assert (local == litEval16 remote)

prop_bind :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Property
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
    assert (local == litEval16 remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refW16 <- send conn $ newRemoteRef 0
    refB  <- send conn $ newRemoteRef (lit False)
    print "Negation Tests:"
    quickCheck (prop_neg conn refW16)
    print "Signum Tests:"
    quickCheck (prop_sign conn refW16)
    print "Addition Tests:"
    quickCheck (prop_add conn refW16)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn refW16)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn refW16)
    print "Division Tests:"
    quickCheck (prop_div conn refW16)
    print "Remainder Tests:"
    quickCheck (prop_rem conn refW16)
    print "Quotient Tests:"
    quickCheck (prop_quot conn refW16)
    print "Modulo Tests:"
    quickCheck (prop_mod conn refW16)
    print "Complement Tests:"
    quickCheck (prop_comp conn refW16)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn refW16)
    print "Bitwise Or Tests:"
    quickCheck (prop_or conn refW16)
    print "Bitwise Xor Tests:"
    quickCheck (prop_xor conn refW16)
    print "Shift Left Tests:"
    quickCheck (prop_shiftL conn refW16)
    print "Shift Right Tests:"
    quickCheck (prop_shiftR conn refW16)
    print "Set Bit Tests:"
    quickCheck (prop_setBit conn refW16)
    print "Clear Bit Tests:"
    quickCheck (prop_clearBit conn refW16)
    print "Test Bit Tests:"
    quickCheck (prop_testBit conn refB)
    print "From Word8 Tests:"
    quickCheck (prop_from8 conn refW16)
    print "From Word32 Tests:"
    quickCheck (prop_from32 conn refW16)
    print "From Int8 Tests:"
    quickCheck (prop_fromI8 conn refW16)
    print "From Int16 Tests:"
    quickCheck (prop_fromI16 conn refW16)
    print "From Int32 Tests:"
    quickCheck (prop_fromI32 conn refW16)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refW16)
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
    quickCheck (prop_arith conn refW16)
    print "Bind Tests:"
    quickCheck (prop_bind conn refW16)
    closeArduino conn
