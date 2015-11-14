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

module System.Hardware.Haskino.Test.ExprWord32 where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger )
import qualified Prelude as P
import System.Hardware.Haskino
import Data.Boolean
import Data.Boolean.Numbers
import Data.Boolean.Bits
import Data.Word
import qualified Data.Bits as DB
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEval32 :: Expr Word32 -> Word32
litEval32 (Lit32 w) = w

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_neg :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRef r $ negate (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_sign :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRef r $ signum (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_add :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) + (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_sub :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) - (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_mult :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) * (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_div :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_div c r x (NonZero y) = monadicIO $ do
    let local = x `P.div` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `div` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_rem :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_rem c r x (NonZero y) = monadicIO $ do
    let local = x `P.rem` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `rem` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_comp :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRef r $ complement (lit x) 
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_and :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .&. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_or :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_or c r x y = monadicIO $ do
    let local = x DB..|. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .|. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_xor :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_xor c r x y = monadicIO $ do
    let local = x `DB.xor` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `xor` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_shiftL :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word8 -> Property
prop_shiftL c r x y = monadicIO $ do
    let local = x `DB.shiftL` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftL` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_shiftR :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word8 -> Property
prop_shiftR c r x y = monadicIO $ do
    let local = x `DB.shiftR` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftR` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_setBit :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word8 -> Property
prop_setBit c r x y = monadicIO $ do
    let local = x `DB.setBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `setBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_clearBit :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word8 -> Property
prop_clearBit c r x y = monadicIO $ do
    let local = x `DB.clearBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `clearBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_testBit :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word8 -> Property
prop_testBit c r x y = monadicIO $ do
    let local = x `DB.testBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `testBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_to8 :: ArduinoConnection -> RemoteRef Word32 -> Word8 -> Property
prop_to8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_to16 :: ArduinoConnection -> RemoteRef Word32 -> Word16 -> Property
prop_to16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_ifb :: ArduinoConnection -> RemoteRef Word32 -> Bool -> Word32 -> Word32 -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) ==* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) /=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_arith :: ArduinoConnection -> RemoteRef Word32 -> 
              Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> NonZero Word32 -> Property
prop_arith c r a b d e f (NonZero g) = monadicIO $ do
    let local = a * b + d * e - f `P.div` g
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) `div` (lit g) 
        v <- readRemoteRef r
        return v
    assert (local == litEval32 remote)

prop_bind :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Property
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
    assert (local == litEval32 remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    ref32 <- send conn $ newRemoteRef 0
    refB  <- send conn $ newRemoteRef (lit False)
    print "Negation Tests:"
    quickCheck (prop_neg conn ref32)
    print "Signum Tests:"
    quickCheck (prop_sign conn ref32)
    print "Addition Tests:"
    quickCheck (prop_add conn ref32)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn ref32)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn ref32)
    print "Division Tests:"
    quickCheck (prop_div conn ref32)
    print "Remainder Tests:"
    quickCheck (prop_rem conn ref32)
    print "Complement Tests:"
    quickCheck (prop_comp conn ref32)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn ref32)
    print "Bitwise Or Tests:"
    quickCheck (prop_or conn ref32)
    print "Bitwise Xor Tests:"
    quickCheck (prop_xor conn ref32)
    print "Shift Left Tests:"
    quickCheck (prop_shiftL conn ref32)
    print "Shift Right Tests:"
    quickCheck (prop_shiftR conn ref32)
    print "Set Bit Tests:"
    quickCheck (prop_setBit conn ref32)
    print "Clear Bit Tests:"
    quickCheck (prop_clearBit conn ref32)
    print "Test Bit Tests:"
    quickCheck (prop_testBit conn refB)
    print "To Word8 Tests:"
    quickCheck (prop_to8 conn ref32)
    print "To Word16 Tests:"
    quickCheck (prop_to16 conn ref32)
    print "ifB Tests:"
    quickCheck (prop_ifb conn ref32)
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
    quickCheck (prop_arith conn ref32)
    print "Bind Tests:"
    quickCheck (prop_bind conn ref32)
    closeArduino conn
