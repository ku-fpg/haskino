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
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hardware.Haskino.Test.ExprWord32 where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger, (<*) )
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

litEval32 :: Expr Word32 -> Word32
litEval32 (LitW32 w) = w

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_neg :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRefE r $ negate (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_sign :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRefE r $ signum (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_add :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) + (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_sub :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) - (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_mult :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) * (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_div :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_div c r x (NonZero y) = monadicIO $ do
    let local = x `P.div` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `div` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_rem :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_rem c r x (NonZero y) = monadicIO $ do
    let local = x `P.rem` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `rem` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_quot :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_quot c r x (NonZero y) = monadicIO $ do
    let local = x `P.quot` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `quot` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_mod :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonZero Word32 -> Property
prop_mod c r x (NonZero y) = monadicIO $ do
    let local = x `P.mod` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `mod` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_comp :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRefE r $ complement (lit x) 
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_and :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) .&. (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_or :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_or c r x y = monadicIO $ do
    let local = x DB..|. y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) .|. (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_xor :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Property
prop_xor c r x y = monadicIO $ do
    let local = x `DB.xor` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `xor` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_shiftL :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonNegative Int -> Property
prop_shiftL c r x (NonNegative y) = monadicIO $ do
    let local = x `DB.shiftL` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `shiftL` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_shiftR :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonNegative Int -> Property
prop_shiftR c r x (NonNegative y) = monadicIO $ do
    let local = x `DB.shiftR` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `shiftR` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_setBit :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonNegative Int -> Property
prop_setBit c r x (NonNegative y) = monadicIO $ do
    let local = x `DB.setBit` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `setBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_clearBit :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> NonNegative Int -> Property
prop_clearBit c r x (NonNegative y) = monadicIO $ do
    let local = x `DB.clearBit` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `clearBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_testBit :: ArduinoConnection -> RemoteRef Bool -> Word32 -> NonNegative Int -> Property
prop_testBit c r x (NonNegative y) = monadicIO $ do
    let local = x `DB.testBit` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `testBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_from8 :: ArduinoConnection -> RemoteRef Word32 -> Word8 -> Property
prop_from8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_from16 :: ArduinoConnection -> RemoteRef Word32 -> Word16 -> Property
prop_from16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_fromI8 :: ArduinoConnection -> RemoteRef Word32 -> Int8 -> Property
prop_fromI8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_fromI16 :: ArduinoConnection -> RemoteRef Word32 -> Int16 -> Property
prop_fromI16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_fromI32 :: ArduinoConnection -> RemoteRef Word32 -> Int32 -> Property
prop_fromI32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_ifb :: ArduinoConnection -> RemoteRef Word32 -> Bool -> Word32 -> Word32 -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRefE r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) ==* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) /=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Word32 -> Word32 -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_arith :: ArduinoConnection -> RemoteRef Word32 -> 
              Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> NonZero Word32 -> Property
prop_arith c r a b d e f (NonZero g) = monadicIO $ do
    let local = a * b + d * e - f `P.div` g
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) `div` (lit g) 
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_bind :: ArduinoConnection -> RemoteRef Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Property
prop_bind c r a b d e = monadicIO $ do
    let local = a * b + d * e
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit a) 
        a' <- readRemoteRefE r
        writeRemoteRefE r $ a' * (lit b) 
        ab' <- readRemoteRefE r
        writeRemoteRefE r $ (lit d) 
        d' <- readRemoteRefE r
        v <- readRemoteRefE r
        writeRemoteRefE r $ d' * (lit e) 
        de' <- readRemoteRefE r
        writeRemoteRefE r $ ab' + de' 
        v <- readRemoteRefE r
        return v
    assert (local == litEval32 remote)

prop_while :: ArduinoConnection -> NonZero Word8 -> Property
prop_while c (NonZero x) = monadicIO $ do
    let x'::Word32 = fromIntegral x
    let local = x'
    remote <- run $ send c $ do
        v <- whileE (lit 0) (\z -> z <* lit x') (\z -> return $ z + 1)
        return v
    assert (local == litEval32 remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refW32 <- send conn $ newRemoteRefE 0
    refB  <- send conn $ newRemoteRefE (lit False)
    print "Negation Tests:"
    quickCheck (prop_neg conn refW32)
    print "Signum Tests:"
    quickCheck (prop_sign conn refW32)
    print "Addition Tests:"
    quickCheck (prop_add conn refW32)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn refW32)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn refW32)
    print "Division Tests:"
    quickCheck (prop_div conn refW32)
    print "Remainder Tests:"
    quickCheck (prop_rem conn refW32)
    print "Quotient Tests:"
    quickCheck (prop_quot conn refW32)
    print "Modulo Tests:"
    quickCheck (prop_mod conn refW32)
    print "Complement Tests:"
    quickCheck (prop_comp conn refW32)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn refW32)
    print "Bitwise Or Tests:"
    quickCheck (prop_or conn refW32)
    print "Bitwise Xor Tests:"
    quickCheck (prop_xor conn refW32)
    print "Shift Left Tests:"
    quickCheck (prop_shiftL conn refW32)
    print "Shift Right Tests:"
    quickCheck (prop_shiftR conn refW32)
    print "Set Bit Tests:"
    quickCheck (prop_setBit conn refW32)
    print "Clear Bit Tests:"
    quickCheck (prop_clearBit conn refW32)
    print "Test Bit Tests:"
    quickCheck (prop_testBit conn refB)
    print "From Word8 Tests:"
    quickCheck (prop_from8 conn refW32)
    print "From Word16 Tests:"
    quickCheck (prop_from16 conn refW32)
    print "From Int8 Tests:"
    quickCheck (prop_fromI8 conn refW32)
    print "From Int16 Tests:"
    quickCheck (prop_fromI16 conn refW32)
    print "From Int32 Tests:"
    quickCheck (prop_fromI32 conn refW32)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refW32)
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
    quickCheck (prop_arith conn refW32)
    print "Bind Tests:"
    quickCheck (prop_bind conn refW32)
    print "While Tests:"
    quickCheck (prop_while conn)
    closeArduino conn
