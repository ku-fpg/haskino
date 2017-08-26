-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprInt
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr Int
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}

module System.Hardware.Haskino.Test.ExprInt where

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

litEval :: Expr Int -> Int
litEval (LitI w) = w

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_neg :: ArduinoConnection -> RemoteRef Int -> Int -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRefE r $ negate (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_sign :: ArduinoConnection -> RemoteRef Int -> Int -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRefE r $ signum (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_add :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) + (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_sub :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) - (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_mult :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) * (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_div :: ArduinoConnection -> RemoteRef Int -> Int -> NonZero Int -> Property
prop_div c r x (NonZero y) = monadicIO $ do
    let local = x `P.div` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `div` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_rem :: ArduinoConnection -> RemoteRef Int -> Int -> NonZero Int -> Property
prop_rem c r x (NonZero y) = monadicIO $ do
    let local = x `P.rem` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `rem` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_quot :: ArduinoConnection -> RemoteRef Int -> Int -> NonZero Int -> Property
prop_quot c r x (NonZero y) = monadicIO $ do
    let local = x `P.quot` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `quot` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_mod :: ArduinoConnection -> RemoteRef Int -> Int -> NonZero Int -> Property
prop_mod c r x (NonZero y) = monadicIO $ do
    let local = x `P.mod` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `mod` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_comp :: ArduinoConnection -> RemoteRef Int -> Int -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRefE r $ complement (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_and :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) .&. (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_or :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_or c r x y = monadicIO $ do
    let local = x DB..|. y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) .|. (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_xor :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_xor c r x y = monadicIO $ do
    let local = x `DB.xor` y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `xor` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_shiftL :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_shiftL c r x y = monadicIO $ do
    let local = x `DB.shiftL` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `shiftL` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_shiftR :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_shiftR c r x y = monadicIO $ do
    let local = x `DB.shiftR` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `shiftR` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_setBit :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_setBit c r x y = monadicIO $ do
    let local = x `DB.setBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `setBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_clearBit :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Property
prop_clearBit c r x y = monadicIO $ do
    let local = x `DB.clearBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `clearBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_testBit :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_testBit c r x y = monadicIO $ do
    let local = x `DB.testBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) `testBit` (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_from8 :: ArduinoConnection -> RemoteRef Int -> Word8 -> Property
prop_from8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_from16 :: ArduinoConnection -> RemoteRef Int -> Word16 -> Property
prop_from16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_from32 :: ArduinoConnection -> RemoteRef Int -> Word32 -> Property
prop_from32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromI8 :: ArduinoConnection -> RemoteRef Int -> Int8 -> Property
prop_fromI8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromI16 :: ArduinoConnection -> RemoteRef Int -> Int16 -> Property
prop_fromI16 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRefE r $ fromIntegralB (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromFTrunc :: ArduinoConnection -> RemoteRef Int -> Float -> Property
prop_fromFTrunc c r x = monadicIO $ do
    let local = P.truncate x
    remote <- run $ send c $ do
        writeRemoteRefE r $ Data.Boolean.Numbers.truncate (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromFRound :: ArduinoConnection -> RemoteRef Int -> Float -> Property
prop_fromFRound c r x = monadicIO $ do
    let local = P.round x
    remote <- run $ send c $ do
        writeRemoteRefE r $ Data.Boolean.Numbers.round (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromFCeil :: ArduinoConnection -> RemoteRef Int -> Float -> Property
prop_fromFCeil c r x = monadicIO $ do
    let local = P.ceiling x
    remote <- run $ send c $ do
        writeRemoteRefE r $ Data.Boolean.Numbers.ceiling (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_fromFFloor :: ArduinoConnection -> RemoteRef Int -> Float -> Property
prop_fromFFloor c r x = monadicIO $ do
    let local = P.floor x
    remote <- run $ send c $ do
        writeRemoteRefE r $ Data.Boolean.Numbers.floor (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_ifb :: ArduinoConnection -> RemoteRef Int -> Bool -> Int -> Int -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRefE r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) ==* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) /=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Int -> Int -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_arith :: ArduinoConnection -> RemoteRef Int ->
              Int -> Int -> Int -> Int -> Int -> NonZero Int -> Property
prop_arith c r a b d e f (NonZero g) = monadicIO $ do
    let local = a * b + d * e - f `P.div` g
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) `div` (lit g)
        v <- readRemoteRefE r
        return v
    assert (local == litEval remote)

prop_bind :: ArduinoConnection -> RemoteRef Int -> Int -> Int -> Int -> Int -> Property
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
    assert (local == litEval remote)

prop_while :: ArduinoConnection -> Int8 -> Property
prop_while c x = monadicIO $ do
    let x'::Int = fromIntegral x
    let local = x'
    remote <- run $ send c $ do
        v <- whileE (lit (-128::Int)) (\z -> z <* lit x') (\z -> return $ z + 1)
        return v
    assert (local == litEval remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refI <- send conn $ newRemoteRefE 0
    refB  <- send conn $ newRemoteRefE (lit False)
    print "Negation Tests:"
    quickCheck (prop_neg conn refI)
    print "Signum Tests:"
    quickCheck (prop_sign conn refI)
    print "Addition Tests:"
    quickCheck (prop_add conn refI)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn refI)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn refI)
    print "Division Tests:"
    quickCheck (prop_div conn refI)
    print "Remainder Tests:"
    quickCheck (prop_rem conn refI)
    print "Quotient Tests:"
    quickCheck (prop_quot conn refI)
    print "Modulo Tests:"
    quickCheck (prop_mod conn refI)
    print "Complement Tests:"
    quickCheck (prop_comp conn refI)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn refI)
    print "Bitwise Or Tests:"
    quickCheck (prop_or conn refI)
    print "Bitwise Xor Tests:"
    quickCheck (prop_xor conn refI)
    print "Shift Left Tests:"
    quickCheck (prop_shiftL conn refI)
    print "Shift Right Tests:"
    quickCheck (prop_shiftR conn refI)
    print "Set Bit Tests:"
    quickCheck (prop_setBit conn refI)
    print "Clear Bit Tests:"
    quickCheck (prop_clearBit conn refI)
    print "Test Bit Tests:"
    quickCheck (prop_testBit conn refB)
    print "From Word8 Tests:"
    quickCheck (prop_from8 conn refI)
    print "From Word16 Tests:"
    quickCheck (prop_from16 conn refI)
    print "From Word32 Tests:"
    quickCheck (prop_from32 conn refI)
    print "From Int8 Tests:"
    quickCheck (prop_fromI8 conn refI)
    print "From Int16 Tests:"
    quickCheck (prop_fromI16 conn refI)
    print "From Float Truncate Tests:"
    quickCheck (prop_fromFTrunc conn refI)
    print "From Float Round Tests:"
    quickCheck (prop_fromFRound conn refI)
    print "From Float Ceiling Tests:"
    quickCheck (prop_fromFCeil conn refI)
    print "From Float Floor Tests:"
    quickCheck (prop_fromFFloor conn refI)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refI)
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
    quickCheck (prop_arith conn refI)
    print "Bind Tests:"
    quickCheck (prop_bind conn refI)
    print "While Tests:"
    quickCheck (prop_while conn)
    closeArduino conn
