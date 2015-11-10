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
import Data.Word
import qualified Data.Bits as DB
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEval :: Expr Word16 -> Word16
litEval (Lit16 w) = w

prop_neg :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_neg c r x = monadicIO $ do
    let local = negate x
    remote <- run $ send c $ do
        writeRemoteRef r $ negate (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_sign :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_sign c r x = monadicIO $ do
    let local = signum x
    remote <- run $ send c $ do
        writeRemoteRef r $ signum (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_add :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) + (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_sub :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) - (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_mult :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) * (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_div :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_div c r x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.div` y
            remote <- run $ send c $ do
                writeRemoteRef r $ (lit x) `div` (lit y)
                v <- readRemoteRef r
                return v
            assert (local == litEval remote)

prop_rem :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_rem c r x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.rem` y
            remote <- run $ send c $ do
                writeRemoteRef r $ (lit x) `rem` (lit y)
                v <- readRemoteRef r
                return v
            assert (local == litEval remote)

prop_comp :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRef r $ complement (lit x) 
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_and :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .&. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_or :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_or c r x y = monadicIO $ do
    let local = x DB..|. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .|. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_xor :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word16 -> Property
prop_xor c r x y = monadicIO $ do
    let local = x `DB.xor` y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `xor` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_shiftL :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_shiftL c r x y = monadicIO $ do
    let local = x `DB.shiftL` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftL` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_shiftR :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_shiftR c r x y = monadicIO $ do
    let local = x `DB.shiftR` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `shiftR` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_setBit :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_setBit c r x y = monadicIO $ do
    let local = x `DB.setBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `setBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_clearBit :: ArduinoConnection -> RemoteRef Word16 -> Word16 -> Word8 -> Property
prop_clearBit c r x y = monadicIO $ do
    let local = x `DB.clearBit` (fromIntegral y)
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) `clearBit` (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_from32 :: ArduinoConnection -> RemoteRef Word16 -> Word32 -> Property
prop_from32 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_from8 :: ArduinoConnection -> RemoteRef Word16 -> Word8 -> Property
prop_from8 c r x = monadicIO $ do
    let local = fromIntegral x
    remote <- run $ send c $ do
        writeRemoteRef r $ fromIntegralB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_ifb :: ArduinoConnection -> RemoteRef Word16 -> Bool -> Word16 -> Word16 -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x + y else x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ ifB (lit b) (lit x + lit y) (lit x - lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_arith :: ArduinoConnection -> RemoteRef Word16 -> 
              Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Property
prop_arith c r a b d e f g = monadicIO $ do
    if g == 0
    then assert (True)
    else do let local = a * b + d * e - f `P.div` g
            remote <- run $ send c $ do
                writeRemoteRef r $ (lit a) * (lit b) + (lit d) * (lit e) - (lit f) `div` (lit g) 
                v <- readRemoteRef r
                return v
            assert (local == litEval remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    ref <- send conn $ newRemoteRef 0
    print "Negation Tests:"
    quickCheck (prop_neg conn ref)
    print "Signum Tests:"
    quickCheck (prop_sign conn ref)
    print "Addition Tests:"
    quickCheck (prop_add conn ref)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn ref)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn ref)
    print "Division Tests:"
    quickCheck (prop_div conn ref)
    print "Remainder Tests:"
    quickCheck (prop_rem conn ref)
    print "Complement Tests:"
    quickCheck (prop_comp conn ref)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn ref)
    print "Bitwise Or Tests:"
    quickCheck (prop_or conn ref)
    print "Bitwise Xor Tests:"
    quickCheck (prop_xor conn ref)
    print "Shift Left Tests:"
    quickCheck (prop_shiftL conn ref)
    print "Shift Right Tests:"
    quickCheck (prop_shiftR conn ref)
    print "Set Bit Tests:"
    quickCheck (prop_setBit conn ref)
    print "Clear Bit Tests:"
    quickCheck (prop_clearBit conn ref)
    print "From Word32 Tests:"
    quickCheck (prop_from32 conn ref)
    print "From Word8 Tests:"
    quickCheck (prop_from8 conn ref)
    print "ifB Tests:"
    quickCheck (prop_ifb conn ref)
    print "Arithemtic Tests:"
    quickCheck (prop_arith conn ref)
    closeArduino conn
