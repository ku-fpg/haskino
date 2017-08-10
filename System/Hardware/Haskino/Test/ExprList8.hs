-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprList8
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr List8
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds #-}

module System.Hardware.Haskino.Test.ExprList8 where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger, (<*) )
import qualified Prelude as P
import System.Hardware.Haskino
import Data.Boolean
import Data.Boolean.Numbers
import Data.Boolean.Bits
import Data.Char
import Data.Int
import Data.Word
import Numeric
import qualified Data.Bits as DB
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEvalL :: Expr [Word8] -> [Word8]
litEvalL (LitList8 l) = l

litEval8 :: Expr Word8 -> Word8
litEval8 (LitW8 w) = w

litEvalB :: Expr Bool -> Bool
litEvalB (LitB b) = b

stringToBytes :: String -> [Word8]
stringToBytes s = map (\d -> fromIntegral $ ord d) s

bytesToString :: [Word8] -> String
bytesToString bs = map (\d -> chr $ fromIntegral d) bs

prop_cons :: ArduinoConnection -> RemoteRef [Word8] -> Word8  -> [Word8] -> Property
prop_cons c r x xs = monadicIO $ do
    let local = x : xs
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) *: (lit xs)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_app :: ArduinoConnection -> RemoteRef [Word8] -> [Word8]  -> [Word8] -> Property
prop_app c r xs ys = monadicIO $ do
    let local = xs ++ ys
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit xs) ++* (lit ys)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_tail :: ArduinoConnection -> RemoteRef [Word8] -> NonEmptyList Word8 -> Property
prop_tail c r (NonEmpty xs) = monadicIO $ do
    let local = tail xs
    remote <- run $ send c $ do
        writeRemoteRefE r $ tailE (lit xs)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_len :: ArduinoConnection -> RemoteRef Word8 -> [Word8] -> Property
prop_len c r xs = monadicIO $ do
    let local = length xs
    remote <- run $ send c $ do
        writeRemoteRefE r $ len (lit xs)
        v <- readRemoteRefE r
        return v
    assert (local == (fromIntegral $ litEval8 remote))

prop_elem :: ArduinoConnection -> RemoteRef Word8 -> NonEmptyList Word8 -> Property
prop_elem c r (NonEmpty xs) = 
    forAll (choose (0::Word8, fromIntegral $ length xs - 1)) $ \e ->
        monadicIO $ do
            let local = xs !! (fromIntegral e)
            remote <- run $ send c $ do
                writeRemoteRefE r $ (lit xs) !!* (lit e)
                v <- readRemoteRefE r
                return v
            assert (local == (fromIntegral $ litEval8 remote))

prop_head :: ArduinoConnection -> RemoteRef Word8 -> NonEmptyList Word8 -> Property
prop_head c r (NonEmpty xs) = monadicIO $ do
    let local = head xs
    remote <- run $ send c $ do
        writeRemoteRefE r $ headE (lit xs)
        v <- readRemoteRefE r
        return v
    assert (local == (fromIntegral $ litEval8 remote))
-- ToDo: generate prop_elem_out_of_bounds

prop_ifb :: ArduinoConnection -> RemoteRef [Word8] -> Bool -> Word8 -> Word8 -> 
            [Word8] -> [Word8] -> Property
prop_ifb c r b x y xs ys = monadicIO $ do
    let local = if b then x : xs else y : ys
    remote <- run $ send c $ do
        writeRemoteRefE r $ ifB (lit b) (lit x *: lit xs) (lit y *: lit ys)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) ==* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) /=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) <=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> [Word8] -> [Word8] -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRefE r $ (lit x) >=* (lit y)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalB remote)

prop_showW8 :: ArduinoConnection -> RemoteRef [Word8] -> Word8 -> Property
prop_showW8 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showW16 :: ArduinoConnection -> RemoteRef [Word8] -> Word16 -> Property
prop_showW16 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showW32 :: ArduinoConnection -> RemoteRef [Word8] -> Word32 -> Property
prop_showW32 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showI8 :: ArduinoConnection -> RemoteRef [Word8] -> Int8 -> Property
prop_showI8 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showI16 :: ArduinoConnection -> RemoteRef [Word8] -> Int16 -> Property
prop_showI16 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showI32 :: ArduinoConnection -> RemoteRef [Word8] -> Int32 -> Property
prop_showI32 c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

prop_showFloat :: ArduinoConnection -> RemoteRef [Word8] -> Float -> Property
prop_showFloat c r x = monadicIO $ do
    let local = showFFloat (Just 2) x ""
    remote <- run $ send c $ do
        writeRemoteRefE r $ showFFloatE (Just (lit 2)) (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == (bytesToString $ litEvalL remote))

prop_showBool :: ArduinoConnection -> RemoteRef [Word8] -> Bool -> Property
prop_showBool c r x = monadicIO $ do
    let local = stringToBytes $ show x
    remote <- run $ send c $ do
        writeRemoteRefE r $ showE (lit x)
        v <- readRemoteRefE r
        return v
    assert (local == litEvalL remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refL <- send conn $ newRemoteRefE (lit [])
    refW8 <- send conn $ newRemoteRefE (lit 0)
    refB <- send conn $ newRemoteRefE (lit False)
    print "Cons Tests:"
    quickCheck (prop_cons conn refL)
    print "Apppend Tests:"
    quickCheck (prop_app conn refL)
    print "Length Tests:"
    quickCheck (prop_len conn refW8)
    print "Element Tests:"
    quickCheck (prop_len conn refW8)
    print "Head Tests:"
    quickCheck (prop_head conn refW8)
    print "Tail Tests:"
    quickCheck (prop_tail conn refL)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refL)
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
    print "Show Word8 Tests:"
    quickCheck (prop_showW8 conn refL)
    print "Show Word16 Tests:"
    quickCheck (prop_showW16 conn refL)
    print "Show Word32 Tests:"
    quickCheck (prop_showW32 conn refL)
    print "Show Int8 Tests:"
    quickCheck (prop_showI8 conn refL)
    print "Show Int16 Tests:"
    quickCheck (prop_showI16 conn refL)
    print "Show Int32 Tests:"
    quickCheck (prop_showI32 conn refL)
    print "Show Bool Tests:"
    quickCheck (prop_showBool conn refL)
    print "Show Float Tests:"
    quickCheck (prop_showFloat conn refL)
    closeArduino conn
