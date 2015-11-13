-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprBool
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr Bool
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Test.ExprBool where

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

litEvalB :: Expr Bool -> Bool
litEvalB (LitB w) = w

prop_not :: ArduinoConnection -> RemoteRef Bool -> Bool -> Property
prop_not c r x = monadicIO $ do
    let local = not x
    remote <- run $ send c $ do
        writeRemoteRef r $ notB (lit x)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_and :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_and c r x y = monadicIO $ do
    let local = x && y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) &&* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_or :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_or c r x y = monadicIO $ do
    let local = x || y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) ||* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_ifb :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Bool -> Property
prop_ifb c r b x y = monadicIO $ do
    let local = if b then x else y
    remote <- run $ send c $ do
        writeRemoteRef r $ ifB (lit b) (lit x) (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_eq :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_eq c r x y = monadicIO $ do
    let local = x == y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) ==* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_neq :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_neq c r x y = monadicIO $ do
    let local = x /= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) /=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lt :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_lt c r x y = monadicIO $ do
    let local = x < y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gt :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_gt c r x y = monadicIO $ do
    let local = x > y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_lte :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_lte c r x y = monadicIO $ do
    let local = x <= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) <=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

prop_gte :: ArduinoConnection -> RemoteRef Bool -> Bool -> Bool -> Property
prop_gte c r x y = monadicIO $ do
    let local = x >= y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) >=* (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEvalB remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refB <- send conn $ newRemoteRef (lit False)
    print "Not Tests:"
    quickCheck (prop_not conn refB)
    print "And Tests:"
    quickCheck (prop_and conn refB)
    print "Or Tests:"
    quickCheck (prop_or conn refB)
    print "ifB Tests:"
    quickCheck (prop_ifb conn refB)
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
    closeArduino conn
