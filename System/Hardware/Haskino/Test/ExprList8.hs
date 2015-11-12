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

module System.Hardware.Haskino.Test.ExprWord8 where

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

litEvalL :: Expr [Word8] -> [Word8]
litEvalL (LitList8 l) = l

litEval8 :: Expr Word8 -> Word8
litEval8 (Lit8 w) = w

prop_cons :: ArduinoConnection -> RemoteRef [Word8] -> Word8  -> [Word8] -> Property
prop_cons c r x xs = monadicIO $ do
    let local = x : xs
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) *: (lit xs)
        v <- readRemoteRef r
        return v
    assert (local == litEvalL remote)

prop_app :: ArduinoConnection -> RemoteRef [Word8] -> [Word8]  -> [Word8] -> Property
prop_app c r xs ys = monadicIO $ do
    let local = xs ++ ys
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit xs) ++* (lit ys)
        v <- readRemoteRef r
        return v
    assert (local == litEvalL remote)

prop_len :: ArduinoConnection -> RemoteRef Word8 -> [Word8] -> Property
prop_len c r xs = monadicIO $ do
    let local = length xs
    remote <- run $ send c $ do
        writeRemoteRef r $ len (lit xs)
        v <- readRemoteRef r
        return v
    assert (local == (fromIntegral $ litEval8 remote))

prop_elem :: ArduinoConnection -> RemoteRef Word8 -> NonEmpty Word8 -> Word8 -> Property
prop_elem c r (NonEmpty xs) e = monadicIO $ do
    let local = xs !! e
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit xs) !!* (lit e)
        v <- readRemoteRef r
        return v
    assert (local == (fromIntegral $ litEval8 remote))

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    refL <- send conn $ newRemoteRef (lit [])
    ref8 <- send conn $ newRemoteRef (lit 0)
    print "Cons Tests:"
    quickCheck (prop_cons conn refL)
    print "Apppend Tests:"
    quickCheck (prop_app conn refL)
    print "Length Tests:"
    quickCheck (prop_len conn ref8)
    print "Element Tests:"
    quickCheck (prop_len conn ref8)
    closeArduino conn
