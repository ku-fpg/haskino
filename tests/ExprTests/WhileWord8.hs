-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.WhileWord8
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Whiles returning a Expr Word8
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Test.WhileWord8 where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger, (<*) )
import qualified Prelude as P
import System.Hardware.Haskino
import Data.Boolean
import Data.Boolean.Numbers
import Data.Boolean.Bits
import Data.Int
import Data.Word
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEval8 :: Expr Word8 -> Word8
litEval8 (LitW8 w) = w

prop_while :: ArduinoConnection -> NonZero Word8 -> Property
prop_while c (NonZero x) = monadicIO $ do
    let local = x
    remote <- run $ send c $ do
        v <- whileE (lit 0) (\z -> z <* lit x) (\z -> return $ z + 1)
        return v
    assert (local == litEval8 remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    print "While Tests:"
    quickCheck (prop_while conn)
    closeArduino conn
