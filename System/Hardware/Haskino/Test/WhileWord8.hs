-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Test.ExprWord8
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Quick Check tests for Expressions returning a Expr Word8
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

prop_ifthenelse :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_ifthenelse c x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        v <- whileE (lit x) (\z -> z <* lit y) (\x -> return $ x + 1)
        return v
    assert (local == litEval8 remote)

main :: IO ()
main = do
    conn <- openArduino True "/dev/cu.usbmodem1421"
    print "IfThenElse Tests:"
    quickCheck (prop_ifthenelse conn)
    closeArduino conn
