{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Test.ExprWord8 where

import Prelude hiding 
  ( quotRem, divMod, quot, rem, div, mod, properFraction, fromInteger, toInteger )
import qualified Prelude as P
import System.Hardware.Haskino
-- import System.Hardware.Haskino.Expr
import Data.Boolean
import Data.Boolean.Numbers
import Data.Boolean.Bits
import Data.Word
import qualified Data.Bits as DB
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

litEval :: Expr Word8 -> Word8
litEval (Lit8 w) = w

prop_add :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_add c x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ eval8 $ (lit x) + (lit y)
    assert (local == remote)

prop_sub :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_sub c x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ eval8 $ (lit x) - (lit y)
    assert (local == remote)

prop_mult :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_mult c x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ eval8 $ (lit x) * (lit y)
    assert (local == remote)

prop_div :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_div c x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.div` y
            remote <- run $ send c $ eval8 $ (lit x) `div` (lit y)
            assert (local == remote)

prop_rem :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_rem c x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.rem` y
            remote <- run $ send c $ eval8 $ (lit x) `rem` (lit y)
            assert (local == remote)

prop_comp :: ArduinoConnection -> Word8 -> Property
prop_comp c x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ eval8 $ complement (lit x) 
    assert (local == remote)

prop_and :: ArduinoConnection -> Word8 -> Word8 -> Property
prop_and c x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ eval8 $ (lit x) .&. (lit y)
    assert (local == remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    print "Addition Tests:"
    quickCheck (prop_add conn)
    print "Subtraction Tests:"
    quickCheck (prop_sub conn)
    print "Multiplcation Tests:"
    quickCheck (prop_mult conn)
    print "Division Tests:"
    quickCheck (prop_div conn)
    print "Remainder Tests:"
    quickCheck (prop_rem conn)
    print "Complement Tests:"
    quickCheck (prop_comp conn)
    print "Bitwise And Tests:"
    quickCheck (prop_and conn)
    closeArduino conn
