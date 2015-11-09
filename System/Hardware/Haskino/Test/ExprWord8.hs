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

prop_add :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_add c r x y = monadicIO $ do
    let local = x + y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) + (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_sub :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_sub c r x y = monadicIO $ do
    let local = x - y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) - (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_mult :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_mult c r x y = monadicIO $ do
    let local = x * y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) * (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_div :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_div c r x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.div` y
            remote <- run $ send c $ do
                writeRemoteRef r $ (lit x) `div` (lit y)
                v <- readRemoteRef r
                return v
            assert (local == litEval remote)

prop_rem :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_rem c r x y = monadicIO $ do
    if y == 0
    then assert (True)
    else do let local = x `P.rem` y
            remote <- run $ send c $ do
                writeRemoteRef r $ (lit x) `rem` (lit y)
                v <- readRemoteRef r
                return v
            assert (local == litEval remote)

prop_comp :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Property
prop_comp c r x = monadicIO $ do
    let local = DB.complement x
    remote <- run $ send c $ do
        writeRemoteRef r $ complement (lit x) 
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

prop_and :: ArduinoConnection -> RemoteRef Word8 -> Word8 -> Word8 -> Property
prop_and c r x y = monadicIO $ do
    let local = x DB..&. y
    remote <- run $ send c $ do
        writeRemoteRef r $ (lit x) .&. (lit y)
        v <- readRemoteRef r
        return v
    assert (local == litEval remote)

main :: IO ()
main = do
    conn <- openArduino False "/dev/cu.usbmodem1421"
    ref <- send conn $ newRemoteRef 0
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
    closeArduino conn
