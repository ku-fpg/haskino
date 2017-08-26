-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurLetTestE
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Recursion test example used for rewrite written in shallow version.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurLetTestE(recurProgE) where

import Prelude hiding ((<*))
import System.Hardware.Haskino
import Control.Monad
import Control.Monad.Fix
import Data.Word
import Data.Boolean

data Key = KeyNone
         | KeyRight
         | KeyLeft
         | KeyUp
         | KeyDown
         | KeySelect
  deriving (Enum)

keyValue :: Key -> Word8
keyValue k = fromIntegral $ fromEnum k

led = 6
button1 = 2
button2 = 3

analogKey :: Arduino (Expr Word8)
analogKey = iterateE LitUnit (\x -> do
                v <- analogReadE button2
                ifThenElseEither (v <* 30) (return (ExprRight (lit (keyValue KeyRight))))
                  (ifThenElseEither (v <* 150) (return (ExprRight (lit (keyValue KeyUp))))
                    (ifThenElseEither (v <* 350) (return (ExprRight (lit (keyValue KeyDown))))
                      (ifThenElseEither (v <* 535) (return (ExprRight (lit (keyValue KeyLeft))))
                        (ifThenElseEither (v <* 760) (return (ExprRight (lit (keyValue KeySelect))))
                           (return (ExprLeft LitUnit)))))))


recurProgE :: Arduino (Expr ())
recurProgE = do
    setPinModeE led OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    wait
    blink 2
    wait
    blink 3
    return LitUnit
  where
    wait :: Arduino (Expr ())
    wait = iterateE LitUnit (\x -> do
                b <- digitalReadE button1
                ifThenElseEither b (return (ExprRight LitUnit)) (return (ExprLeft LitUnit)))

    blink :: Expr Word8 -> Arduino ( Expr () )
    blink t = iterateE t (\x -> do
                ifThenElseEither (x `eqE` 0) (return (ExprRight LitUnit)) (do
                                                                    digitalWriteE led true
                                                                    delayMillisE 1000
                                                                    digitalWriteE led false
                                                                    delayMillisE 1000
                                                                    return (ExprLeft ( x-1 ))
                                                                )
              )

