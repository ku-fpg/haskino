{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonIter
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino
import Control.Monad
import Control.Monad.Fix
import Data.Word
import Data.Boolean
import System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonE

led = 13
button1 = 2
button2 = 3


-- stuff :: Arduino (Iter a ())
-- stuff = done <$> return ()

blink :: Expr Word8 -> Arduino (Expr ())
blink t = iterateE t (\x -> do
            ifThenElseEither (x `eqE` 0) (return (ExprRight LitUnit)) (do
                                                                digitalWriteE led true
                                                                delayMillisE 1000
                                                                digitalWriteE led false
                                                                delayMillisE 1000
                                                                return (ExprLeft ( x-1 ))
                                                            )
          )

blink2 :: Expr Word8 -> Arduino (Expr Bool)
blink2 t = iterateE t (\x -> do
            ifThenElseEither (x `eqE` 0) (ExprRight <$> (digitalReadE (lit 2))) (do
                                                                digitalWriteE led true
                                                                delayMillisE 1000
                                                                digitalWriteE led false
                                                                delayMillisE 1000
                                                                return (ExprLeft ( x-1 ))
                                                            )
          )

recurProg :: Arduino ()
recurProg = do
    setPinModeE led OUTPUT
    setPinModeE button1 INPUT
    setPinModeE button2 INPUT
    blink2 3
    return ()

main :: IO ()
main = do
  compileProgram recurProg "recur.ino"


