{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButton
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

analogKey :: () -> Arduino Word8
analogKey _ = do
    v <- analogRead button2
    case v of
      _ | v < 30  -> return (keyValue KeyRight)
      _ | v < 150 -> return (keyValue KeyUp)
      _ | v < 350 -> return (keyValue KeyDown)
      _ | v < 535 -> return (keyValue KeyLeft)
      _ | v < 760 -> return (keyValue KeySelect)
      _           -> analogKey ()

wait :: Arduino ()
wait = wait' ()

wait' :: () -> Arduino ()
wait' p = do
    b <- digitalRead button1
    if b then return () else wait' ()

wait2 :: Arduino ()
wait2 = do
    b <- digitalRead button1
    if b then return () else wait2

blink :: Word8 -> Arduino ()
blink 0 = return ()
blink t = do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blink ( t-1 )

blink2 :: Word8 -> Arduino Bool
blink2 0 = digitalRead 3
blink2 t = do
    digitalWrite led True
    delayMillis 1000
    digitalWrite led False
    delayMillis 1000
    blink2 ( t-1 )

recurProg :: Arduino ()
recurProg = do
    setPinMode led OUTPUT
    setPinMode button1 INPUT
    setPinMode button2 INPUT
    wait2
    blink2 3
    analogKey ()
    return ()

main :: IO ()
main = do
--  putStrLn $ show recurProg
    compileProgram recurProg "iterBlink.ino"

