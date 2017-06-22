{-# OPTIONS_GHC -fplugin=System.Hardware.Haskino.ShallowDeepPlugin #-}
-- {-# OPTIONS_GHC -fenable-rewrite-rules #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc1
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Two button example used for rewrite
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Rewrite.TwoButtonFunc1 (myRead1, myRead2, myRead3, myWrite) where

import System.Hardware.Haskino
import Control.Monad
import Data.Word
import Data.Boolean

myRead1 :: Word8 -> Arduino Bool
myRead1 p = do
    delayMillis 100
    a <- digitalRead (p+1)
    return (not a)

myRead2 :: Word8 -> Arduino Bool
myRead2 p = do
    delayMillis 100
    digitalRead (p+1)

myRead3 :: Word8 -> Arduino Bool
myRead3 p = do
    delayMillis 100
    return True

myWrite :: Word8 -> Bool -> Arduino ()
myWrite p b = do
    delayMillis 100
    digitalWrite (p+1) (not b)

extraFunc :: Word32 -> Arduino ()
extraFunc a = delayMillis (a * 100)

