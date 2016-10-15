{-# OPTIONS_GHC -fenable-rewrite-rules #-}
{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Strong.Blink
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- The /hello world/ of the arduino world, blinking the led.
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Strong.Blink where

import Prelude hiding (abs)

import System.Hardware.Haskino
import System.Hardware.Haskino.Rules
import Data.Word

blink :: IO ()
blink = withArduino True "/dev/cu.usbmodem1421" $ do
           let led = 13
           let delay = 1000
           setPinMode led OUTPUT
           loop $ do digitalWrite led True
                     a <- millis
                     b <- millis
                     delayMillis $ a
                     digitalWrite led False
                     delayMillis $ b

rep :: Arduino (Expr a) -> Arduino a
rep _ = error "Internal error: rep called"

repe ::  (Expr a) ->  a
repe _ = error "Internal error: rep called"

eval32 :: Expr Word32 -> Arduino Word32
eval32 (LitW32 w) = return w
eval32 _ = return 0

lit32 :: Word32 -> Expr Word32
lit32 w = lit w

{-# RULES 
    "delay"
    forall (d :: Word32).
    delayMillis d
      =
    delayMillisE (lit32 d)
  #-}

{-# RULES 
    "millis"
    millis = millisE >>= eval32
  #-}

{-# RULES 
    "remove_eval_lit"
    millis = millisE >>= eval32
  #-}

{-# RULES "digitalWrite"
    forall (p :: Word8) (b :: Bool).
    digitalWrite p b
      =
    digitalWriteE (lit p) (lit b)
  #-}

{-# RULES "pinMode"
    forall (p :: Word8) m.
    setPinMode p m
      =
    setPinModeE (lit p) m
  #-}

{-# RULES "loop"
    forall (m :: Arduino ()).
    loop m
      =
    loopE m
  #-}

--{-# RULES "+-intro"
--    forall (x :: Word32) (y :: Word32) .
--    x + y
--      =
--    lit x + lit y
--  #-}

--{-# RULES "abs-rep-elim" [~]
--    forall x.
--    lit (repe x) = x
--  #-}
