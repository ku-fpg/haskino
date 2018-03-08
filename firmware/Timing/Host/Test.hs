-------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD.
-------------------------------------------------------------------------------
module Main where

import System.Hardware.Haskino

prog :: Arduino ()
prog = loop $
          do 
            --digitalWrite 2 True
            _ <- digitalRead 2
            return ()

main :: IO ()
main = withArduino True "/dev/cu.usbmodem1421" prog
-- main = compileProgram prog "listPack.ino"


-- main = putStrLn $ show newEvenOdd
