-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Rewrite.TranslationTest
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Translation Plugin Test
-------------------------------------------------------------------------------

module Main where

import System.Hardware.Haskino.SamplePrograms.Rewrite.TransTest
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransLetTest
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTest
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransMultiTest2
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransFuncTest
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransIfTest
import System.Hardware.Haskino.SamplePrograms.Rewrite.TransRecurTest

main :: IO ()
main = do
  transTest
  transLetTest
  transFuncTest
  transMultiTest
  transIfTest
  transRecurTest
