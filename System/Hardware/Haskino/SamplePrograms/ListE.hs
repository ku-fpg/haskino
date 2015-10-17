-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.ListE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- TBD
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.ListE where

import Control.Monad.Trans (liftIO)
import Data.Boolean.Bits
import System.Hardware.Haskino
-- import Data.Boolean

listE :: IO ()
listE = withArduino True "/dev/cu.usbmodem1421" $ do
           let l1 = lit [1,2,3,4]
           let l2 = lit [5,6,7,8]
           x <- newRemoteRef (l1 ++* l2)
           a <- readRemoteRef x
           return ()
