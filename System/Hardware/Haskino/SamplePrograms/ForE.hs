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
import Data.Word
import Data.Boolean
import System.Hardware.Haskino

forE :: IO ()
forE = withArduino False "/dev/cu.usbmodem1421" $ do
           let l = lit [1,2,3,4]
           r <- newRemoteRef 0
           forInE l (\x -> modifyRemoteRef r (\a -> a + x))
           s <- readRemoteRef r
           liftIO $ print s
           return ()
