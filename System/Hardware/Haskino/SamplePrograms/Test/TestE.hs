-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.SamplePrograms.Test.TestE
--                Based on System.Hardware.Arduino
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-------------------------------------------------------------------------------

module System.Hardware.Haskino.SamplePrograms.Test.TestE where

import System.Hardware.Haskino
import Control.Monad.Trans (liftIO)
import Data.Boolean
import Data.Word

testE :: Float -> IO ()
testE x = withArduino False "/dev/cu.usbmodem1421" $ do
               r <- newRemoteRef 0.0
               let local = atanh x
               writeRemoteRef r $ atanh (lit x)
               remote <- readRemoteRef r
               LiftIO $ print $ "Local  :" ++ show local
               LiftIO $ print $ "Remote :" ++ show remote
