-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangePass (bindChangePass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.Dictionary (buildDictionaryT, PassCoreM(..), )

import qualified System.Hardware.Haskino
import qualified System.Hardware.Haskino.Data
import qualified System.Hardware.Haskino.Expr

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts
      }

newtype BindM a = BindM { runBindM :: ReaderT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader BindEnv)

instance PassCoreM BindM where
  liftCoreM = BindM . ReaderT . const
  getModGuts = BindM $ ReaderT (return . pluginModGuts)

bindChangePass :: ModGuts -> CoreM ModGuts
bindChangePass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeBind) x) (BindEnv guts))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ exprType e
  let tyCon_m = splitTyConApp_maybe retTy
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") &&
                                  (showSDoc df (ppr retTy') /= "()") -> do
          let tyCon_m' = splitTyConApp_maybe retTy'
          case tyCon_m' of
              -- We do not want types of Arduino (Expr a), so we look for an
              -- empty list of types, and just a TyCon from the tyCon_m'.
              Just (retTyCon', []) -> do
                  liftCoreM $ putMsg $ ppr b
                  liftCoreM $ putMsg $ ppr retTyCon'
                  let (bs, e') = collectBinders e
                  liftCoreM $ putMsg $ ppr bs
                  return bndr
              _ -> return bndr
      _ -> return bndr
changeBind (Rec bs) = do
  return $ Rec bs
