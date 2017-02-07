-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangeArgPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangeArgPass (bindChangeArgPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.Dictionary (buildDictionaryT, PassCoreM(..), )

import qualified System.Hardware.Haskino

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

bindChangeArgPass :: ModGuts -> CoreM ModGuts
bindChangeArgPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeBind) x) (BindEnv guts))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ varType b
  let (bs, e') = collectBinders e
  liftCoreM $ putMsg $ ppr b
  liftCoreM $ putMsg $ ppr bs
  liftCoreM $ putMsg $ ppr argTys
  liftCoreM $ putMsgS $ "-----------"
  let tyCon_m = splitTyConApp_maybe retTy
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") -> do
          zipBsArgTys <- mapM changeArg (zip bs argTys)
          let (bs', argTys') = unzip zipBsArgTys
          return bndr
      _ -> return bndr
changeBind (Rec bs) = do
  return $ Rec bs

changeArg :: (CoreBndr, Type) -> BindM (CoreBndr, Type)
changeArg (b, ty) = do
  let tyCon_m = splitTyConApp_maybe ty
  case tyCon_m of
    Nothing -> do
        -- Lookup the GHC type constructor of Expr
        Just exprName <- liftCoreM $ thNameToGhcName ''System.Hardware.Haskino.Expr
        exprTyCon <- liftCoreM $ lookupTyCon exprName
        -- Make the type of the Expr for the specified type
        let ty' = mkTyConApp exprTyCon [ty]
        let b' = setVarType b ty'
        return (b', ty')
    _       -> return (b, ty)

