-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.ApRemovePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.ApRemovePass (apRemovePass) where

import CoreMonad
import GhcPlugins
import Type
import Data.List
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import qualified System.Hardware.Haskino

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts
      }

newtype BindM a = BindM { runCondM :: ReaderT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader BindEnv)

instance PassCoreM BindM where
  liftCoreM = BindM . ReaderT . const
  getModGuts = BindM $ ReaderT (return . pluginModGuts)

apRemovePass :: ModGuts -> CoreM ModGuts
apRemovePass guts = do
    bindsOnlyPass (\x -> (runReaderT (runCondM $ (mapM apRemoveBind) x) (BindEnv guts))) guts

apRemoveBind :: CoreBind -> BindM CoreBind
apRemoveBind bndr@(NonRec b e) = do
  e' <- apRemoveExpr e
  return (NonRec b e')
apRemoveBind (Rec bs) = do
  bs' <- apRemoveExpr' bs
  return $ Rec bs'

apRemoveExpr :: CoreExpr -> BindM CoreExpr
apRemoveExpr e = do
  apId <- thNameToId apNameTH
  df <- liftCoreM getDynFlags
  case e of
    Var v -> return e
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (f, args) = collectArgs e
      let defaultReturn = do
              e1' <- apRemoveExpr e1
              e2' <- apRemoveExpr e2
              return $ App e1' e2'
      case f of
          Var fv | fv == apId -> do
              case args of
                  [_, _, _, Var fv', arg] -> do
                      liftCoreM $ putMsgS "^^^^^^^^^^"
                      liftCoreM $ putMsg $ ppr fv'
                      liftCoreM $ putMsg $ ppr arg
                      return $ mkCoreApps (Var fv') [arg]
                  _ -> defaultReturn
          _ -> defaultReturn
    Lam tb e -> do
      e' <- apRemoveExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- apRemoveExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- apRemoveExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- apRemoveExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- apRemoveExpr e
      alts' <- apRemoveExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- apRemoveExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- apRemoveExpr e
      return $ Cast e' co

apRemoveExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
apRemoveExpr' [] = return []
apRemoveExpr' ((b, e) : bs) = do
  e' <- apRemoveExpr e
  bs' <- apRemoveExpr' bs
  return $ (b, e') : bs'

apRemoveExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
apRemoveExprAlts [] = return []
apRemoveExprAlts ((ac, b, a) : as) = do
  a' <- apRemoveExpr a
  bs' <- apRemoveExprAlts as
  return $ (ac, b, a') : bs'
