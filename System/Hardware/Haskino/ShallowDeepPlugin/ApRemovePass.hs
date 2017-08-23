-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.ApRemovePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Removal of ($) app calls pass
-- This pass removes all instances of the application of the ($) function.
-- For example,
-- a <$> b c
--    =
-- a (b c)
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.ApRemovePass (apRemovePass) where

import Control.Monad.Reader
import CoreMonad
import Data.List
import Data.Functor
import GhcPlugins
import Type

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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
apRemoveBind (NonRec b e) = do
  e' <- apRemoveExpr e
  return (NonRec b e')
apRemoveBind (Rec bs) = do
  bs' <- apRemoveExpr' bs
  return $ Rec bs'

apRemoveExpr :: CoreExpr -> BindM CoreExpr
apRemoveExpr e = do
  apId <- thNameToId apNameTH
  case e of
    Var _ -> return e
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (f, args) = collectArgs e
      let defaultReturn = do
              e1' <- apRemoveExpr e1
              e2' <- apRemoveExpr e2
              return $ App e1' e2'
      -- Pattern match instances of:
      -- <$> :$ Type t1 :$ Type t2 :$ dict :$ f :$ g
      -- and replace with f :$ g
      case f of
          Var fv | fv == apId -> do
              case args of
                  [_, _, _, f', arg] -> return $ mkCoreApps f' [arg]
                  _ -> defaultReturn
          _ -> defaultReturn
    Lam tb el -> do
      e' <- apRemoveExpr el
      return $ Lam tb e'
    Let bind body -> do
      body' <- apRemoveExpr body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- apRemoveExpr el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- apRemoveExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- apRemoveExpr ec
      alts' <- apRemoveExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- apRemoveExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- apRemoveExpr ec
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
