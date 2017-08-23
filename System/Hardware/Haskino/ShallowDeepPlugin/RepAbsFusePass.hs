-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Rep Abs Fusion Pass - Performs the equivelent of the following rules:
--    forall (x :: ExprB a => a)
--    rep_(abs_(x))
--      =
--    x
--
--    forall (m :: ExprB a => Arduino (Expr a)).
--    rep_ <$> (abs_ <$> m)
--      =
--    m
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass (repAbsFusePass) where

import CoreMonad
import GhcPlugins
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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

repAbsFusePass :: ModGuts -> CoreM ModGuts
repAbsFusePass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeFuse) x) (BindEnv guts))) guts

changeFuse :: CoreBind -> BindM CoreBind
changeFuse (NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeFuseExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeFuse (Rec bs) = do
  bs' <- changeFuse' bs
  return $ Rec bs'

changeFuse' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeFuse' [] = return []
changeFuse' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- changeFuseExpr e'
  let e''' = mkLams lbs e''
  bs' <- changeFuse' bs
  return $ (b, e''') : bs'

changeFuseExpr :: CoreExpr -> BindM CoreExpr
changeFuseExpr e = do
  repId <- thNameToId repNameTH
  absId <- thNameToId absNameTH
  fmapId <- thNameToId fmapNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    -- Look for case of rep_(abs_(x))
    (Var fv) :$ (Type _) :$ _ :$ ((Var fv') :$ (Type _) :$ e') | fv == repId && fv' == absId -> do
        e'' <- changeFuseExpr e'
        return e''
    -- Look for case of rep_ <$> (abs_ <$> m)
    (Var fv) :$ (Type _) :$ (Type _) :$ (Type _) :$ _ :$ ((Var fv1) :$ Type _ :$ _) :$ ((Var fv2) :$ (Type _) :$ (Type _) :$ (Type _) :$ _ :$ (Var fv3 :$ Type _) :$ e'' )| fv == fmapId && fv1 == repId && fv2 == fmapId && fv3 == absId -> do
        e''' <- changeFuseExpr e''
        return e'''
    App e1 e2 -> do
        e1' <- changeFuseExpr e1
        e2' <- changeFuseExpr e2
        return $ App e1' e2'
    Lam tb el -> do
      e' <- changeFuseExpr el
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeFuseExpr body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- changeFuseExpr el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeFuseExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- changeFuseExpr ec
      alts' <- changeFuseExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- changeFuseExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- changeFuseExpr ec
      return $ Cast e' co

changeFuseExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeFuseExpr' [] = return []
changeFuseExpr' ((b, e) : bs) = do
  e' <- changeFuseExpr e
  bs' <- changeFuseExpr' bs
  return $ (b, e') : bs'

changeFuseExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeFuseExprAlts [] = return []
changeFuseExprAlts ((ac, b, a) : as) = do
  a' <- changeFuseExpr a
  bs' <- changeFuseExprAlts as
  return $ (ac, b, a') : bs'
