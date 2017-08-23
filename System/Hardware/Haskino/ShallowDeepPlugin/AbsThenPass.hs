-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.AbsThenPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Eliminate "abs after >>"" pass
-- Applies the equivelent of this rule, to eliminate abs expressions which
-- will never be evaluated due to Haskell lazy evaluation.
-- forall (f :: Arduino (Expr a)) (g :: Arduino (Expr b))
--     (abs_ <$> f) >> g
--        =
--     f >> g
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.AbsThenPass (absThenPass) where

import Control.Monad.Reader
import CoreMonad
import GhcPlugins

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

absThenPass :: ModGuts -> CoreM ModGuts
absThenPass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM absThen) x) (BindEnv guts))) guts

absThen :: CoreBind -> BindM CoreBind
absThen (NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- absThenExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
absThen (Rec bs) = do
  bs' <- absThen' bs
  return $ Rec bs'

absThen' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
absThen' [] = return []
absThen' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- absThenExpr e'
  let e''' = mkLams lbs e''
  bs' <- absThen' bs
  return $ (b, e''') : bs'

absThenExpr :: CoreExpr -> BindM CoreExpr
absThenExpr e = do
  thenId <- thNameToId bindThenNameTH
  fmapId <- thNameToId fmapNameTH
  absId  <- thNameToId absNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    -- Look for expressions of the form:
    -- forall (f :: Arduino (Expr a)) (g :: Arduino (Expr b))
    --     (abs <$> f) >> g
    (Var thenV) :$ (Type m1Ty) :$ dict1 :$ (Type arg1Ty) :$ (Type arg2Ty) :$ 
      ((Var fmapV) :$ (Type _) :$ (Type _) :$ (Type _) :$ _ :$ 
        ((Var abs_V ) :$ (Type _)) :$ e1) :$ e2 | thenV == thenId && fmapV == fmapId && abs_V == absId -> do
      e1' <- absThenExpr e1
      e2' <- absThenExpr e2
      return ((Var thenV) :$ (Type m1Ty) :$ dict1 :$ (Type arg1Ty) :$ (Type arg2Ty) :$ e1' :$ e2')
    App e1 e2 -> do
      e1' <- absThenExpr e1
      e2' <- absThenExpr e2
      return $ App e1' e2'
    Lam tb el -> do
      e' <- absThenExpr el
      return $ Lam tb e'
    Let bind body -> do
      body' <- absThenExpr body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- absThenExpr el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- absThenExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- absThenExpr ec
      alts' <- absThenExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- absThenExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- absThenExpr ec
      return $ Cast e' co

absThenExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
absThenExpr' [] = return []
absThenExpr' ((b, e) : bs) = do
  e' <- absThenExpr e
  bs' <- absThenExpr' bs
  return $ (b, e') : bs'

absThenExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
absThenExprAlts [] = return []
absThenExprAlts ((ac, b, a) : as) = do
  a' <- absThenExpr a
  bs' <- absThenExprAlts as
  return $ (ac, b, a') : bs'

