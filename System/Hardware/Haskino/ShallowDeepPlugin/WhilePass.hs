-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.CommProcPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.WhilePass (whilePass) where

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

whilePass :: ModGuts -> CoreM ModGuts
whilePass guts = do
    bindsOnlyPass (\x -> (runReaderT (runCondM $ (mapM whileBind) x) (BindEnv guts))) guts

whileBind :: CoreBind -> BindM CoreBind
whileBind bndr@(NonRec b e) = do
  e' <- whileExpr e
  return (NonRec b e')
whileBind bndr@(Rec bs) = return bndr

whileExpr :: CoreExpr -> BindM CoreExpr
whileExpr e = do
  df <- liftCoreM getDynFlags
  whileId <- thNameToId whileTH
  whileEId <- thNameToId whileETH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    (Var bind) :$ (Type argTy) :$ dict :$ initExpr :$ condExpr :$ stepExpr | bind == whileId -> do
        initExpr' <- repExpr initExpr
        condExpr' <- changeVarRet False condExpr
        stepExpr' <- changeVarRet True stepExpr
        let whileEExpr = mkCoreApps (Var whileEId) [Type argTy, dict, initExpr', condExpr', stepExpr']
        let (tyCon, [ty]) = splitTyConApp $ exprType whileEExpr
        fmapAbsExpr (mkTyConTy tyCon) ty whileEExpr
        -- TBD - Need to abs_ <$> the whileE
        -- return $ 
    App e1 e2 -> do
        e1' <- whileExpr e1
        e2' <- whileExpr e2
        return $ App e1' e2'
    Lam tb e -> do
      e' <- whileExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- whileExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- whileExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- whileExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- whileExpr e
      alts' <- whileExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- whileExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- whileExpr e
      return $ Cast e' co

whileExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
whileExpr' [] = return []
whileExpr' ((b, e) : bs) = do
  e' <- whileExpr e
  bs' <- whileExpr' bs
  return $ (b, e') : bs'

whileExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
whileExprAlts [] = return []
whileExprAlts ((ac, b, a) : as) = do
  a' <- whileExpr a
  bs' <- whileExprAlts as
  return $ (ac, b, a') : bs'

changeVarRet :: Bool -> CoreExpr -> BindM CoreExpr
changeVarRet ret e = do
  let (bs, e') = collectBinders e
  let b = head bs
  exprTyCon <- thNameToTyCon exprTyConTH
  -- Make the type of the Expr for the specified type
  let ty' = mkTyConApp exprTyCon [varType b]
  let b' = setVarType b ty'
  e'' <- changeVarExpr b e'
  if ret 
  then do
    e''' <- fmapRepBindReturn e''
    return $ mkLams [b'] e'''
  else do
    return $ mkLams [b'] e''

changeVarExpr :: CoreBndr -> CoreExpr -> BindM CoreExpr
changeVarExpr f e = do
  df <- liftCoreM getDynFlags
  case e of
    -- Replace any occurances of the parameters "p" with "abs_ p"
    Var v | v == f -> absExpr e
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- changeVarExpr f e1
      e2' <- changeVarExpr f e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- changeVarExpr f e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeVarExpr f body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeVarExpr f e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeVarExpr' f rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeVarExpr f e
      alts' <- changeVarExprAlts f alts
      return $ Case e' tb ty alts'
    Tick ti e -> do
      e' <- changeVarExpr f e
      return $ Tick ti e'
    Cast e co -> do
      e' <- changeVarExpr f e
      return $ Cast e' co

changeVarExpr' :: CoreBndr -> [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeVarExpr' _ [] = return []
changeVarExpr' f ((b, e) : bs) = do
  e' <- changeVarExpr f e
  bs' <- changeVarExpr' f bs
  return $ (b, e') : bs'

changeVarExprAlts :: CoreBndr -> [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeVarExprAlts _ [] = return []
changeVarExprAlts f ((ac, b, a) : as) = do
  a' <- changeVarExpr f a
  bs' <- changeVarExprAlts f as
  return $ (ac, b, a') : bs'
