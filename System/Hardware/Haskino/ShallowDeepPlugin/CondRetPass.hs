-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.CondRetPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Return Pass 
-- ifThenElseE (rep b) t e => ifThenElseE (rep b) (rep <$> t) (rep <$> e)
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.CondRetPass (condRetPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader
import Var

import System.Hardware.Haskino.ShallowDeepPlugin.Dictionary (buildDictionaryT, 
                                           buildDictionaryTyConT, 
                                           PassCoreM(..), 
                                           thNameToId, thNameToTyCon)

import qualified System.Hardware.Haskino
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

condRetPass :: ModGuts -> CoreM ModGuts
condRetPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeCond) x) (BindEnv guts))) guts

changeCond :: CoreBind -> BindM CoreBind
changeCond bndr@(NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeCondExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeCond (Rec bs) = do
  return $ Rec bs

changeCondExpr :: CoreExpr -> BindM CoreExpr
changeCondExpr e = do
  df <- liftCoreM getDynFlags
  ifThenElseId <- thNameToId 'System.Hardware.Haskino.ifThenElseE
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (App (App (Var f) ty) dict) be) e1) e2 | f == ifThenElseId -> do
        e1' <- changeReturn e1
        e2' <- changeReturn e2
        return $ mkCoreApps (Var f) [ty, dict, be, e1', e2']
    App e1 e2 -> do
        e1' <- changeCondExpr e1
        e2' <- changeCondExpr e2
        return $ App e1' e2'
    Lam tb e -> do
      e' <- changeCondExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeCondExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeCondExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeCondExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeCondExpr e
      alts' <- changeCondExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeCondExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeCondExpr e
      return $ Cast e' co

varString :: Id -> String
varString = occNameString . nameOccName . Var.varName

changeCondExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeCondExpr' [] = return []
changeCondExpr' ((b, e) : bs) = do
  e' <- changeCondExpr e
  bs' <- changeCondExpr' bs
  return $ (b, e') : bs'

changeCondExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeCondExprAlts [] = return []
changeCondExprAlts ((ac, b, a) : as) = do
  a' <- changeCondExpr a
  bs' <- changeCondExprAlts as
  return $ (ac, b, a') : bs'

changeReturn :: CoreExpr -> BindM CoreExpr
changeReturn e = do
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId '(>>=)
    thenId <- thNameToId '(>>)
    case f of
      Var fv -> do
        if fv == bindId || fv == thenId
        then do
            la' <- changeReturn $ last args
            let args' = init args ++ [la']
            return $ mkCoreApps f args'
        else do
            let ty = exprType e'
            let Just tyCon'  = tyConAppTyCon_maybe ty
            let ty' = mkTyConTy tyCon'
            let Just [ty''] = tyConAppArgs_maybe ty 

            repId <- thNameToId 'System.Hardware.Haskino.rep_
            exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
            repDict <- buildDictionaryTyConT exprBTyCon ty''

            exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
            let exprTyConApp = mkTyConApp exprTyCon [ty'']

            fmapId <- thNameToId '(<$>)
            functTyCon <- thNameToTyCon ''Data.Functor.Functor
            functDict <- buildDictionaryTyConT functTyCon ty'

            let repApp = mkCoreApps (Var repId) [Type ty'', repDict]
            let repExpr = mkCoreApps (Var fmapId) [Type ty', Type ty'', Type exprTyConApp, 
                                                              functDict, repApp, e']

            return $ mkLams bs repExpr
      _ -> return e
