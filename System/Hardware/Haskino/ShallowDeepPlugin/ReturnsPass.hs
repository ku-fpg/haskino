-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.ReturnsPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- return express transformation pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.ReturnsPass (returnsPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader
import Var

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

returnsPass :: ModGuts -> CoreM ModGuts
returnsPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeBind) x) (BindEnv guts))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (bs, e') = collectBinders e
  e'' <- changeRetExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeBind bndr@(Rec bs) = return bndr

changeRetExpr :: CoreExpr -> BindM CoreExpr
changeRetExpr e = do
  df <- liftCoreM getDynFlags
  returnId <- thNameToId returnNameTH
  monadTyCon <- thNameToTyCon monadTyConTH
  let monadTy = mkTyConTy monadTyCon
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (b, args) = collectArgs e
      let defaultReturn = do
                  e1' <- changeRetExpr e1
                  e2' <- changeRetExpr e2
                  return $ App e1' e2'
      case b of
        Var bv -> do
          case head args of
            Type ty -> do
              if bv == returnId && ty `eqType` monadTy
              then do
                  e' <- changeReturnTypes e
                  return e'
              else defaultReturn
            _ -> defaultReturn
        _ -> defaultReturn
    Lam tb e -> do
      e' <- changeRetExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeRetExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeRetExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeRetExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeRetExpr e
      alts' <- changeRetExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeRetExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeRetExpr e
      return $ Cast e' co

changeRetExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeRetExpr' [] = return []
changeRetExpr' ((b, e) : bs) = do
  e' <- changeRetExpr e
  bs' <- changeRetExpr' bs
  return $ (b, e') : bs'

changeRetExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeRetExprAlts [] = return []
changeRetExprAlts ((ac, b, a) : as) = do
  a' <- changeRetExpr a
  bs' <- changeRetExprAlts as
  return $ (ac, b, a') : bs'

{-
  The following performs this transform:

    forall (ExprB a => e :: a) 
    return e
      =
    abs_ <$> (return (rep_ e))

-}
changeReturnTypes :: CoreExpr -> BindM CoreExpr
changeReturnTypes e = do
    let (f, args) = collectArgs e
    case args of
      [Type ty1, Var d, Type ty2, ex] -> do
          retArg <- repExpr ex
          exprTyConApp <- thNameTyToTyConApp exprTyConTH ty2
          let f' = mkCoreApps f [Type ty1, Var d, Type exprTyConApp, retArg]
          fmapAbsExpr ty1 ty2 f'
      _ -> return e
