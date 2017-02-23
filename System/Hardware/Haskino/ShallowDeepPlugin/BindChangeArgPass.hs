-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind argument type change pass
-- f :: a -> ... -> c -> Expr d ==> f :: Expr a  -> ... -> Expr c -> Expr d
-- It does this by changing the type of the argument, and then replacing
-- each occurnace of (rep_ a) of the argument 'a' with just the type 
-- changed argument itself.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass (bindChangeArgPass) where

import CoreMonad
import GhcPlugins
import Var
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        args :: [CoreBndr]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
  liftCoreM m = BindM $ lift m
  getModGuts = gets pluginModGuts

bindChangeArgPass :: ModGuts -> CoreM ModGuts
bindChangeArgPass guts =
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeArgBind) x) (BindEnv guts []))) guts

changeArgBind :: CoreBind -> BindM CoreBind
changeArgBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ varType b
  let (bs, e') = collectBinders e
  let tyCon_m = splitTyConApp_maybe retTy
  monadTyConId <- thNameToTyCon monadTyConTH
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
          zipBsArgTys <- mapM changeArg (zip bs argTys)
          let (bs', argTys') = unzip zipBsArgTys
          s <- get
          put s{args = bs'}
          e'' <- changeArgAppsExpr e'
          let b' = setVarType b $ mkFunTys argTys' retTy
          let e''' = mkLams bs' e''
          return (NonRec b' e''')
      _ -> return bndr
changeArgBind (Rec bs) = do
  return $ Rec bs

changeArg :: (CoreBndr, Type) -> BindM (CoreBndr, Type)
changeArg (b, ty) = do
  let tyCon_m = splitTyConApp_maybe ty
  case tyCon_m of
    Just (_, []) -> do
        -- ToDo:  Check that type is a valid Haskino Expr Type?
        -- Lookup the GHC type constructor of Expr
        exprTyCon <- thNameToTyCon exprTyConTH
        -- Make the type of the Expr for the specified type
        let ty' = mkTyConApp exprTyCon [ty]
        let b' = setVarType b ty'
        return (b', ty')
    _       -> return (b, ty)

changeArgAppsExpr :: CoreExpr -> BindM CoreExpr
changeArgAppsExpr e = do
  df <- liftCoreM getDynFlags
  s <- get
  repId <- thNameToId repNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (Var f) (Type _)) (_)) (Var v)  | f == repId && v `elem` (args s) -> do
        return $ (Var v)
    App e1 e2 -> do
      e1' <- changeArgAppsExpr e1
      e2' <- changeArgAppsExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- changeArgAppsExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeArgAppsExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeArgAppsExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeArgAppsExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeArgAppsExpr e
      alts' <- changeArgAppsExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeArgAppsExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeArgAppsExpr e
      return $ Cast e' co

changeArgAppsExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeArgAppsExpr' [] = return []
changeArgAppsExpr' ((b, e) : bs) = do
  e' <- changeArgAppsExpr e
  bs' <- changeArgAppsExpr' bs
  return $ (b, e') : bs'

changeArgAppsExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeArgAppsExprAlts [] = return []
changeArgAppsExprAlts ((ac, b, a) : as) = do
  a' <- changeArgAppsExpr a
  bs' <- changeArgAppsExprAlts as
  return $ (ac, b, a') : bs'

