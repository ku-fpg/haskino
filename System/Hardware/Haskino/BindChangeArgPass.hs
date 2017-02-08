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
import Var
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.Dictionary (buildDictionaryT, PassCoreM(..), )

import qualified System.Hardware.Haskino

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
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeBind) x) (BindEnv guts []))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ varType b
  let (bs, e') = collectBinders e
  let tyCon_m = splitTyConApp_maybe retTy
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") -> do
          zipBsArgTys <- mapM changeArg (zip bs argTys)
          let (bs', argTys') = unzip zipBsArgTys
          s <- get
          put s{args = bs'}
          e'' <- changeArgAppsExpr e'
          let b' = setVarType b $ mkFunTys argTys' retTy
          let e''' = mkLams bs' e''
          return (NonRec b' e''')
      _ -> return bndr
changeBind (Rec bs) = do
  return $ Rec bs

changeArg :: (CoreBndr, Type) -> BindM (CoreBndr, Type)
changeArg (b, ty) = do
  let tyCon_m = splitTyConApp_maybe ty
  case tyCon_m of
    Just (_, []) -> do
        -- ToDo:  Check that type is a valid Haskino Expr Type?
        -- Lookup the GHC type constructor of Expr
        Just exprName <- liftCoreM $ thNameToGhcName ''System.Hardware.Haskino.Expr
        exprTyCon <- liftCoreM $ lookupTyCon exprName
        -- Make the type of the Expr for the specified type
        let ty' = mkTyConApp exprTyCon [ty]
        let b' = setVarType b ty'
        return (b', ty')
    _       -> return (b, ty)

changeArgAppsExpr :: CoreExpr -> BindM CoreExpr
changeArgAppsExpr e = do
  df <- liftCoreM getDynFlags
  s <- get
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (Var f) (Type _)) (Type _)) (Var v)  |
      varString f == "rep_" && v `elem` (args s) -> do
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

varString :: Id -> String
varString = occNameString . nameOccName . Var.varName

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

