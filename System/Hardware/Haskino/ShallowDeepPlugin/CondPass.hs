-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.CondPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-- Performs the equivelent of the following rules:
--
-- forall (b :: Bool) (t :: ExprB a => Arduino a) (f :: Expr a => Arduino a)
-- if b then t else e 
--    =
-- abs_ <$> ifThenElseE (rep_ b) (rep_ <$> t) (rep_ <$> e)
--
-- And
--
-- forall (b :: Bool) (t :: ExprB a => a) (f :: ExprB a => a)
-- if b then t else e 
--    =
-- abs_ (ifBE (rep_ b) (rep_ t) (rep_ e))
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.CondPass (condPass) where

import CoreMonad
import Control.Monad.Reader
import Data.Functor
import Data.List
import GhcPlugins

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

data CondEnv
    = CondEnv
      { pluginModGuts :: ModGuts
      }

newtype CondM a = CondM { runCondM :: ReaderT CondEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader CondEnv)

instance PassCoreM CondM where
  liftCoreM = CondM . ReaderT . const
  getModGuts = CondM $ ReaderT (return . pluginModGuts)

condPass :: ModGuts -> CoreM ModGuts
condPass guts = do
    bindsOnlyPass (\x -> (runReaderT (runCondM $ (mapM condBind) x) (CondEnv guts))) guts

condBind :: CoreBind -> CondM CoreBind
condBind bndr@(NonRec b e) = do
  e' <- condExpr e
  return (NonRec b e')
condBind (Rec bs) = do
  bs' <- condBind' bs
  return $ Rec bs'

condBind' :: [(Id, CoreExpr)] -> CondM [(Id, CoreExpr)]
condBind' [] = return []
condBind' ((b, e) : bs) = do
  e' <- condExpr e
  bs' <- condBind' bs
  return $ (b, e') : bs'

condExpr :: CoreExpr -> CondM CoreExpr
condExpr e = do
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- condExpr e1
      e2' <- condExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- condExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- condExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- condExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- condExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      let tyCon_m = splitTyConApp_maybe ty
      e' <- condExpr e
      alts' <- condExprAlts alts
      let defaultReturn = return $ Case e' tb ty alts'
      monadTyCon <- thNameToTyCon monadTyConTH
      case tyCon_m of
        Just (retTyCon, [retTy']) | retTyCon == monadTyCon -> do
            if length alts' == 2
            then case alts' of
              [(ac1, _, _), _] -> do
                case ac1 of
                  DataAlt d -> do
                    Just falseName <- liftCoreM $ thNameToGhcName falseNameTH
                    if (getName d) == falseName
                    then condTransform ty e' alts'
                    else defaultReturn
                  _ -> defaultReturn
            else defaultReturn
        _ -> do
          isExpr <- isExprClassType ty
          if isExpr && length alts' == 2
          then case alts' of
            [(ac1, _, _), _] -> do
              case ac1 of
                DataAlt d -> do
                  Just falseName <- liftCoreM $ thNameToGhcName falseNameTH
                  if (getName d) == falseName
                  then condExprTransform ty e' alts'
                  else defaultReturn
                _ -> defaultReturn
          else defaultReturn
    Tick t e -> do
      e' <- condExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- condExpr e
      return $ Cast e' co

condExpr' :: [(Id, CoreExpr)] -> CondM [(Id, CoreExpr)]
condExpr' [] = return []
condExpr' ((b, e) : bs) = do
  e' <- condExpr e
  bs' <- condExpr' bs
  return $ (b, e') : bs'

condExprAlts :: [GhcPlugins.Alt CoreBndr] -> CondM [GhcPlugins.Alt CoreBndr]
condExprAlts [] = return []
condExprAlts ((ac, b, a) : as) = do
  a' <- condExpr a
  bs' <- condExprAlts as
  return $ (ac, b, a') : bs'

{-
  The following performs this transform:

    forall (b :: Bool) (t :: ArduinoConditional a => Arduino a) (e :: ArduinoConditional a => Arduino a).
    if b then t else e
      =
    abs_ <$> ifThenElseE (rep_ b) (rep_ t) (rep_ e)

-}
condTransform :: Type -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> CondM CoreExpr
condTransform ty e alts = do
  case alts of
    [(_, _, e1),(_, _, e2)] -> do
      let [ty'] = tyConAppArgs ty

      ifThenElseId <- thNameToId ifThenElseNameTH
      condDict <- thNameTyToDict monadCondTyConTH ty'

      -- Build the args to ifThenElseE
      arg1 <- repExpr e
      e1' <- fmapRepBindReturn e1
      e2' <- fmapRepBindReturn e2

      -- Build the ifThenElse Expr
      let ifteExpr = mkCoreApps (Var ifThenElseId) [Type ty', condDict, arg1, e2', e1']

      -- Apply fmap of abs_
      tyCon <- thNameToTyCon monadTyConTH
      fmapAbsExpr (mkTyConTy tyCon) ty' ifteExpr

condExprTransform :: Type -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> CondM CoreExpr
condExprTransform ty e alts = do
  case alts of
    [(_, _, e1),(_, _, e2)] -> do
      ifbId <- thNameToId ifBNameTH
      ifbDict <- thNameTyToDict exprClassTyConTH ty

      -- Build the ifBE expression
      arg1 <- repExpr e
      arg2 <- repExpr e2
      arg3 <- repExpr e1
      absExpr $ mkCoreApps (Var ifbId) [Type ty, ifbDict, arg1, arg2, arg3]
