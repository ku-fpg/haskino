-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.LambdaPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass 2
-- ifThenElseE (rep b) t e => ifThenElseE (rep b) (rep <$> t) (rep <$> e)
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.LambdaPass (lambdaPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader
import OccName
import Var

import System.Hardware.Haskino.Dictionary (buildDictionaryT, 
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

lambdaPass :: ModGuts -> CoreM ModGuts
lambdaPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeLambda) x) (BindEnv guts))) guts

changeLambda :: CoreBind -> BindM CoreBind
changeLambda bndr@(NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeLambdaExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeLambda (Rec bs) = do
  return $ Rec bs

changeLambdaExpr :: CoreExpr -> BindM CoreExpr
changeLambdaExpr e = do
  df <- liftCoreM getDynFlags
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (App (App (App (Var bind) (Type monadTy)) dict) (Type arg1Ty)) (Type arg2Ty)) e_right) (Lam b e_lam) | varString bind == ">>=" -> do
        (e_right', absFlag) <- checkForAbs e_right
        e_lam' <- changeLambdaExpr e_lam
        if absFlag
        then do
            exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr
            let exprArg1Ty = mkTyConApp exprTyCon [arg1Ty]
            newb <- buildId ((varString b) ++ "_abs") exprArg1Ty
            absId <- thNameToId 'System.Hardware.Haskino.abs_
            e_lam'' <- subVarExpr b (App (App (Var absId) (Type arg1Ty)) (Var newb)) e_lam'
            return $ App (App (App (App (App (App (Var bind) (Type monadTy)) dict) (Type exprArg1Ty)) (Type arg2Ty)) e_right') (Lam newb e_lam'')
        else do
            e_right' <- changeLambdaExpr e_right
            return $ App (App (App (App (App (App (Var bind) (Type monadTy)) dict) (Type arg1Ty)) (Type arg2Ty)) e_right') (Lam b e_lam')
    App e1 e2 -> do
      e1' <- changeLambdaExpr e1
      e2' <- changeLambdaExpr e2
      return $ App e1' e2'       
    Lam tb e -> do
      e' <- changeLambdaExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeLambdaExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeLambdaExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeLambdaExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeLambdaExpr e
      alts' <- changeLambdaExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeLambdaExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeLambdaExpr e
      return $ Cast e' co

varString :: Id -> String 
varString = occNameString . nameOccName . Var.varName

nameString :: Name -> String 
nameString = occNameString . nameOccName

buildId :: String -> Type -> BindM Id
buildId varName typ = do
  dunique <- liftCoreM getUniqueM
  let name = mkInternalName dunique (mkOccName OccName.varName varName) noSrcSpan
  return $ mkLocalVar VanillaId name typ vanillaIdInfo

changeLambdaExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeLambdaExpr' [] = return []
changeLambdaExpr' ((b, e) : bs) = do
  e' <- changeLambdaExpr e
  bs' <- changeLambdaExpr' bs
  return $ (b, e') : bs'

changeLambdaExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeLambdaExprAlts [] = return []
changeLambdaExprAlts ((ac, b, a) : as) = do
  a' <- changeLambdaExpr a
  bs' <- changeLambdaExprAlts as
  return $ (ac, b, a') : bs'

checkForAbs :: CoreExpr -> BindM (CoreExpr, Bool)
checkForAbs e = do
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    if (showSDoc df (ppr f) == ">>=") || (showSDoc df (ppr f) == ">>")
    then do
        (e'', absFlag) <- checkForAbs $ last args
        if absFlag
        then do
            case args of
                [Type ty1, dict, Type ty2, Type ty3, e1, e2] -> do
                    exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr
                    let exprTy3 = mkTyConApp exprTyCon [ty3]
                    let e''' = mkCoreApps f [Type ty1, dict, Type ty2, Type exprTy3, e1, e'']
                    return $ (mkLams bs e''', absFlag)
                _ -> do
                    let e''' = mkCoreApps f ((init args) ++ [e''])        
                    return $ (mkLams bs e''', absFlag)
        else do
            let e''' = mkCoreApps f ((init args) ++ [e''])        
            return $ (mkLams bs e''', absFlag)
    else 
        if (showSDoc df (ppr f) == "<$>")
        then  
            case args of
                [Type ty1, Type ty2, Type ty3, dict, e1, e2] -> do
                    let (g, _) = collectArgs e1
                    if  (showSDoc df (ppr g) == "abs_")
                    then do
                      return (mkLams bs e2, True)
                    else return (e, False)
                _ -> return (e, False)
        else
            return (e, False)

subVarExpr :: Id -> CoreExpr -> CoreExpr -> BindM CoreExpr
subVarExpr id esub e = 
  case e of
    Var v -> do
      if v == id
      then return esub
      else return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- subVarExpr id esub e1
      e2' <- subVarExpr id esub e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- subVarExpr id esub e
      return $ Lam tb e'
    Let bind body -> do
      body' <- subVarExpr id esub body
      bind' <- case bind of 
                  (NonRec v e) -> do
                    e' <- subVarExpr id esub e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- subVarExpr' id esub rbs
                    return $ Rec rbs'
      return $ Let bind' body' 
    Case e tb ty alts -> do
      e' <- subVarExpr id esub e
      alts' <- subVarExprAlts id esub alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- subVarExpr id esub e
      return $ Tick t e'
    Cast e co -> do
      e' <- subVarExpr id esub e
      return $ Cast e' co

subVarExpr' :: Id -> CoreExpr -> [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
subVarExpr' _ _ [] = return []
subVarExpr' id esub ((b, e) : bs) = do
  e' <- subVarExpr id esub e
  bs' <- subVarExpr' id esub bs
  return $ (b, e') : bs'

subVarExprAlts :: Id -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
subVarExprAlts _ _ [] = return []
subVarExprAlts id esub ((ac, b, a) : as) = do
  a' <- subVarExpr id esub a
  bs' <- subVarExprAlts id esub as
  return $ (ac, b, a') : bs'
