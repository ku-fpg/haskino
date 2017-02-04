-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangePass (bindChangePass) where

import CoreMonad
import GhcPlugins
import OccName
import Var

bindChangePass :: ModGuts -> CoreM ModGuts
bindChangePass guts = bindsOnlyPass (mapM changeBind) guts

changeBind :: CoreBind -> CoreM CoreBind
changeBind bndr@(NonRec b e) = do
  putMsg $ ppr b
  putMsg $ ppr $ exprType e
  return (NonRec b e)
changeBind (Rec bs) = do
  return $ Rec bs
{-
changeBind' :: [(Id, CoreExpr)] -> CoreM [(Id, CoreExpr)]
changeBind' [] = return []
changeBind' ((b, e) : bs) = do
  e' <- changeExpr e
  bs' <- changeBind' bs
  return $ (b, e') : bs'

changeExpr :: CoreExpr -> CoreM CoreExpr
changeExpr e = 
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (App (App (Var f) (Type _)) (Type _)) (Type t3)) (Lam b bd)) (App (Var f2) (Type t4)) | 
      varString f == "." && varString f2 == "abs_" -> do
      bd' <- absExpr bd
      newb <- buildId ((varString b) ++ "_abs") t3
      bd'' <- subVarExpr b (App (App (Var f2) (Type t4)) (Var newb)) bd'
      return $ Lam newb bd''
      -- The below was the initial attempt to insert a let inside the lambda
      -- The thought was the the simplifier would do the substitution from the
      -- let to the variable occurance.  However, this only happened in the
      -- inner bind, and did not work across the inner lambda for the outer
      -- bind.  Therefore, the above was used instead, to do the substitution
      -- as part of the pass.
      --
      -- let newe = Lam newb (Let (NonRec b (App (App (Var f2) (Type t4)) (Var newb))) bd')
      -- return newe
    App e1 e2 -> do
      e1' <- absExpr e1
      e2' <- absExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- absExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- absExpr body
      bind' <- case bind of 
                  (NonRec v e) -> do
                    e' <- absExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- absBind' rbs
                    return $ Rec rbs'
      return $ Let bind' body' 
    Case e tb ty alts -> do
      e' <- absExpr e
      alts' <- procAbsAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- absExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- absExpr e
      return $ Cast e' co

varString :: Id -> String 
varString = occNameString . nameOccName . Var.varName

nameString :: Name -> String 
nameString = occNameString . nameOccName

buildId :: String -> Type -> CoreM Id
buildId varName typ = do
  dunique <- getUniqueM
  let name = mkInternalName dunique (mkOccName OccName.varName varName) noSrcSpan
  return $ mkLocalVar VanillaId name typ vanillaIdInfo

procAbsAlts :: [GhcPlugins.Alt CoreBndr] -> CoreM [GhcPlugins.Alt CoreBndr]
procAbsAlts [] = return []
procAbsAlts ((ac, b, a) : as) = do
  a' <- absExpr a
  bs' <- procAbsAlts as
  return $ (ac, b, a') : bs'

subVarExpr :: Id -> CoreExpr -> CoreExpr -> CoreM CoreExpr
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

subVarExpr' :: Id -> CoreExpr -> [(Id, CoreExpr)] -> CoreM [(Id, CoreExpr)]
subVarExpr' _ _ [] = return []
subVarExpr' id esub ((b, e) : bs) = do
  e' <- subVarExpr id esub e
  bs' <- subVarExpr' id esub bs
  return $ (b, e') : bs'

subVarExprAlts :: Id -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> CoreM [GhcPlugins.Alt CoreBndr]
subVarExprAlts _ _ [] = return []
subVarExprAlts id esub ((ac, b, a) : as) = do
  a' <- subVarExpr id esub a
  bs' <- subVarExprAlts id esub as
  return $ (ac, b, a') : bs'
-}
