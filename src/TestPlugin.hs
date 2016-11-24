{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TestPlugin (plugin) where

import CoreMonad
import GhcPlugins
import HscTypes
import Outputable
import SimplEnv
import SimplUtils
import Data.Data
import Data.Typeable
import IOEnv 
import OccName
import TysPrim
import Unique
import Var
import Control.Monad
import Control.Monad.Writer
import Data.List 

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  dflags <- getDynFlags
  let repLambdaToDo = [CoreDoPluginPass "RepLambda" repLambdaPass]
  return $ [rules0Pass] ++ repLambdaToDo ++ [CoreDoFloatInwards] ++ todo -- [rules1Pass] ++ [rules1Pass] ++ todo

rules0Pass :: CoreToDo
rules0Pass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 0,
            sm_rules = True,
            sm_inline = True,
            sm_case_case = False,
            sm_eta_expand = False
            }

rules1Pass :: CoreToDo
rules1Pass = CoreDoSimplify 4 SimplMode {
            sm_names = [],
            sm_phase = Phase 1,
            sm_rules = False,
            sm_inline = True,
            sm_case_case = False,
            sm_eta_expand = False
            }

repLambdaPass :: ModGuts -> CoreM ModGuts
repLambdaPass guts = do
      dflags <- getDynFlags
      bindsOnlyPass (mapM (repBind dflags)) guts

repBind :: DynFlags -> CoreBind -> CoreM CoreBind
repBind dflags bndr@(NonRec b e) = do
  e' <- repExpr dflags e
  return (NonRec b e')
repBind dflags (Rec bs) = do
  bs' <- repBind' dflags bs
  return $ Rec bs'

repBind' :: DynFlags -> [(Id, CoreExpr)] -> CoreM [(Id, CoreExpr)]
repBind' dflags [] = return []
repBind' dflags ((b, e) : bs) = do
  e' <- repExpr dflags e
  bs' <- repBind' dflags bs
  return $ (b, e') : bs'

repExpr :: DynFlags -> CoreExpr -> CoreM CoreExpr
repExpr dflags e = 
    case e of
      Var v -> return $ Var v
      Lit l -> return $ Lit l
      Type ty -> return $ Type ty
      Coercion co -> return $ Coercion co
      App (App (App (App (App (Var f) (Type t1)) (Type t2)) (Type t3)) (Lam b bd)) (App (Var f2) (Type t4)) | 
        varString f == "." && varString f2 == "rep_" -> do
        bd' <- repExpr dflags bd
        putMsgS "5 Function:"
        putMsg $ ppr f
        putMsg $ ppr t1
        putMsg $ ppr t2
        putMsg $ ppr t3
        putMsg $ ppr b
        putMsg $ ppr bd
        putMsg $ ppr f2
        putMsg $ ppr t4
        -- let newb = buildId ((varString b) ++ "_rec") t3
        let newb = buildId ((varString b) ++ "_rec") t3
        let newe = Lam newb (Let (NonRec b (App (App (Var f2) (Type t4)) (Var newb))) bd')
        putMsgS "New Expr:"
        putMsg $ ppr newe
        return newe
        -- return $ App (App (App (App (App (Var f) (Type t1)) (Type t2)) (Type t3)) (Lam b bd')) (App (Var f2) (Type t4)) 
      App e1 e2 -> do
        e1' <- repExpr dflags e1
        e2' <- repExpr dflags e2
        return $ App e1' e2'
      Lam tb e -> do
        e' <- repExpr dflags e
        return $ Lam tb e'
      Let bind body -> do
        body' <- repExpr dflags body
        bind' <- case bind of 
                    (NonRec v e) -> do
                      e' <- repExpr dflags e
                      return $ NonRec v e'
                    (Rec rbs) -> do
                      rbs' <- repBind' dflags rbs
                      return $ Rec rbs
        return $ Let bind' body' 
      Case e tb ty alts -> do
        e' <- repExpr dflags e
        alts' <- procRepAlts dflags alts
        return $ Case e' tb ty alts'
      Tick t e -> do
        e' <- repExpr dflags e
        return $ Tick t e'
      Cast e co -> do
        e' <- repExpr dflags e
        return $ Cast e' co

varString :: Id -> String 
varString = occNameString . nameOccName . Var.varName

buildId :: String -> Type -> Id
buildId varName typ = mkLocalVar VanillaId name typ vanillaIdInfo
  where
    name           = mkInternalName dunique (mkOccName OccName.varName varName) noSrcSpan
    dunique        = mkUnique '_' 0

procRepAlts :: DynFlags -> [GhcPlugins.Alt CoreBndr] -> CoreM [GhcPlugins.Alt CoreBndr]
procRepAlts dflags [] = return []
procRepAlts dflags ((ac, b, a) : as) = do
  a' <- repExpr dflags a
  bs' <- procRepAlts dflags as
  return $ (ac, b, a') : bs'
