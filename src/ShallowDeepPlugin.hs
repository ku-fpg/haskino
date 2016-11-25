module ShallowDeepPlugin (plugin) where

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
  let repLambdaToDo = [CoreDoPluginPass "RepLambda" repLambdaPass]
  let dumpTodo = [CoreDoPluginPass "DumpPass" dumpPass]
  return $ rules0Pass : repLambdaToDo ++ [rules1Pass] ++ todo ++ dumpTodo

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
rules1Pass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 1,
            sm_rules = True,
            sm_inline = True,
            sm_case_case = False,
            sm_eta_expand = False
            }

repLambdaPass :: ModGuts -> CoreM ModGuts
repLambdaPass guts = bindsOnlyPass (mapM repBind) guts

dumpPass :: ModGuts -> CoreM ModGuts
dumpPass guts = do
  putMsgS "In dumpPass"
  putMsg $ ppr (mg_binds guts)
  return guts      

repBind :: CoreBind -> CoreM CoreBind
repBind bndr@(NonRec b e) = do
  e' <- repExpr e
  return (NonRec b e')
repBind (Rec bs) = do
  bs' <- repBind' bs
  return $ Rec bs'

repBind' :: [(Id, CoreExpr)] -> CoreM [(Id, CoreExpr)]
repBind' [] = return []
repBind' ((b, e) : bs) = do
  e' <- repExpr e
  bs' <- repBind' bs
  return $ (b, e') : bs'

repExpr :: CoreExpr -> CoreM CoreExpr
repExpr e = 
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App (App (App (App (App (Var f) (Type _)) (Type _)) (Type t3)) (Lam b bd)) (App (Var f2) (Type t4)) | 
      varString f == "." && varString f2 == "rep_" -> do
      bd' <- repExpr bd
      newb <- buildId ((varString b) ++ "_rep") t3
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
      e1' <- repExpr e1
      e2' <- repExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- repExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- repExpr body
      bind' <- case bind of 
                  (NonRec v e) -> do
                    e' <- repExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- repBind' rbs
                    return $ Rec rbs'
      return $ Let bind' body' 
    Case e tb ty alts -> do
      e' <- repExpr e
      alts' <- procRepAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- repExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- repExpr e
      return $ Cast e' co

varString :: Id -> String 
varString = occNameString . nameOccName . Var.varName

buildId :: String -> Type -> CoreM Id
buildId varName typ = do
  dunique <- getUniqueM
  let name = mkInternalName dunique (mkOccName OccName.varName varName) noSrcSpan
  return $ mkLocalVar VanillaId name typ vanillaIdInfo

procRepAlts :: [GhcPlugins.Alt CoreBndr] -> CoreM [GhcPlugins.Alt CoreBndr]
procRepAlts [] = return []
procRepAlts ((ac, b, a) : as) = do
  a' <- repExpr a
  bs' <- procRepAlts as
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
