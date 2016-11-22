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
import Var
import Control.Monad
import Control.Monad.Writer
import Data.List 

ourMode :: SimplifierMode
ourMode = SimplMode {
            sm_names = [],
            sm_phase = Phase 0,
            sm_rules = True,
            sm_inline = True,
            sm_case_case = True,
            sm_eta_expand = True
            }

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  -- putMsgS $ "Currently Compiler has " ++ (show $ length todo) ++ " passes."
  dflags <- getDynFlags
  let ruleToDo = [CoreDoPluginPass "Rules" rulePass]
  let ruleSimplifierToDo = ruleToDo ++ [simplifierPass]
  let repLambdaToDo = [CoreDoPluginPass "RepLambda" repLambdaPass]
  return $ ruleSimplifierToDo ++ ruleSimplifierToDo ++ repLambdaToDo ++ todo

simplifierPass :: CoreToDo
simplifierPass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 0,
            sm_rules = False,
            sm_inline = False,
            sm_case_case = False,
            sm_eta_expand = False
            }

rulePass :: ModGuts -> CoreM ModGuts
rulePass guts = do
      dflags <- getDynFlags
      rb <- getRuleBase
      -- hscEnv  <- getHscEnv
      -- rb' <- liftM eps_rule_base $ liftIO $ runIOEnv () $ readMutVar (hsc_EPS hscEnv)
      let rules = (mg_rules guts) ++ concat (nameEnvElts rb) -- ++ concat (nameEnvElts rb')
      -- putMsgS "Rules:"
      -- putMsg $ pprRulesForUser rules
      let ruleBse = mkRuleBase rules
          ruleEnv = mkRuleEnv rb [(mg_module guts)]
          vars = getVars (mg_binds guts)
          inScopeSet = extendInScopeSetList emptyInScopeSet vars
          env = setInScopeSet (mkSimplEnv ourMode) inScopeSet
      ruleSubPass dflags env rules guts
      -- return guts {mg_binds = mgbs}

ruleSubPass :: DynFlags -> SimplEnv -> [CoreRule] -> ModGuts -> CoreM ModGuts
ruleSubPass dflags env rules guts = do
  (mgbs, rs) <- runWriterT $ mapM (procBind dflags env rules) (mg_binds guts)
  case rs of
    []  -> return guts {mg_binds = mgbs}
    rss -> do
        putMsgS $ "Rules Applied: " ++ intercalate ", " (map (unpackFS . ruleName) rs)
        return guts {mg_binds = mgbs}
        --guts' <- doCorePass simplifierPass (guts {mg_binds = mgbs})
        -- ruleSubPass dflags env rules guts {mg_binds = mgbs}
        -- return mgbs

procBind :: DynFlags -> SimplEnv -> [CoreRule] -> CoreBind -> WriterT [CoreRule] CoreM CoreBind
procBind dflags env rules bndr@(NonRec b e) = do
  --lift $ putMsgS $ "Non-Recursive Bind " ++ (showSDoc dflags (ppr bndr))
  e' <- tryRules dflags env rules e
  -- lift $ putMsgS $ "New expr " ++ (showSDoc dflags (ppr e'))
  return (NonRec b e')
procBind dflags env rules bndr@(Rec bs) = do
  --lift $ putMsgS $ "Recursive Bind " ++ (showSDoc dflags (ppr bndr))
  bs' <- procBind' dflags env rules bs
  --lift $ putMsgS $ "New bind " ++ (showSDoc dflags (ppr bs'))
  return $ Rec bs'

procBind' :: DynFlags -> SimplEnv -> [CoreRule] -> [(Id, CoreExpr)] -> WriterT [CoreRule] CoreM [(Id, CoreExpr)]
procBind' dflags env rules [] = return []
procBind' dflags env rules ((b, e) : bs) = do
  e' <- tryRules dflags env rules e
  bs' <- procBind' dflags env rules bs
  return $ (b, e') : bs'

tryRules :: DynFlags -> SimplEnv -> [CoreRule] -> CoreExpr -> WriterT [CoreRule] CoreM CoreExpr
tryRules dflags env rules e = do
    case e of
      Var v -> return $ Var v
      Lit l -> return $ Lit l
      Type ty -> return $ Type ty
      Coercion co -> return $ Coercion co
      App e1 e2 ->
        let b = getArgs e1 e2 []
        in case b of
            Nothing -> do 
              e1' <- tryRules dflags env rules e1
              e2' <- tryRules dflags env rules e2
              return $ App e1' e2'
            Just (fn, args) ->
              let m = lookupRule dflags (getUnfoldingInRuleMatch env) (\x -> True) fn args rules 
              in case m of
                 Nothing -> do
                   tryInsideArgs dflags env rules fn (reverse args)
                 Just (r, e') -> do
                   --lift $ putMsgS $ "Applied Rule: "
                   --lift $ putMsg $ pprRulesForUser [r]
                   tell [r]
                   -- Try other rules after this one worked
                   -- tryRules dflags env (removeRule r rules) e'
                   case e' of 
                      App (Var v) be -> do
                        be' <- tryRules dflags env rules e'
                        return $ App (Var v) be'
                      e''       -> tryRules dflags env rules e''
                   -- tryRules dflags env rules e'
      Lam tb e -> do
        e' <- tryRules dflags env rules e
        return $ Lam tb e'
      Let bind body -> do
        body' <- tryRules dflags env rules body
        bind' <- case bind of 
                    (NonRec v e) -> do
                      e' <- tryRules dflags env rules e
                      return $ NonRec v e'
                    (Rec rbs) -> do
                      rbs' <- procBind' dflags env rules rbs
                      return $ Rec rbs
        return $ Let bind' body' 
      Case e tb ty alts -> do
        e' <- tryRules dflags env rules e
        alts' <- procAlts dflags env rules alts
        return $ Case e' tb ty alts'
      Tick t e -> do
        e' <- tryRules dflags env rules e
        return $ Tick t e'
      Cast e co -> do
        e' <- tryRules dflags env rules e
        return $ Cast e' co

procAlts :: DynFlags -> SimplEnv -> [CoreRule] -> [GhcPlugins.Alt CoreBndr] -> WriterT [CoreRule] CoreM [GhcPlugins.Alt CoreBndr]
procAlts dflags env rules [] = return []
procAlts dflags env rules ((ac, b, a) : as) = do
  a' <- tryRules dflags env rules a
  bs' <- procAlts dflags env rules as
  return $ (ac, b, a') : bs'

getVars :: [CoreBind] -> [Var]
getVars [] = []
getVars (b:bs) = case b of 
                   (NonRec v _) -> v : getVars bs
                   (Rec rbs) -> (getrVars rbs) ++ (getVars bs)
  where
    getrVars :: [(Var, CoreExpr)] -> [Var]
    getrVars [] = []
    getrVars ((v,e) : bs) = v : getrVars bs

getArgs :: CoreExpr -> CoreExpr -> [CoreExpr] -> Maybe (Id, [CoreExpr])
getArgs e1 e2 args =
   case e1 of
      App e3 e4 -> getArgs e3 e4 (e2 : args)
      Var v -> Just (v, e2 : args)
      e -> Nothing

-- Note, args are passed in a reverse order list for efficiency
tryInsideArgs :: DynFlags -> SimplEnv -> [CoreRule] -> Id -> [CoreExpr] -> WriterT [CoreRule] CoreM CoreExpr
tryInsideArgs dflags env rules fn [a] = do
    a' <- tryRules dflags env rules a
    return $ App (Var fn) a'
tryInsideArgs dflags env rules fn (a : as) = do
    a' <- tryRules dflags env rules a
    e <- tryInsideArgs dflags env rules fn as
    return $ App e a'

removeRules :: [CoreRule] -> [CoreRule] -> [CoreRule]
removeRules [] rs = rs
removeRules (rr : rrs) rs = removeRules rrs (removeRule rr rs) 

removeRule :: CoreRule -> [CoreRule] -> [CoreRule]
removeRule rd [] = []
removeRule rd (r:rs) = if ruleName rd == ruleName r
                       then removeRule rd rs
                       else r : removeRule rd rs

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
        return $ App (App (App (App (App (Var f) (Type t1)) (Type t2)) (Type t3)) (Lam b bd')) (App (Var f2) (Type t4)) 
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
varString = occNameString . nameOccName . varName

procRepAlts :: DynFlags -> [GhcPlugins.Alt CoreBndr] -> CoreM [GhcPlugins.Alt CoreBndr]
procRepAlts dflags [] = return []
procRepAlts dflags ((ac, b, a) : as) = do
  a' <- repExpr dflags a
  bs' <- procRepAlts dflags as
  return $ (ac, b, a') : bs'
