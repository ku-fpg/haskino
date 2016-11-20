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
import Control.Monad
import Data.List 

data SomeAnn = SomeAnn deriving (Data, Typeable)

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
  putMsgS $ "Currently Compiler has " ++ (show $ length todo) ++ " passes."
  -- return (todo ++ [(CoreDoPluginPass "Say name" pass)])
  return $ ((CoreDoPluginPass "Rules" rulePass) : todo) ++ [CoreDoPluginPass "Rules" rulePass] ++ todo ++ [CoreDoPluginPass "Rules" rulePass]

rulePass :: ModGuts -> CoreM ModGuts
rulePass guts = do
      dflags <- getDynFlags
      rb <- getRuleBase
      -- hscEnv  <- getHscEnv
      -- rb' <- liftM eps_rule_base $ liftIO $ runIOEnv () $ readMutVar (hsc_EPS hscEnv)
      let rules = (mg_rules guts) ++ concat (nameEnvElts rb) -- ++ concat (nameEnvElts rb')
      putMsgS "Rules:"
      putMsg $ pprRulesForUser rules
      let ruleBse = mkRuleBase rules
          ruleEnv = mkRuleEnv rb [(mg_module guts)]
          vars = getVars (mg_binds guts)
          inScopeSet = extendInScopeSetList emptyInScopeSet vars
          env = setInScopeSet (mkSimplEnv ourMode) inScopeSet
      bindsOnlyPass (mapM (procBind dflags env rules)) guts
  where procBind :: DynFlags -> SimplEnv -> [CoreRule] -> CoreBind -> CoreM CoreBind
        procBind dflags env rules bndr@(NonRec b e) = do
          putMsgS $ "Non-Recursive Bind " ++ (showSDoc dflags (ppr bndr))
          let env' = addNewInScopeIds env [b]
          e' <- tryRules dflags env' rules e
          putMsgS $ "New expr " ++ (showSDoc dflags (ppr e'))
          return (NonRec b e')
        procBind dflags env rules bndr@(Rec bs) = do
          putMsgS $ "Recursive Bind " ++ (showSDoc dflags (ppr bndr))
          bs' <- procBind' dflags env rules bs
          putMsgS $ "New bind " ++ (showSDoc dflags (ppr bs'))
          return $ Rec bs'

        procBind' :: DynFlags -> SimplEnv -> [CoreRule] -> [(Id, CoreExpr)] -> CoreM [(Id, CoreExpr)]
        procBind' dflags env rules [] = return []
        procBind' dflags env rules ((b, e) : bs) = do
          let env' = addNewInScopeIds env [b]
          e' <- tryRules dflags env' rules e
          bs' <- procBind' dflags env' rules bs
          return $ (b, e') : bs'

        tryRules :: DynFlags -> SimplEnv -> [CoreRule] -> CoreExpr -> CoreM CoreExpr
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
                      let env' = addNewInScopeIds env [fn]
                          m = lookupRule dflags (getUnfoldingInRuleMatch env') (\x -> True) fn args rules 
                      in case m of
                         Nothing -> do
                           tryInsideArgs dflags env rules fn (reverse args)
                         Just (r, e') -> do
                           putMsgS $ "Applied Rule: "
                           putMsg $ pprRulesForUser [r]
                           -- Try other rules after this one worked
                           tryRules dflags env (removeRule r rules) e'
              Lam tb e -> do
                let env' = addNewInScopeIds env [tb]
                e' <- tryRules dflags env' rules e
                return $ Lam tb e'
              Let bind body -> do
                body' <- tryRules dflags env rules body
                bind' <- case bind of 
                            (NonRec v e) -> do
                              e' <- tryRules dflags (addNewInScopeIds env [v]) rules e
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

        procAlts :: DynFlags -> SimplEnv -> [CoreRule] -> [Alt CoreBndr] -> CoreM [Alt CoreBndr]
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
        tryInsideArgs :: DynFlags -> SimplEnv -> [CoreRule] -> Id -> [CoreExpr] -> CoreM CoreExpr
        tryInsideArgs dflags env rules fn [a] = do
            a' <- tryRules dflags env rules a
            return $ App (Var fn) a'
        tryInsideArgs dflags env rules fn (a : as) = do
            a' <- tryRules dflags env rules a
            e <- tryInsideArgs dflags env rules fn as
            return $ App e a'


removeRule :: CoreRule -> [CoreRule] -> [CoreRule]
removeRule rd [] = []
removeRule rd (r:rs) = if ruleName rd == ruleName r
                       then removeRule rd rs
                       else r : removeRule rd rs
