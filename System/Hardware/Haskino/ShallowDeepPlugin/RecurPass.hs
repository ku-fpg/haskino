-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RecurPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Recursion Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RecurPass (recurPass) where

import CoreMonad
import GhcPlugins
import Data.List
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        funcId        :: [Id],
        conds         :: [CoreExpr],
        dicts         :: [CoreExpr]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
    liftCoreM m = BindM $ lift m
    getModGuts = gets pluginModGuts


recurPass :: ModGuts -> CoreM ModGuts
recurPass guts = do
    bindsL' <- (\x -> fst <$> (runStateT (runBindM $ (mapM recurBind) x) (BindEnv guts [] [] []))) (mg_binds guts)
    return (guts { mg_binds = concat bindsL' })

recurBind :: CoreBind -> BindM [CoreBind]
recurBind bndr@(NonRec b e) = return [bndr]
recurBind (Rec bs) = do
    (nonRec, bs') <- recurBind' bs
    return $ nonRec ++ [Rec bs']

recurBind' :: [(Id, CoreExpr)] -> BindM ([CoreBind],[(Id, CoreExpr)])
recurBind' [] = return ([],[])
recurBind' ((b, e) : bs) = do
    let defaultRet = do
        (nonrec, bs') <- recurBind' bs
        return $ (nonrec, (b, e) : bs')
    s <- get
    put s {funcId = [b], conds = []}
    let (argTys, retTy) = splitFunTys $ exprType e
    let tyCon_m = splitTyConApp_maybe retTy
    monadTyCon <- thNameToTyCon monadTyConTH
    case tyCon_m of
        Just (tyCon, [tyArg]) -> do
            if length argTys == 1 && tyCon == monadTyCon
            then do
                s <- get
                put s {funcId = [b]}
{-
                liftCoreM $ putMsgS "One Arg Arudino Rec Bind"
                liftCoreM $ putMsg $ ppr b
                liftCoreM $ putMsg $ ppr argTys
                liftCoreM $ putMsg $ ppr retTy
-}

                let argTy = head argTys

                let (lbs, e') = collectBinders e

                let arg = head lbs

                -- Build the step body of the While
                (e'', hasAp)  <- checkForRecur True e'
                newStepB <- buildId ("x") argTy
                stepE <- changeVarExpr arg newStepB e''
                let stepLam = mkLams [newStepB] stepE

                -- Build the done body which executes after the while
                (e''', hasAp) <- checkForRecur False e'
                newDoneB <- buildId ("x'") argTy
                doneE <- changeVarExpr arg newDoneB e'''
                let doneLam = mkLams [newDoneB] doneE

                -- Build the conditional for the while
                conds <- gets conds
                cond <- genWhileCond conds
                newCondB <- buildId ("x") argTy
                cond' <- changeVarExpr arg newCondB cond
                -- condDeep <- repExpr cond'
                let condLam = mkLams [newCondB] cond'

                -- Build the initialization argument
                -- initArg <- repExpr (Var arg)

                -- Build the while expression
                whileId <- thNameToId whileTH
                condDict <- thNameTyToDict monadCondTyConTH argTy
                let whileExpr = mkCoreApps (Var whileId) [Type argTy, condDict, Var arg, condLam, stepLam]

                -- Build the While >>= Done expression
                bindId <- thNameToId bindNameTH
                dicts <- gets dicts
                monadTyConId <- thNameToTyCon monadTyConTH
                let monadTyConTy = mkTyConTy monadTyConId
                let bindExpr = mkCoreApps (Var bindId) [Type monadTyConTy, head dicts, Type argTy, Type tyArg, whileExpr, doneLam]

                -- Create the transformed non-recursive bind
                let nonrecBind = NonRec b (mkLams lbs bindExpr)
{-
                liftCoreM $ putMsgS "Cond Lam = "
                liftCoreM $ putMsg $ ppr condLam
                liftCoreM $ putMsgS "Step Lam = "
                liftCoreM $ putMsg $ ppr stepLam
                liftCoreM $ putMsgS "Done Lam = "
                liftCoreM $ putMsg $ ppr stepLam
                liftCoreM $ putMsgS "While Expr = "
                liftCoreM $ putMsg $ ppr whileExpr
                liftCoreM $ putMsgS "----------------"
                liftCoreM $ putMsg $ ppr $ mkLams lbs bindExpr
-}
                -- Recursively call for the other binds in the array
                (nonrecs, bs') <- recurBind' bs
                return $ (nonrecBind : nonrecs, bs')
            else defaultRet
        _ -> defaultRet


checkForRecur :: Bool -> CoreExpr -> BindM (CoreExpr, Bool)
checkForRecur step e = do
    funcId <- gets funcId
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId bindNameTH
    thenId <- thNameToId bindThenNameTH
    fmapId <- thNameToId fmapNameTH
    apId <- thNameToId apNameTH
    case f of
      Var fv -> do
          -- Check if we have reached the bottom of the bind chain or if
          -- there is another level.
          if fv == bindId || fv == thenId
          then do
              -- TBD - This is a hack, need to generate dictionary 
              -- not copy it from a bind.
              s <- get               
              put s {dicts = [args !! 1]}              
              -- Check if the next level has a recur
              (e'', recurFlag) <- checkForRecur step $ last args
              let e''' = mkCoreApps f ((init args) ++ [e''])
              return $ (mkLams bs e''', recurFlag)
          else
              -- We are at the bottom of the bind chain.....
              -- Check for recursive call.
              -- Either in the form of (Var funcId) arg ...
              if fv == head funcId
              then do
                  ret_e <- genReturn $ head args
                  return (mkLams bs ret_e, True)
              -- ... Or in the form of (Var funcId) $ arg
              else if fv == apId
                   then case args of
                       [_, _, _, Var fv', arg] | fv' == head funcId -> do
                           ret_e <- genReturn arg
                           return (mkLams bs ret_e, True)
                       _ -> return (e, False)
                   else return (e, False)
      Case e' tb ty alts -> do
          alts' <- checkAltsForRecur step e' alts
          return (Case e' tb ty alts', False)
      _ -> return (e, False)


checkAltsForRecur :: Bool -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
checkAltsForRecur _ _ [] = return []
checkAltsForRecur step e ((ac, b, a) : as) = do
    recurErrName <- thNameToId recurErrNameTH
    (a', hasAp) <- checkForRecur step a
    a'' <- if hasAp
           then do
              -- For a Step branch, save the conditional
              e' <- case ac of
                      DataAlt d -> do
                        Just falseName <- liftCoreM $ thNameToGhcName falseNameTH
                        -- if d == falseName
                        if (getName d) == falseName
                        then do
                          -- If we are in the False branch of the case, we
                          -- need to negate the conditional
                          notName <- thNameToId notNameTH
                          return $ mkCoreApps (Var notName) [e]
                        else return e
              if step
              then do
                  -- Add conditional to list to generate while conditional
                  -- during the Step phase.
                  s <- get
                  put s {conds = e' : conds s}
                  return a'
              else return (Var recurErrName)
           else if step
                then return (Var recurErrName)
                else return a'
    bs' <- checkAltsForRecur step e as
    return $ (ac, b, a'') : bs'

genReturn :: CoreExpr -> BindM CoreExpr
genReturn e = do
    dicts <- gets dicts
    returnId <- thNameToId returnNameTH
    monadTyConId <- thNameToTyCon monadTyConTH
    let monadTyConTy = mkTyConTy monadTyConId
    -- dict <- buildDictionaryTyConT (tyConAppTyCon monadTyConTy) $ exprType e
    return $ mkCoreApps (Var returnId) [Type monadTyConTy, head dicts, Type (exprType e), e]

genWhileCond :: [CoreExpr] -> BindM CoreExpr
genWhileCond [c]        = return c
genWhileCond [c1,c2]    = do
    andId <- thNameToId andNameTH
    return $ mkCoreApps (Var andId) [c1, c2]
genWhileCond (c:cs) = do
    andId <- thNameToId andNameTH
    gcs <- genWhileCond cs
    return $ mkCoreApps (Var andId) [c, gcs]
 
changeVarExpr :: CoreBndr -> CoreBndr -> CoreExpr -> BindM CoreExpr
changeVarExpr f t e = do
  df <- liftCoreM getDynFlags
  case e of
    -- Replace any occurances of the parameters "p" with "abs_ p"
    Var v | v == f -> return $ Var t
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- changeVarExpr f t e1
      e2' <- changeVarExpr f t e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- changeVarExpr f t e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeVarExpr f t body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeVarExpr f t e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeVarExpr' f t rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeVarExpr f t e
      alts' <- changeVarExprAlts f t alts
      return $ Case e' tb ty alts'
    Tick ti e -> do
      e' <- changeVarExpr f t e
      return $ Tick ti e'
    Cast e co -> do
      e' <- changeVarExpr f t e
      return $ Cast e' co

changeVarExpr' :: CoreBndr -> CoreBndr -> [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeVarExpr' _ _ [] = return []
changeVarExpr' f t ((b, e) : bs) = do
  e' <- changeVarExpr f t e
  bs' <- changeVarExpr' f t bs
  return $ (b, e') : bs'

changeVarExprAlts :: CoreBndr -> CoreBndr -> [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeVarExprAlts _ _ [] = return []
changeVarExprAlts f t ((ac, b, a) : as) = do
  a' <- changeVarExpr f t a
  bs' <- changeVarExprAlts f t as
  return $ (ac, b, a') : bs'
