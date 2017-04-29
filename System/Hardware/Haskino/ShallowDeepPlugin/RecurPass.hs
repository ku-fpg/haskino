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
-- module System.Hardware.Haskino.ShallowDeepPlugin.RecurPass where

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
        Just (tyCon, tyArgs) -> do
            if length argTys == 1 && tyCon == monadTyCon && length tyArgs == 1
            then do
                let argTy = head argTys
                let retTyArg = head tyArgs
                s <- get
                put s {funcId = [b]}
{-
                liftCoreM $ putMsgS "One Arg Arudino Rec Bind"
                liftCoreM $ putMsg $ ppr b
                liftCoreM $ putMsg $ ppr argTys
                liftCoreM $ putMsg $ ppr retTy
-}

                let (lbs, e') = collectBinders e
                let arg = head lbs

                -- Build the step body of the While
                e''  <- checkForRecur argTy retTy e'
                newStepB <- buildId ("x") argTy
                stepE <- changeVarExpr arg newStepB e''
                let stepLam = mkLams [newStepB] stepE

                -- Build the iterate expression
                iterateEId <- thNameToId iterateETH
                eitherDict <- thNameTysToDict monadIterateTyConTH [argTy, retTy]
                let iterateExpr = mkCoreApps (Var iterateEId) [Type argTy, Type retTy, eitherDict, Var arg, stepLam]

                -- Create the transformed non-recursive bind
                let nonrecBind = NonRec b (mkLams lbs iterateExpr)
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


checkForRecur :: Type -> Type -> CoreExpr -> BindM CoreExpr
checkForRecur argTy retTy e = do
    funcId <- gets funcId
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId bindNameTH
    thenId <- thNameToId bindThenNameTH
    fmapId <- thNameToId fmapNameTH
    apId <- thNameToId apNameTH
    returnId <- thNameToId returnNameTH
    ifThenElseEitherId <- thNameToId ifThenElseEitherNameTH
    ifThenElseUnitId <- thNameToId ifThenElseUnitNameTH
    eitherTyCon <- thNameToTyCon eitherTyConTH
    let eitherTyp = mkTyConApp eitherTyCon [argTy, retTy]
    case f of
      -- Check if we have reached the bottom of the bind chain or if
      -- there is another level.
      Var fv | fv == bindId || fv == thenId -> do
          case args of
            [Type monadTy, dict, Type arg1Ty, Type arg2Ty, e1, e2] -> do
               e2' <- checkForRecur argTy retTy e2
               let e'' = mkCoreApps (Var fv) [Type monadTy, dict, Type arg1Ty, Type eitherTyp, e1, e2']
               return $ (mkLams bs e'')
            _ -> return e
      -- Check if the next level has a recur
      Var fv | fv == ifThenElseUnitId -> do
          case args of
            [cond, thenE, elseE] -> do
                thenE' <- checkForRecur argTy retTy thenE
                elseE' <- checkForRecur argTy retTy elseE
                eitherDict <- thNameTysToDict monadIterateTyConTH [argTy, retTy]
                let e'' = mkCoreApps (Var ifThenElseEitherId) [Type argTy, Type retTy, eitherDict, cond, thenE', elseE']
                return $ (mkLams bs e'')
            _ -> return e
      -- We are at the bottom of the bind chain.....
      -- Check for recursive call.
      -- Either in the form of (Var funcId) arg ...
      Var fv | fv == head funcId -> do
          -- Generate the ExprRight expression with the function arg
          argDict <- thNameTyToDict exprClassTyConTH argTy
          retDict <- thNameTyToDict exprClassTyConTH retTy
          rightId <- thNameToId rightNameTH
          let rightExpr = mkCoreApps (Var rightId) [Type argTy, Type retTy, argDict, retDict, head args]
          -- Generate the return expression
          monadTyConId <- thNameToTyCon monadTyConTH
          let monadTyConTy = mkTyConTy monadTyConId
          monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
          let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, rightExpr]
          return $ mkLams bs returnExpr
{-
          -- ... Or in the form of (Var funcId) $ arg
      Var fv | fv == apId -> do
          case args of
              [_, _, _, Var fv', arg] | fv' == head funcId -> do
                   -- TBD Save type on case
                   ret_e <- genReturn arg
                   let (tyCon, [tyArg]) = splitTyConApp $ exprType ret_e
                   return (mkLams bs ret_e, Immed tyArg)
              _ -> return e, No
-}
      -- This branch is not a recursive call
      Var fv | fv == returnId -> do
          case args of 
            [Type mTy, dict, Type ty, retE] -> do
                -- Generate the ExprRight expression with the function arg
                argDict <- thNameTyToDict exprClassTyConTH argTy
                retDict <- thNameTyToDict exprClassTyConTH retTy
                leftId <- thNameToId leftNameTH
                let leftExpr = mkCoreApps (Var leftId) [Type argTy, Type retTy, argDict, retDict, retE]
                -- Generate the return expression
                monadTyConId <- thNameToTyCon monadTyConTH
                let monadTyConTy = mkTyConTy monadTyConId
                monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
                let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, leftExpr]
                return $ mkLams bs returnExpr        
            _ -> return e
      _ -> return e -- TBD do other left with <$>

{-
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
-}

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


