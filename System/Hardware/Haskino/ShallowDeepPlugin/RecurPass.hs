-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RecurPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Recursion Transformation Pass
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
        funcId        :: [Id]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
    liftCoreM m = BindM $ lift m
    getModGuts = gets pluginModGuts


recurPass :: ModGuts -> CoreM ModGuts
recurPass guts = do
    bindsL' <- (\x -> fst <$> (runStateT (runBindM $ (mapM recurBind) x) (BindEnv guts []))) (mg_binds guts)
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
    put s {funcId = [b]}
    let (argTys, retTy) = splitFunTys $ exprType e
    let retTyCon_m = splitTyConApp_maybe retTy
    monadTyCon <- thNameToTyCon monadTyConTH
    case retTyCon_m of
      Just (retTyCon, retTyArgs) -> do
        if length argTys == 1 && retTyCon == monadTyCon && length retTyArgs == 1
        then do
          let argTyCon_m = splitTyConApp_maybe $ head argTys
          case argTyCon_m of
            Just (argTyCon, argTyArgs) -> do
                let argTyArg = head argTyArgs
                let retTyArg = case splitTyConApp_maybe $ head retTyArgs of
                                  Just (rTyCon, [])      -> mkTyConTy rTyCon
                                  Just (rTyCon, rTyArgs) -> head rTyArgs
                                  Nothing                -> head retTyArgs
                s <- get
                put s {funcId = [b]}

                let (lbs, e') = collectBinders e
                let arg = head lbs

                -- Build the step body of the While
                e''  <- checkForRecur argTyArg retTyArg e'
                newStepB <- buildId ("x") argTyArg
                stepE <- changeVarExpr arg newStepB e''
                let stepLam = mkLams [newStepB] stepE

                -- Build the iterate expression
                iterateEId <- thNameToId iterateETH
                eitherDict <- thNameTysToDict monadIterateTyConTH [argTyArg, retTyArg]
                let iterateExpr = mkCoreApps (Var iterateEId) [Type argTyArg, Type retTyArg, eitherDict, Var arg, stepLam]

                -- Create the transformed non-recursive bind
                let nonrecBind = NonRec b (mkLams lbs iterateExpr)

                -- Recursively call for the other binds in the array
                (nonrecs, bs') <- recurBind' bs
                return $ (nonrecBind : nonrecs, bs')
            _ -> defaultRet
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
    ifThenElseId <- thNameToId ifThenElseNameTH
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
      -- Check if the next level has a recur
      Var fv | fv == ifThenElseId -> do
          case args of
            [Type _, _, cond, thenE, elseE] -> do
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
          -- Generate the ExprLeft expression with the function arg
          argDict <- thNameTyToDict exprClassTyConTH argTy
          retDict <- thNameTyToDict exprClassTyConTH retTy
          leftId <- thNameToId leftNameTH
          let leftExpr = mkCoreApps (Var leftId) [Type argTy, Type retTy, argDict, retDict, head args]
          -- Generate the return expression
          monadTyConId <- thNameToTyCon monadTyConTH
          let monadTyConTy = mkTyConTy monadTyConId
          monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
          let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, leftExpr]
          return $ mkLams bs returnExpr
          -- ... Or in the form of (Var funcId) $ arg
      Var fv | fv == apId -> do
          case args of
              [_, _, _, _, Var fv', arg] | fv' == head funcId -> do
                -- Generate the ExprLeft expression with the function arg
                argDict <- thNameTyToDict exprClassTyConTH argTy
                retDict <- thNameTyToDict exprClassTyConTH retTy
                leftId <- thNameToId leftNameTH
                let leftExpr = mkCoreApps (Var leftId) [Type argTy, Type retTy, argDict, retDict, arg]
                -- Generate the return expression
                monadTyConId <- thNameToTyCon monadTyConTH
                let monadTyConTy = mkTyConTy monadTyConId
                monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
                let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, leftExpr]
                return $ mkLams bs returnExpr
              _ -> return e -- TBD non-recursive call
      -- This branch is not a recursive call
      Var fv | fv == returnId -> do
          case args of 
            [Type mTy, dict, Type ty, retE] -> do
                -- Generate the ExprRight expression with the function arg
                argDict <- thNameTyToDict exprClassTyConTH argTy
                retDict <- thNameTyToDict exprClassTyConTH retTy
                rightId <- thNameToId rightNameTH
                let rightExpr = mkCoreApps (Var rightId) [Type argTy, Type retTy, argDict, retDict, retE]
                -- Generate the return expression
                monadTyConId <- thNameToTyCon monadTyConTH
                let monadTyConTy = mkTyConTy monadTyConId
                monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
                let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, rightExpr]
                return $ mkLams bs returnExpr        
            _ -> return e
      -- This is another Arduino function, so we need to fmap 
      -- _ ->
      _ -> return e -- TBD do other left with <$>


{-
(<$>
  @ Arduino
  @ (Expr Bool)
  @ (ExprEither Word8 Bool)
  System.Hardware.Haskino.Data.$fFunctorArduino

-}



changeVarExpr :: CoreBndr -> CoreBndr -> CoreExpr -> BindM CoreExpr
changeVarExpr f t e = do
  df <- liftCoreM getDynFlags
  case e of
    -- Replace any occurances of the variable "f" with "t"
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


