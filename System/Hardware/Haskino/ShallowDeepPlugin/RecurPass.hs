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

import Control.Monad.State
import CoreMonad
import GhcPlugins

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
recurBind (NonRec b e) = do
    e' <- recurSubExpr e
    return [NonRec b e']
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
    listTyCon' <- thNameToTyCon listTyConTH
    case retTyCon_m of
      Just (retTyCon, retTyArgs) -> do
        if length argTys == 1 && retTyCon == monadTyCon && length retTyArgs == 1
        then do
          let argTyCon_m = splitTyConApp_maybe $ head argTys
          case argTyCon_m of
            Just (argTyCon, argTyArgs) -> do
                let argTyArg = head argTyArgs
                let retTyArg = case splitTyConApp_maybe $ head retTyArgs of
                                  Just (rTyCon, []) -> mkTyConTy rTyCon
                                  Just (rTyCon, [retTyArg']) -> retTyArg'
                                  _                 -> head retTyArgs
                s' <- get
                put s' {funcId = [b]}

                let (lbs, e') = collectBinders e
                e'' <- recurSubExpr e'
                let arg = head lbs

                -- Build the step body of the While
                e'''  <- transformRecur argTyArg retTyArg e''
                newStepB <- buildId ("x") argTyArg
                stepE <- changeVarExpr arg newStepB e'''
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
        else if length argTys == 0 && retTyCon == monadTyCon && length retTyArgs == 1
        then do
            let retTyArg = case splitTyConApp_maybe $ head retTyArgs of
                              Just (rTyCon, [])      -> mkTyConTy rTyCon
                              _                      -> head retTyArgs

            unitTyCon' <- thNameToTyCon unitTyConTH
            let unitTyConTy = mkTyConTy unitTyCon'
            -- Generate "Expr retTy"
            exprTyConApp <- thNameTyToTyConApp exprTyConTH unitTyConTy
            let newBTy = mkFunTy exprTyConApp retTy
            newStepB <- buildId ((varString b) ++ "'") newBTy
            -- Create body of wrapper function
            unitValueId <- thNameToId unitValueTH
            let wrapperB = mkCoreApps (Var newStepB) [(Var unitValueId)]

            -- Create new function argument
            newArgB <- buildId "p" unitTyConTy

            -- Build the step body of the While
            e'  <- transformRecur unitTyConTy retTyArg e
            newStepLamB <- buildId ("x") unitTyConTy
            let stepLam = mkLams [newStepLamB] e'

            -- Build the iterate expression
            iterateEId <- thNameToId iterateETH
            eitherDict <- thNameTysToDict monadIterateTyConTH [unitTyConTy, retTyArg]
            let iterateExpr = mkCoreApps (Var iterateEId) [Type unitTyConTy, Type retTyArg, eitherDict, Var newArgB, stepLam]

            -- Create the transformed non-recursive bind
            let nonrecBind = NonRec newStepB (mkLams [newArgB] iterateExpr)
            -- Create the wrapper bind
            let wrapperBind = NonRec b wrapperB

            -- Recursively call for the other binds in the array
            (nonrecs, bs') <- recurBind' bs
            return $ ([nonrecBind, wrapperBind] ++ nonrecs, bs')
        else if retTyCon == monadTyCon
        then error $ "*** Haskiono RecurPass: " ++ getOccString b ++ ": Unable to translate recursive function with\ngreater than one arguments"
        else defaultRet
      _ -> defaultRet


transformRecur :: Type -> Type -> CoreExpr -> BindM CoreExpr
transformRecur argTy retTy e = do
    funcId' <- gets funcId
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId bindNameTH
    thenId <- thNameToId bindThenNameTH
    returnId <- thNameToId returnNameTH
    ifThenElseEitherId <- thNameToId ifThenElseEitherNameTH
    ifThenElseId <- thNameToId ifThenElseNameTH
    eitherTyCon <- thNameToTyCon eitherTyConTH
    let eitherTyp = mkTyConApp eitherTyCon [argTy, retTy]
    case f of
      -- Check if we have reached the bottom of the bind chain or if
      -- there is another level.
      Var fv | fv == bindId || fv == thenId -> do
          case args of
            [Type monadTy, dict, Type arg1Ty, Type _, e1, e2] -> do
               e2' <- transformRecur argTy retTy e2
               let e'' = mkCoreApps (Var fv) [Type monadTy, dict, Type arg1Ty, Type eitherTyp, e1, e2']
               return $ (mkLams bs e'')
            _ -> return e
      -- Check if the next level has a recur
      Var fv | fv == ifThenElseId -> do
          case args of
            [Type _, _, cond, thenE, elseE] -> do
                thenE' <- transformRecur argTy retTy thenE
                elseE' <- transformRecur argTy retTy elseE
                eitherDict <- thNameTysToDict monadIterateTyConTH [argTy, retTy]
                let e'' = mkCoreApps (Var ifThenElseEitherId) [Type argTy, Type retTy, eitherDict, cond, thenE', elseE']
                return $ (mkLams bs e'')
            _ -> return e
      -- We are at the bottom of the bind chain.....
      -- Check for recursive call.
      Var fv | fv == head funcId' -> do
          -- Find the argument for the Left call
          unitValueId <- thNameToId unitValueTH
          let leftArg = if length args == 0
                        then Var unitValueId
                        else head args
          -- Generate the ExprLeft expression with the function arg
          argDict <- thNameTyToDict exprClassTyConTH argTy
          retDict <- thNameTyToDict exprClassTyConTH retTy
          leftId <- thNameToId leftNameTH
          let leftExpr = mkCoreApps (Var leftId) [Type argTy, Type retTy, argDict, retDict, leftArg]
          -- Generate the return expression
          monadTyConId <- thNameToTyCon monadTyConTH
          let monadTyConTy = mkTyConTy monadTyConId
          monadDict <- thNameTyToDict monadClassTyConTH monadTyConTy
          let returnExpr = mkCoreApps (Var returnId) [Type monadTyConTy, monadDict, Type eitherTyp, leftExpr]
          return $ mkLams bs returnExpr
      -- This branch is not a recursive call
      Var fv | fv == returnId -> do
          case args of
            [Type _, _, Type _, retE] -> do
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
      -- This is another Arduino function, so we need to fmap ExprRight to it.
      _ -> do
          -- Generate "Expr retTy"
          exprTyConApp <- thNameTyToTyConApp exprTyConTH retTy
          -- Generate functorArduino ditctionary
          monadTyConId <- thNameToTyCon monadTyConTH
          let monadTyConTy = mkTyConTy monadTyConId
          functDict <- thNameTyToDict functTyConTH monadTyConTy
          -- Generate the ExprRight expression with the function arg
          argDict <- thNameTyToDict exprClassTyConTH argTy
          retDict <- thNameTyToDict exprClassTyConTH retTy
          rightId <- thNameToId rightNameTH
          let rightExpr = mkCoreApps (Var rightId) [Type argTy, Type retTy, argDict, retDict]
          -- Generate the fmapExpr
          fmapId <- thNameToId fmapNameTH
          let fmapExpr = mkCoreApps (Var fmapId) [Type monadTyConTy, Type exprTyConApp, Type eitherTyp, functDict, rightExpr, e']
          return $ mkLams bs fmapExpr

changeVarExpr :: CoreBndr -> CoreBndr -> CoreExpr -> BindM CoreExpr
changeVarExpr f t e = do
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
    Lam tb el -> do
      e' <- changeVarExpr f t el
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeVarExpr f t body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- changeVarExpr f t el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeVarExpr' f t rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- changeVarExpr f t ec
      alts' <- changeVarExprAlts f t alts
      return $ Case e' tb ty alts'
    Tick ti et -> do
      e' <- changeVarExpr f t et
      return $ Tick ti e'
    Cast ec co -> do
      e' <- changeVarExpr f t ec
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

recurSubExpr :: CoreExpr -> BindM CoreExpr
recurSubExpr e = do
  case e of
    Var _ -> return e
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- recurSubExpr e1
      e2' <- recurSubExpr e2
      return $ App e1' e2'
    Lam tb el -> do
      e' <- recurSubExpr el
      return $ Lam tb e'
    Let bind body -> do
      case bind of
        (NonRec v el) -> do
          e' <- recurSubExpr el
          body' <- recurSubExpr body
          return $ Let (NonRec v e') body'
        (Rec rbs) -> do
          rbs' <- recurSubExpr' rbs
          body' <- recurSubExpr body
          (nonRec, rbs'') <- recurBind' rbs'
          -- ToDo: Recurse inside of bind expressions.
          let ls = case rbs'' of
                    [] -> nonRec
                    _  -> (Rec rbs'') : nonRec
          return $ mkCoreLets ls body'
    Case ec tb ty alts -> do
      e' <- recurSubExpr ec
      alts' <- recurSubExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- recurSubExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- recurSubExpr ec
      return $ Cast e' co

recurSubExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
recurSubExpr' [] = return []
recurSubExpr' ((b, e) : bs) = do
  e' <- recurSubExpr e
  bs' <- recurSubExpr' bs
  return $ (b, e') : bs'

recurSubExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
recurSubExprAlts [] = return []
recurSubExprAlts ((ac, b, a) : as) = do
  a' <- recurSubExpr a
  bs' <- recurSubExprAlts as
  return $ (ac, b, a') : bs'
