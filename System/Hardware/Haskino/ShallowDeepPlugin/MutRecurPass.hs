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
module System.Hardware.Haskino.ShallowDeepPlugin.MutRecurPass (mutRecurPass) where

import Control.Monad.State
import CoreMonad
import Data.List
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

mutSuffix :: String
mutSuffix = "_mut'"

mutRecurPass :: ModGuts -> CoreM ModGuts
mutRecurPass guts = do
    bindsL' <- (\x -> fst <$> (runStateT (runBindM $ (mapM mutRecurBind) x) (BindEnv guts []))) (mg_binds guts)
    return (guts { mg_binds = concat bindsL' })

mutRecurBind :: CoreBind -> BindM [CoreBind]
mutRecurBind (NonRec b e) = do
    e' <- mutRecurSubExpr e
    return [NonRec b e']
mutRecurBind (Rec bs) = do
    (nonRec, bs') <- mutRecurBind' bs
    return $ nonRec ++ [Rec bs']

mutRecurBind' :: [(Id, CoreExpr)] -> BindM ([CoreBind],[(Id, CoreExpr)])
mutRecurBind' bs = do
    let defaultRet = do
            bs' <- mapM mutRecurBind'' bs
            return $ ([], bs')
    if (length bs /= 1) 
    then do
        let (ids, es) = unzip bs
            esTypes  = map exprType es
            eqExpr   = length (nubBy eqType esTypes) == 1
        if length (nubBy eqType esTypes) == 1
        then mutRecurXform bs
        else defaultRet
    else defaultRet

mutRecurBind'' :: (Id, CoreExpr) -> BindM (Id, CoreExpr)
mutRecurBind'' (id, e) = do
    e' <- mutRecurSubExpr e
    return (id, e')

mutRecurXform :: [(Id, CoreExpr)] -> BindM ([CoreBind],[(Id, CoreExpr)])
mutRecurXform bs = do
    let defaultRet = do
            bs' <- mapM mutRecurBind'' bs
            return $ ([], bs')
    let (ids, es) = unzip bs
    s <- get
    put s {funcId = ids}
    let (argTys, retTy) = splitFunTys $ exprType $ head es
    let retTyCon_m = splitTyConApp_maybe retTy
    monadTyCon <- thNameToTyCon monadTyConTH
    exprTyCon <- thNameToTyCon exprTyConTH
    intTyCon <- thNameToTyCon intTyConTH
    iterateEId <- thNameToId iterateETH
    let intTyConTy = mkTyConTy intTyCon
    let exprIntTy = mkTyConApp exprTyCon [intTyConTy]
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
                                  Just (rTyCon, [retTyArg']) | rTyCon == exprTyCon -> retTyArg'
                                  _                 -> head retTyArgs
                let (lbs, e') = collectBinders $ head es
                let arg = head lbs

                -- Build the new Step ID used for Iteration (x)
                let exprArgTy = mkTyConApp exprTyCon [head argTys]
                newStepB <- buildId ("x") $ exprArgTy

                -- Build the new function index ID's, one for the
                -- lambda passed to IterateE (i), and another for the one
                -- we will add to the xformed function and passed to
                -- IterateE (indx)
                newSelectB <- buildId ("i") exprIntTy
                newArgB <- buildId ("indx") exprIntTy

                -- Transform the bodies of each of the functions
                es' <- transformRecurs argTyArg retTyArg newStepB es

                -- Wrap the transformed functions with ifThenElseEither's
                -- that test the function index
                es'' <- addIfThenElses newSelectB argTyArg retTyArg es'

                let stepLam = mkLams [newSelectB, newStepB] es''

                -- Build the iterate expression
                eitherDict <- thNameTysToDict monadIterateTyConTH [argTyArg, retTyArg]
                let iterateExpr = mkCoreApps (Var iterateEId) [Type argTyArg, Type retTyArg, eitherDict, Var newArgB, Var arg, stepLam]

                -- Build the new Bind
                bMut <- modId (head ids) mutSuffix
                let newFunc = setVarType bMut $ mkFunTys (exprIntTy : argTys) retTy
                let xformedBind = NonRec newFunc $ mkLams (newArgB : lbs) iterateExpr

                -- Modify bodies of old functions
                newFuncBinds <- modOrigFuncs ids newFunc 

                -- Return modified originals and new function
                return $ (xformedBind : newFuncBinds, [])
            _ -> defaultRet
{-
        else if length argTys == 0 && retTyCon == monadTyCon && length retTyArgs == 1
        then do
                let retTyArg = case splitTyConApp_maybe $ head retTyArgs of
                                  Just (rTyCon, [])      -> mkTyConTy rTyCon
                                  _                      -> head retTyArgs

                s' <- get
                put s' {funcId = ids}

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
                let iterateExpr = mkCoreApps (Var iterateEId) [Type unitTyConTy, Type retTyArg, eitherDict, Var litZeroId,  Var newArgB, stepLam]

                -- Create the transformed non-recursive bind
                let nonrecBind = NonRec newStepB (mkLams [newArgB] iterateExpr)
                -- Create the wrapper bind
                let wrapperBind = NonRec b wrapperB

                -- Recursively call for the other binds in the array
                (nonrecs, bs') <- mutRecurBind' bs
                return $ ([nonrecBind, wrapperBind] ++ nonrecs, bs')
-}
        else if retTyCon == monadTyCon
        then error $ "*** Haskiono RecurPass: " ++ getOccString (head ids) ++ ": Unable to translate recursive function with\ngreater than one arguments"
        else defaultRet
      _ -> defaultRet

modOrigFuncs :: [Id] -> Id -> BindM [CoreBind]
modOrigFuncs ids newFunc = modOrigFuncs' ids 0
  where
    modOrigFuncs' :: [Id] -> Int -> BindM [CoreBind]
    modOrigFuncs' [] _ = return []
    modOrigFuncs' (f:fs) i = do
        df <- liftCoreM getDynFlags
        indexArg <- repExpr $ mkIntExprInt df i
        let newe = mkCoreApps (Var newFunc) [indexArg]
        bs' <- modOrigFuncs' fs (i+1)
        return $ (NonRec f newe) : bs'

addIfThenElses :: CoreBndr -> Type -> Type -> [CoreExpr] -> BindM CoreExpr
addIfThenElses sb at rt es = addIfThenElses' 0 es
  where
    addIfThenElses' :: Int -> [CoreExpr] -> BindM CoreExpr
    addIfThenElses' _ [s] = return s 
    addIfThenElses' i (tb : eb : []) = do
        ifThenElseEitherId <- thNameToId ifThenElseEitherNameTH
        eitherDict <- thNameTysToDict monadIterateTyConTH [at, rt]
        eqId <- thNameToId eqNameTH
        intTyCon <- thNameToTyCon intTyConTH
        let intTyConTy = mkTyConTy intTyCon
        exprTyCon <- thNameToTyCon exprTyConTH
        let exprIntTy = mkTyConApp exprTyCon [intTyConTy]
        eqDict <- thNameTysToDict exprClassTyConTH [intTyConTy]
        df <- liftCoreM getDynFlags
        indexArg <- repExpr $ mkIntExprInt df i
        let cond = mkCoreApps (Var eqId) [Type exprIntTy, eqDict, (Var sb), indexArg]
        return $ mkCoreApps (Var ifThenElseEitherId) [Type at, Type rt, eitherDict, cond, tb, eb]
    addIfThenElses i (tb : rest) = do
        ifThenElseEitherId <- thNameToId ifThenElseEitherNameTH
        eitherDict <- thNameTysToDict monadIterateTyConTH [at, rt]
        eqId <- thNameToId eqNameTH
        intTyCon <- thNameToTyCon intTyConTH
        let intTyConTy = mkTyConTy intTyCon
        exprTyCon <- thNameToTyCon exprTyConTH
        let exprIntTy = mkTyConApp exprTyCon [intTyConTy]
        eqDict <- thNameTysToDict exprClassTyConTH [intTyConTy]
        df <- liftCoreM getDynFlags
        indexArg <- repExpr $ mkIntExprInt df i
        let cond = mkCoreApps (Var eqId) [Type exprIntTy, eqDict, (Var sb), indexArg]
        eb' <- addIfThenElses' (i+1) rest
        return $ mkCoreApps (Var ifThenElseEitherId) [Type at, Type rt, eitherDict, cond, tb, eb']

transformRecurs :: Type -> Type -> CoreBndr -> [CoreExpr] -> BindM [CoreExpr]
transformRecurs argTy retTy newStepB es = mapM transformRecur' es
  where
    transformRecur' :: CoreExpr -> BindM CoreExpr
    transformRecur' e = do
        let (lbs, e') = collectBinders e
        let arg = head lbs
        e'' <- mutRecurSubExpr e'
        e''' <- transformRecur argTy retTy e''
        changeVarExpr arg newStepB e'''

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
      Var fv | fv `elem` funcId' -> do
          -- Find the argument for the Left call
          unitValueId <- thNameToId unitValueTH
          let leftArg = if length args == 0
                        then Var unitValueId
                        else head args
          -- Generate the ExprLeft expression with the function arg
          argDict <- thNameTyToDict exprClassTyConTH argTy
          retDict <- thNameTyToDict exprClassTyConTH retTy
          leftId <- thNameToId leftNameTH
          df <- liftCoreM getDynFlags
          let Just funcIndex = fv `elemIndex` funcId'
          leftIndexArg <- repExpr $ mkIntExprInt df funcIndex
          let leftExpr = mkCoreApps (Var leftId) [Type argTy, Type retTy, argDict, retDict, leftIndexArg, leftArg]
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

mutRecurSubExpr :: CoreExpr -> BindM CoreExpr
mutRecurSubExpr e = do
  case e of
    Var _ -> return e
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- mutRecurSubExpr e1
      e2' <- mutRecurSubExpr e2
      return $ App e1' e2'
    Lam tb el -> do
      e' <- mutRecurSubExpr el
      return $ Lam tb e'
    Let bind body -> do
      case bind of
        (NonRec v el) -> do
          e' <- mutRecurSubExpr el
          body' <- mutRecurSubExpr body
          return $ Let (NonRec v e') body'
        (Rec rbs) -> do
          rbs' <- mutRecurSubExpr' rbs
          body' <- mutRecurSubExpr body
          (nonRec, rbs'') <- mutRecurBind' rbs'
          -- ToDo: Recurse inside of bind expressions.
          let ls = case rbs'' of
                    [] -> nonRec
                    _  -> (Rec rbs'') : nonRec
          return $ mkCoreLets ls body'
    Case ec tb ty alts -> do
      e' <- mutRecurSubExpr ec
      alts' <- mutRecurSubExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- mutRecurSubExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- mutRecurSubExpr ec
      return $ Cast e' co

mutRecurSubExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
mutRecurSubExpr' [] = return []
mutRecurSubExpr' ((b, e) : bs) = do
  e' <- mutRecurSubExpr e
  bs' <- mutRecurSubExpr' bs
  return $ (b, e') : bs'

mutRecurSubExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
mutRecurSubExprAlts [] = return []
mutRecurSubExprAlts ((ac, b, a) : as) = do
  a' <- mutRecurSubExpr a
  bs' <- mutRecurSubExprAlts as
  return $ (ac, b, a') : bs'
