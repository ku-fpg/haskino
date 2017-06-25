-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind argument and return type change pass
-- f :: a -> ... -> c -> d ==> f :: Expr a  -> ... -> Expr c -> Expr d
-- It does this by changing the type of the argument, and then replacing
-- each occurnace of (rep_ a) of the argument 'a' with just the type 
-- changed argument itself.
-- It does this by inserting a rep_ <$> to the last expresion of the bind
-- chain for the local bind to change the return type.
-- 
-- Local bind call site type change pass:
-- forall (f: a -> .. b -> Arduino a).
-- f aa .. ab
--   =
-- abs_ <$> f (rep aa) .. (rep ab)
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass (bindChangeArgRetAppPass) where

import CoreMonad
import GhcPlugins
import Avail
import NameSet
import Var
import Data.Functor
import Data.List
import qualified Data.Map as M
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

-- ToDo: Cleanup and Unify
comProcList :: [String]
comProcList = [ "systemReset"
              , "setPinModeE"
              , "digitalWriteE"
              , "digitalPortWriteE"
              , "analogWriteE"
              , "toneE"
              , "noToneE"
              , "i2cWriteE"
              , "i2cConfig"
              , "stepperSetSpeedE"
              , "servoDetachE"
              , "servoWriteE"
              , "servoWriteMicrosE"
              , "createTaskE"
              , "deleteTaskE"
              , "scheduleTaskE"
              , "attachIntE"
              , "detachIntE"
              , "interrupts"
              , "noInterrupts"
              , "scheduleReset"
              , "giveSemE"
              , "takeSemE"
              , "loopE"
              , "forInE"
              , "writeRemoteRef"
              , "modifyRemoteRef"
              , "queryFirmwareE"
              , "queryProcessorE"
              , "microsE"
              , "millisE"
              , "delayMillisE"
              , "delayMicrosE"
              , "digitalReadE"
              , "digitalPortReadE"
              , "analogReadE"
              , "i2CReadE"
              , "stepper2PinE"
              , "stepper4PinE"
              , "stepperStepE"
              , "servoAttachE"
              , "servoAttachMinMaxE"
              , "servoReadE"
              , "servoReadMicrosE"
              , "queryAllTasksE"
              , "queryTaskE"
              , "bootTaskE"
              , "readRemoteRef"
              , "newRemoteRef"
              , "ifThenElseBoolE"
              , "ifThenElseWord8E"
              , "ifThenElseWord16E"
              , "ifThenElseWord32E"
              , "ifThenElseInt8E"
              , "ifThenElseInt16E"
              , "ifThenElseInt32E"
              , "ifThenElseL8E"
              , "ifThenElseFloatE"
              , "ifThenElseW8Unit"
              , "ifThenElseW8W8"
              , "ifThenElseUnitUnit"
              , "ifThenElseUnitW8"
              , "ifThenElseW8Bool"
              , "whileBoolE"
              , "whileWord8E"
              , "whileWord16E"
              , "whileWord32E"
              , "whileInt8E"
              , "whileInt16E"
              , "whileInt32E"
              , "whileFloatE"
              , "whileL8E"
              , "iterateW8W8E"
              , "iterateW8UnitE"
              , "iterateUnitW8E"
              , "iterateUnitUnitE"
              , "iterateW8BoolE"
              , "liftIO"
              , "debugE"
              , "debugListen"
              , "die"
              ]

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        args :: [CoreBndr],
        shallowDeepMap :: M.Map Id Id
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
  liftCoreM m = BindM $ lift m
  getModGuts = gets pluginModGuts

deepSuffix :: String
deepSuffix = "_deep'"

bindChangeArgRetAppPass :: ModGuts -> CoreM ModGuts
bindChangeArgRetAppPass guts = do
    (bindsL', s) <- (\x -> (runStateT (runBindM $ (mapM changeArgBind) x) (BindEnv guts [] M.empty))) (mg_binds guts)
    let availNames = nameSetElems $ availsToNameSet $ mg_exports guts
    let shallowDeepMap' = M.filterWithKey (keyExported availNames) (shallowDeepMap s)
    let es = M.elems $ shallowDeepMap'
    let guts' = guts { mg_binds = concat bindsL', mg_exports = mg_exports guts ++ (map (Avail NotPatSyn)(map varName es)) }
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeAppBind) x) (BindEnv guts' [] (shallowDeepMap s)))) guts'
  where
    keyExported :: [Name] -> Id  -> Id -> Bool
    keyExported ns sId _ = (varName sId) `elem` ns

changeArgBind :: CoreBind -> BindM [CoreBind]
changeArgBind (NonRec b e) = do
  ides <- changeSubBind b e
  let bs = map (\ide -> NonRec (fst ide) (snd ide)) ides
  return bs
changeArgBind bndr@(Rec bs) = do
  (nrbs', rbs') <- changeArgBind' bs
  return $ nrbs' ++ [Rec rbs']

changeArgBind' :: [(Id, CoreExpr)] -> BindM ([CoreBind], [(Id, CoreExpr)])
changeArgBind' [] = return ([], [])
changeArgBind' ((b, e) : bs) = do
  ides <- changeSubBind b e
  (nrbs', rbs') <- changeArgBind' bs
  if length ides == 1
  then return (nrbs', ides ++ rbs')
  else do
    let [nrb, rb] = ides
    let (b', e')  = nrb
    return ((NonRec b' e') : nrbs', rb : rbs')

changeSubBind :: Id -> CoreExpr -> BindM [(Id, CoreExpr)]
changeSubBind b e = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ varType b
  let (bs, e') = collectBinders e
  let tyCon_m = splitTyConApp_maybe retTy
  monadTyConId <- thNameToTyCon monadTyConTH
  unitTyConId <- thNameToTyCon ''()
  let unitTyConTy = mkTyConTy unitTyConId
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
          -- Change the binds and arg types to Expr a
          zipBsArgTys <- mapM changeArg (zip bs argTys)
          let (bs', argTys') = unzip zipBsArgTys

          -- Put arg types into state
          s <- get
          put s{args = bs'}

          -- Change any apps of the args in the body
          e'' <- changeArgAppsExpr e'

          -- Generate args for new shallow body
          deepArgs <- mapM repExpr (map Var bs)

          -- If it is not a unit type return, change return type
          if not (retTy' `eqType` unitTyConTy)
          then do
              exprTyCon <- thNameToTyCon exprTyConTH
              let exprTyConApp = mkTyConApp exprTyCon [retTy']

              -- Change the return
              e''' <- fmapRepBindReturn e''

              -- Create a new top level bind type with the deep tyep
              bDeep <- modId b deepSuffix
              let b' = setVarType bDeep $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])

              -- Apply the abs <$> to the new shallow body
              let shallowE = mkCoreApps (Var b') deepArgs
              absExpr <- fmapAbsExpr (mkTyConTy retTyCon) retTy' shallowE

              -- Put id pair into state
              s <- get
              put s{shallowDeepMap = M.insert b b' (shallowDeepMap s)}

              return [(b, mkLams bs absExpr), (b', mkLams bs' e''')]
          else if length bs > 0 
              then do
                  -- Create a new top level bind type with the deep tyep
                  bDeep <- modId b deepSuffix
                  let b' = setVarType bDeep $ mkFunTys argTys' retTy

                  let shallowE = mkCoreApps (Var b') deepArgs

                  -- Put id pair into state
                  s <- get
                  put s{shallowDeepMap = M.insert b  b' (shallowDeepMap s)}

                  return [(b, mkLams bs shallowE), (b', mkLams bs' e'')]
              else return [(b, e)]
      _ -> return [(b, e)]

changeArg :: (CoreBndr, Type) -> BindM (CoreBndr, Type)
changeArg (b, ty) = do
  let tyCon_m = splitTyConApp_maybe ty
  case tyCon_m of
    Just (_, []) -> do
        -- ToDo:  Check that type is a valid Haskino Expr Type?
        -- Lookup the GHC type constructor of Expr
        exprTyCon <- thNameToTyCon exprTyConTH
        -- Make the type of the Expr for the specified type
        let ty' = mkTyConApp exprTyCon [ty]
        let b' = setVarType b ty'
        return (b', ty')
    _       -> return (b, ty)

changeArgAppsExpr :: CoreExpr -> BindM CoreExpr
changeArgAppsExpr e = do
  df <- liftCoreM getDynFlags
  s <- get
  repId <- thNameToId repNameTH
  case e of
    -- Replace any occurances of the parameters "p" with "abs_ p"
    Var v | v `elem` (args s) -> absExpr e
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- changeArgAppsExpr e1
      e2' <- changeArgAppsExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- changeArgAppsExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeArgAppsExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeArgAppsExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeArgAppsExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeArgAppsExpr e
      alts' <- changeArgAppsExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeArgAppsExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeArgAppsExpr e
      return $ Cast e' co

changeArgAppsExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeArgAppsExpr' [] = return []
changeArgAppsExpr' ((b, e) : bs) = do
  e' <- changeArgAppsExpr e
  bs' <- changeArgAppsExpr' bs
  return $ (b, e') : bs'

changeArgAppsExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeArgAppsExprAlts [] = return []
changeArgAppsExprAlts ((ac, b, a) : as) = do
  a' <- changeArgAppsExpr a
  bs' <- changeArgAppsExprAlts as
  return $ (ac, b, a') : bs'

changeAppBind :: CoreBind -> BindM CoreBind
changeAppBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (bs, e') = collectBinders e
  e'' <- changeAppExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeAppBind bndr@(Rec bs) = do
  bs' <- changeAppBind' bs
  return $ Rec bs'

changeAppBind' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeAppBind' [] = return []
changeAppBind' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- changeAppExpr e'
  let e''' = mkLams lbs e''
  bs' <- changeAppBind' bs
  return $ (b, e''') : bs'

changeAppExpr :: CoreExpr -> BindM CoreExpr
changeAppExpr e = do
  df <- liftCoreM getDynFlags
  monadTyConId <- thNameToTyCon monadTyConTH
  unitTyConId <- thNameToTyCon ''()
  let unitTyConTy = mkTyConTy unitTyConId
  s <- get
  let sdMap = shallowDeepMap s
  case e of
    Var v -> do
      let tyCon_m = splitTyConApp_maybe $ varType v
      let defaultRet = return $ Var v
      let vs = varString v
      if isSuffixOf deepSuffix vs || vs `elem` comProcList
      then defaultRet
      else case tyCon_m of
            Just (retTyCon, [retTy']) | retTyCon == monadTyConId &&
                                        not (retTy' `eqType` unitTyConTy) -> do
                v' <- if v `M.member` sdMap
                      then return $ sdMap M.! v
                      else stringToId $ (varString v) ++ deepSuffix
                fmapAbsExpr (mkTyConTy retTyCon) retTy' (Var v')
            _ -> defaultRet
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (b, args) = collectArgs e
      let (argTys, retTy) = splitFunTys $ exprType b
      let tyCon_m = splitTyConApp_maybe retTy
      let defaultRet = do
            e1' <- changeAppExpr e1
            e2' <- changeAppExpr e2
            return $ App e1' e2'
      let (Var vb) = b
      let vbs = varString vb
      if isSuffixOf deepSuffix vbs || vbs `elem` comProcList
      then defaultRet
      else case tyCon_m of
            Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
                vb' <- if vb `M.member` sdMap
                       then return $ sdMap M.! vb
                       else stringToId $ (varString vb) ++ deepSuffix
                args' <- mapM repExpr args
                if not (retTy' `eqType` unitTyConTy)
                then do
                    let e' = mkCoreApps (Var vb') args'
                    absExpr <- fmapAbsExpr (mkTyConTy retTyCon) retTy' e'
                    return $ absExpr
                else do
                    return $ mkCoreApps (Var vb') args'
            _ -> defaultRet
    Lam tb e -> do
      e' <- changeAppExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeAppExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeAppExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeAppExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeAppExpr e
      alts' <- changeAppExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeAppExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeAppExpr e
      return $ Cast e' co

changeAppExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeAppExpr' [] = return []
changeAppExpr' ((b, e) : bs) = do
  e' <- changeAppExpr e
  bs' <- changeAppExpr' bs
  return $ (b, e') : bs'

changeAppExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeAppExprAlts [] = return []
changeAppExprAlts ((ac, b, a) : as) = do
  a' <- changeAppExpr a
  bs' <- changeAppExprAlts as
  return $ (ac, b, a') : bs'
