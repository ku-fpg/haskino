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
              , "writeRemoteRefE"
              , "modifyRemoteRefE"
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
              , "readRemoteRefE"
              , "newRemoteRefE"
              , "ifThenElseBoolE"
              , "ifThenElseWord8E"
              , "ifThenElseWord16E"
              , "ifThenElseWord32E"
              , "ifThenElseInt8E"
              , "ifThenElseInt16E"
              , "ifThenElseInt32E"
              , "ifThenElseL8E"
              , "ifThenElseFloatE"
              , "ifThenElseUnitE"
              , "ifThenElseW8Unit"
              , "ifThenElseW8W8"
              , "ifThenElseUnitUnit"
              , "ifThenElseUnitBool"
              , "ifThenElseUnitW8"
              , "ifThenElseUnitW16"
              , "ifThenElseUnitW32"
              , "ifThenElseUnitI8"
              , "ifThenElseUnitI16"
              , "ifThenElseUnitI32"
              , "ifThenElseUnitL8"
              , "ifThenElseUnitFloat"
              , "ifThenElseBoolUnit"
              , "ifThenElseBoolBool"
              , "ifThenElseBoolW8"
              , "ifThenElseBoolW16"
              , "ifThenElseBoolW32"
              , "ifThenElseBoolI8"
              , "ifThenElseBoolI16"
              , "ifThenElseBoolI32"
              , "ifThenElseBoolL8"
              , "ifThenElseBoolFloat"
              , "ifThenElseW8Unit"
              , "ifThenElseW8Bool"
              , "ifThenElseW8W8"
              , "ifThenElseW8W16"
              , "ifThenElseW8W32"
              , "ifThenElseW8I8"
              , "ifThenElseW8I16"
              , "ifThenElseW8I32"
              , "ifThenElseW8L8"
              , "ifThenElseW8Float"
              , "ifThenElseW16Unit"
              , "ifThenElseW16Bool"
              , "ifThenElseW16W8"
              , "ifThenElseW16W16"
              , "ifThenElseW16W32"
              , "ifThenElseW16I8"
              , "ifThenElseW16I16"
              , "ifThenElseW16I32"
              , "ifThenElseW16L8"
              , "ifThenElseW16Float"
              , "ifThenElseW32Unit"
              , "ifThenElseW32Bool"
              , "ifThenElseW32W8"
              , "ifThenElseW32W16"
              , "ifThenElseW32W32"
              , "ifThenElseW32I8"
              , "ifThenElseW32I16"
              , "ifThenElseW32I32"
              , "ifThenElseW32L8"
              , "ifThenElseW32Float"
              , "ifThenElseI8Unit"
              , "ifThenElseI8Bool"
              , "ifThenElseI8W8"
              , "ifThenElseI8W16"
              , "ifThenElseI8W32"
              , "ifThenElseI8I8"
              , "ifThenElseI8I16"
              , "ifThenElseI8I32"
              , "ifThenElseI8L8"
              , "ifThenElseI8Float"
              , "ifThenElseI16Unit"
              , "ifThenElseI16Bool"
              , "ifThenElseI16W8"
              , "ifThenElseI16W16"
              , "ifThenElseI16W32"
              , "ifThenElseI16I8"
              , "ifThenElseI16I16"
              , "ifThenElseI16I32"
              , "ifThenElseI16L8"
              , "ifThenElseI16Float"
              , "ifThenElseI32Unit"
              , "ifThenElseI32Bool"
              , "ifThenElseI32W8"
              , "ifThenElseI32W16"
              , "ifThenElseI32W32"
              , "ifThenElseI32I8"
              , "ifThenElseI32I16"
              , "ifThenElseI32I32"
              , "ifThenElseI32L8"
              , "ifThenElseI32Float"
              , "ifThenElseL8Unit"
              , "ifThenElseL8Bool"
              , "ifThenElseL8W8"
              , "ifThenElseL8W16"
              , "ifThenElseL8W32"
              , "ifThenElseL8I8"
              , "ifThenElseL8I16"
              , "ifThenElseL8I32"
              , "ifThenElseL8L8"
              , "ifThenElseL8Float"
              , "ifThenElseFloatUnit"
              , "ifThenElseFloatBool"
              , "ifThenElseFloatW8"
              , "ifThenElseFloatW16"
              , "ifThenElseFloatW32"
              , "ifThenElseFloatI8"
              , "ifThenElseFloatI16"
              , "ifThenElseFloatI32"
              , "ifThenElseFloatL8"
              , "ifThenElseFloatFloat"
              , "iterateUnitUnitE"
              , "iterateUnitBoolE"
              , "iterateUnitW8E"
              , "iterateUnitW16E"
              , "iterateUnitW32E"
              , "iterateUnitI8E"
              , "iterateUnitI16E"
              , "iterateUnitI32E"
              , "iterateUnitL8E"
              , "iterateUnitFloatE"
              , "iterateBoolUnitE"
              , "iterateBoolBoolE"
              , "iterateBoolW8E"
              , "iterateBoolW16E"
              , "iterateBoolW32E"
              , "iterateBoolI8E"
              , "iterateBoolI16E"
              , "iterateBoolI32E"
              , "iterateBoolL8E"
              , "iterateBoolFloatE"
              , "iterateW8UnitE"
              , "iterateW8BoolE"
              , "iterateW8W8E"
              , "iterateW8W16E"
              , "iterateW8W32E"
              , "iterateW8I8E"
              , "iterateW8I16E"
              , "iterateW8I32E"
              , "iterateW8L8E"
              , "iterateW8FloatE"
              , "iterateW16UnitE"
              , "iterateW16BoolE"
              , "iterateW16W8E"
              , "iterateW16W16E"
              , "iterateW16W32E"
              , "iterateW16I8E"
              , "iterateW16I16E"
              , "iterateW16I32E"
              , "iterateW16L8E"
              , "iterateW16FloatE"
              , "iterateW32UnitE"
              , "iterateW32BoolE"
              , "iterateW32W8E"
              , "iterateW32W16E"
              , "iterateW32W32E"
              , "iterateW32I8E"
              , "iterateW32I16E"
              , "iterateW32I32E"
              , "iterateW32L8E"
              , "iterateW32FloatE"
              , "iterateI8UnitE"
              , "iterateI8BoolE"
              , "iterateI8W8E"
              , "iterateI8W16E"
              , "iterateI8W32E"
              , "iterateI8I8E"
              , "iterateI8I16E"
              , "iterateI8I32E"
              , "iterateI8L8E"
              , "iterateI8FloatE"
              , "iterateI16UnitE"
              , "iterateI16BoolE"
              , "iterateI16W8E"
              , "iterateI16W16E"
              , "iterateI16W32E"
              , "iterateI16I8E"
              , "iterateI16I16E"
              , "iterateI16I32E"
              , "iterateI16L8E"
              , "iterateI16FloatE"
              , "iterateI32UnitE"
              , "iterateI32BoolE"
              , "iterateI32W8E"
              , "iterateI32W16E"
              , "iterateI32W32E"
              , "iterateI32I8E"
              , "iterateI32I16E"
              , "iterateI32I32E"
              , "iterateI32L8E"
              , "iterateI32FloatE"
              , "iterateL8UnitE"
              , "iterateL8BoolE"
              , "iterateL8W8E"
              , "iterateL8W16E"
              , "iterateL8W32E"
              , "iterateL8I8E"
              , "iterateL8I16E"
              , "iterateL8I32E"
              , "iterateL8L8E"
              , "iterateL8FloatE"
              , "iterateFloatUnitE"
              , "iterateFloatBoolE"
              , "iterateFloatW8E"
              , "iterateFloatW16E"
              , "iterateFloatW32E"
              , "iterateFloatI8E"
              , "iterateFloatI16E"
              , "iterateFloatI32E"
              , "iterateFloatL8E"
              , "iterateFloatFloatE"
              , "liftIO"
              , "debugE"
              , "debugListen"
              , "die"
              ]

data BindEnv
    = BindEnv
      { pluginModGuts   :: ModGuts,
        args            :: [CoreBndr],
        shallowDeepMap  :: M.Map Id Id,
        shallowDeepMaps :: [M.Map Id Id]
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
    (bindsL', s) <- (\x -> (runStateT (runBindM $ (mapM changeArgBind) x) (BindEnv guts [] M.empty []))) (mg_binds guts)
    let availNames = nameSetElems $ availsToNameSet $ mg_exports guts
    let shallowDeepMap' = M.filterWithKey (keyExported availNames) (shallowDeepMap s)
    let es = M.elems $ shallowDeepMap'
    let guts' = guts { mg_binds = concat bindsL', mg_exports = mg_exports guts ++ (map (Avail NotPatSyn)(map varName es)) }
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeAppBind) x) (BindEnv guts' [] (shallowDeepMap s) []))) guts'
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
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
          -- Calculate which args we should translate, only
          -- ones of our languages expression types
          xlats <- mapM isExprClassType argTys
          -- Change the binds and arg types to Expr a
          zipBsArgTys <- mapM changeArg (zip3 bs argTys xlats)
          let (bs', argTys') = unzip zipBsArgTys

          -- Put arg names to translate in body into state
          let (bs'', _) = unzip $ filter snd $ zip bs' xlats
          s <- get
          put s{args = bs''}

          -- Change any apps of the changed args in the body
          e'' <- changeArgAppsExpr e'

          -- Generate args for new shallow body
          deepArgs <- mapM repShallowArg $ zip (map Var bs) xlats

          -- Change return type
          exprTyCon <- thNameToTyCon exprTyConTH
          let exprTyConApp = mkTyConApp exprTyCon [retTy']

          -- Change the return
          e''' <- fmapRepBindReturn e''

          -- Create a new top level bind type with the deep tyep
          bDeep <- modId b deepSuffix
          let b' = setVarType bDeep $ mkFunTys argTys' (mkTyConApp retTyCon [exprTyConApp])

          -- Apply the abs <$> to the new shallow body
          let shallowE = mkCoreApps (Var b') deepArgs
          isExpr <- isExprClassType retTy'
          absE <- if isExpr
                  then fmapAbsExpr (mkTyConTy retTyCon) retTy' shallowE
                  else return shallowE

          -- Put id pair into state
          s <- get
          put s{shallowDeepMap = M.insert b b' (shallowDeepMap s)}

          return [(b, mkLams bs absE), (b', mkLams bs' e''')]
      _ -> return [(b, e)]

changeArg :: (CoreBndr, Type, Bool) -> BindM (CoreBndr, Type)
changeArg (b, ty, p) = do
  if p then do
    let tyCon_m = splitTyConApp_maybe ty
    case tyCon_m of
      Just (_, []) -> do
          -- Lookup the GHC type constructor of Expr
          exprTyCon <- thNameToTyCon exprTyConTH
          -- Make the type of the Expr for the specified type
          let ty' = mkTyConApp exprTyCon [ty]
          let b' = setVarType b ty'
          return (b', ty')
      _       -> return (b, ty)
  else return (b, ty)

repShallowArg :: (CoreExpr, Bool) -> BindM CoreExpr
repShallowArg (e, p) =
  if p then do
    e' <- repExpr e
    return e'
  else return e

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
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeArgAppsExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeArgAppsExpr' rbs
                    return $ Rec rbs'
      body' <- changeArgAppsExpr body
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

findDeepId :: Id -> BindM Id
findDeepId v = do
  s <- get
  let sdMap = shallowDeepMap s
  let sdMaps = shallowDeepMaps s
  if v `M.member` sdMap
    then return $ sdMap M.! v
    else case findDeepId' v sdMaps of
      Nothing -> stringToId $ (varString v) ++ deepSuffix
      Just v' -> return v'
  where
    findDeepId' :: Id -> [M.Map Id Id] -> Maybe Id
    findDeepId' v' []     = Nothing
    findDeepId' v' (m:ms) = if v `M.member` m then Just $ m M.! v else findDeepId' v' ms

changeAppExpr :: CoreExpr -> BindM CoreExpr
changeAppExpr e = do
  df <- liftCoreM getDynFlags
  monadTyConId <- thNameToTyCon monadTyConTH
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
          Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
              let exprtyCon_m = splitTyConApp_maybe retTy'
              case exprtyCon_m of
                  Just (exprTy, [exprTy']) -> defaultRet
                  _ -> do
                      v' <- findDeepId v
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
                vb' <- findDeepId vb
                -- Calculate which args we should translate, only
                -- ones of our languages expression types
                xlats <- mapM isExprClassType argTys
                args' <- mapM repShallowArg $ zip args xlats
                -- args' <- mapM repExpr args
                let e' = mkCoreApps (Var vb') args'
                isExpr <- isExprClassType retTy'
                absE <- if isExpr
                        then fmapAbsExpr (mkTyConTy retTyCon) retTy' e'
                        else return e'
                return $ absE
            _ -> defaultRet
    Lam tb e -> do
      e' <- changeAppExpr e
      return $ Lam tb e'
    Let bind body -> do
      put s {shallowDeepMap  = M.empty,
             shallowDeepMaps = shallowDeepMap s : shallowDeepMaps s}
      e' <- case bind of
              (NonRec lb le) -> do
                ides <- changeSubBind lb le
                body' <- changeAppExpr body
                case ides of
                  [(b1,e1),(b2,e2)] -> do
                    e2' <- changeAppExpr e2
                    -- ToDo:  Do we need to retain shallow vesion of
                    -- let expression?  It's never called after
                    -- transformation.
                    -- return $ Let (NonRec b2 e2') (Let (NonRec b1 e1) body')
                    return $ Let (NonRec b2 e2') body'
                  [(b1,e1)]         -> do
                    e1' <- changeAppExpr e1
                    return $ Let (NonRec b1 e1') body'
              (Rec rbs) -> do
                (nrbs', rbs') <- changeArgBind' rbs
                rbs'' <- changeAppExpr' rbs'
                body' <- changeAppExpr body
                case nrbs' of
                  []        -> return $ Let (Rec rbs'') body'
                  [NonRec b1 e1] -> do
                    e1' <- changeAppExpr e1
                    -- ToDo: Same as above, do we need shallow?
                    -- return $ Let (Rec rbs'') (Let (NonRec b1 e1') body')
                    return $ Let (Rec rbs'') body'
      s' <- get
      put s' {shallowDeepMap  = head $ shallowDeepMaps s',
              shallowDeepMaps = tail $ shallowDeepMaps s'}
      return e'
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
