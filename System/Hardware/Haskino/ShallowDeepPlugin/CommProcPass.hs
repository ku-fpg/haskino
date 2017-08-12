-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.CommProcPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.CommProcPass (commProcPass) where

import CoreMonad
import GhcPlugins
import Type
import Data.List
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import qualified System.Hardware.Haskino

data XlatEntry = XlatEntry {  fromId   :: BindM Id
                            , toId     :: BindM Id
                           }

-- The following talbe defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (thNameToId 'System.Hardware.Haskino.setPinMode)
                        (thNameToId 'System.Hardware.Haskino.setPinModeE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalWrite)
                        (thNameToId 'System.Hardware.Haskino.digitalWriteE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalPortWrite)
                        (thNameToId 'System.Hardware.Haskino.digitalPortWriteE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.analogWrite)
                        (thNameToId 'System.Hardware.Haskino.analogWriteE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.tone)
                        (thNameToId 'System.Hardware.Haskino.toneE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.noTone)
                        (thNameToId 'System.Hardware.Haskino.noToneE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.i2cWrite)
                        (thNameToId 'System.Hardware.Haskino.i2cWriteE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoDetach)
                        (thNameToId 'System.Hardware.Haskino.servoDetachE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.stepperSetSpeed)
                        (thNameToId 'System.Hardware.Haskino.stepperSetSpeedE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoWrite)
                        (thNameToId 'System.Hardware.Haskino.servoWriteE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoWriteMicros)
                        (thNameToId 'System.Hardware.Haskino.servoWriteMicrosE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.createTask)
                        (thNameToId 'System.Hardware.Haskino.createTaskE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.deleteTask)
                        (thNameToId 'System.Hardware.Haskino.deleteTaskE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.scheduleTask)
                        (thNameToId 'System.Hardware.Haskino.scheduleTaskE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.attachInt)
                        (thNameToId 'System.Hardware.Haskino.attachIntE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.detachInt)
                        (thNameToId 'System.Hardware.Haskino.detachIntE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.giveSem)
                        (thNameToId 'System.Hardware.Haskino.giveSemE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.takeSem)
                        (thNameToId 'System.Hardware.Haskino.takeSemE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.loop)
                        (thNameToId 'System.Hardware.Haskino.loopE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.queryFirmware)
                        (thNameToId 'System.Hardware.Haskino.queryFirmwareE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.queryProcessor)
                        (thNameToId 'System.Hardware.Haskino.queryProcessorE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.micros)
                        (thNameToId 'System.Hardware.Haskino.microsE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.millis)
                        (thNameToId 'System.Hardware.Haskino.millisE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.delayMillis)
                        (thNameToId 'System.Hardware.Haskino.delayMillisE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.delayMicros)
                        (thNameToId 'System.Hardware.Haskino.delayMicrosE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalRead)
                        (thNameToId 'System.Hardware.Haskino.digitalReadE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalPortRead)
                        (thNameToId 'System.Hardware.Haskino.digitalPortReadE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.analogRead)
                        (thNameToId 'System.Hardware.Haskino.analogReadE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.i2cRead)
                        (thNameToId 'System.Hardware.Haskino.i2cReadE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.stepper2Pin)
                        (thNameToId 'System.Hardware.Haskino.stepper2PinE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.stepper4Pin)
                        (thNameToId 'System.Hardware.Haskino.stepper4PinE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.stepperStepE)
                        (thNameToId 'System.Hardware.Haskino.stepperStepE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoAttach)
                        (thNameToId 'System.Hardware.Haskino.servoAttachE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoAttachMinMax)
                        (thNameToId 'System.Hardware.Haskino.servoAttachMinMaxE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoReadE)
                        (thNameToId 'System.Hardware.Haskino.servoReadE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.servoReadMicrosE)
                        (thNameToId 'System.Hardware.Haskino.servoReadMicrosE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.queryAllTasks)
                        (thNameToId 'System.Hardware.Haskino.queryAllTasksE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.queryTask)
                        (thNameToId 'System.Hardware.Haskino.queryTaskE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.bootTaskE)
                        (thNameToId 'System.Hardware.Haskino.bootTaskE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.debug)
                        (thNameToId 'System.Hardware.Haskino.debugE)
            , XlatEntry (thNameToId 'System.Hardware.Haskino.compileProgram)
                        (thNameToId 'System.Hardware.Haskino.compileProgramE)
           ]

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts
      }

newtype BindM a = BindM { runCondM :: ReaderT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader BindEnv)

instance PassCoreM BindM where
  liftCoreM = BindM . ReaderT . const
  getModGuts = BindM $ ReaderT (return . pluginModGuts)

commProcPass :: ModGuts -> CoreM ModGuts
commProcPass guts = do
    bindsOnlyPass (\x -> (runReaderT (runCondM $ (mapM commProcBind) x) (BindEnv guts))) guts

commProcBind :: CoreBind -> BindM CoreBind
commProcBind bndr@(NonRec b e) = do
  e' <- commProcExpr e
  return (NonRec b e')
commProcBind (Rec bs) = do
  bs' <- commProcExpr' bs
  return $ Rec bs'

funcInXlatList :: Id -> BindM (Maybe XlatEntry)
funcInXlatList id = do
  funcInXlatList' id xlatList
    where
      funcInXlatList' :: Id -> [XlatEntry] -> BindM (Maybe XlatEntry)
      funcInXlatList' id [] = return Nothing
      funcInXlatList' id (xl:xls) = do
          fId <- fromId xl
          if fId == id
          then return $ Just xl
          else funcInXlatList' id xls

commProcExpr :: CoreExpr -> BindM CoreExpr
commProcExpr e = do
  apId <- thNameToId apNameTH
  df <- liftCoreM getDynFlags
  case e of
    Var v -> do
      inList <- funcInXlatList v
      case inList of
          Just xe -> do
            v' <- toId xe
            return $ Var v'
          Nothing -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (f, args) = collectArgs e
      let defaultReturn = do
              e1' <- commProcExpr e1
              e2' <- commProcExpr e2
              return $ App e1' e2'
      case f of
          Var v -> do
              inList <- funcInXlatList v
              case inList of
                  Just xe -> commProcXlat xe e
                  Nothing -> defaultReturn
          _ -> defaultReturn
    Lam tb e -> do
      e' <- commProcExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- commProcExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- commProcExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- commProcExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- commProcExpr e
      alts' <- commProcExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- commProcExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- commProcExpr e
      return $ Cast e' co

commProcExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
commProcExpr' [] = return []
commProcExpr' ((b, e) : bs) = do
  e' <- commProcExpr e
  bs' <- commProcExpr' bs
  return $ (b, e') : bs'

commProcExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
commProcExprAlts [] = return []
commProcExprAlts ((ac, b, a) : as) = do
  a' <- commProcExpr a
  bs' <- commProcExprAlts as
  return $ (ac, b, a') : bs'

commProcXlat :: XlatEntry -> CoreExpr -> BindM CoreExpr
commProcXlat xe e = do
  let (f, args) = collectArgs e
  (xlatRet, xlatArgs) <- genXlatBools (fromId xe) (toId xe)
  let zargs = zip xlatArgs args
  args' <- mapM commProcXlatArg zargs
  newId <- toId xe
  let f' = Var newId
  if xlatRet
  then do
    -- ToDo: Handle Error cases using _maybe function varients.
    let (argTys, retTy) = splitFunTys $ exprType e
    let (tyCon, [ty]) = splitTyConApp retTy
    let e' = mkCoreApps f' args'
    fmapAbsExpr (mkTyConTy tyCon) ty e'
  else
    return $ mkCoreApps f' args'

commProcXlatArg :: (Bool, CoreExpr) -> BindM CoreExpr
commProcXlatArg (xlat, e) =
  if xlat
  then do
    let tyCon_m = splitTyConApp_maybe $ exprType e
    case tyCon_m of
      Just (tyCon, [ty]) -> do
        e' <- commProcExpr e
        fmapAbsExpr (mkTyConTy tyCon) ty e'
      _                  -> repExpr e
  else return e

genXlatBools :: BindM Id -> BindM Id -> BindM (Bool, [Bool])
genXlatBools from to = do
  f <- from
  t <- to
  let (fTys, fRetTy) = splitFunTys $ exprType $ Var f
  let (tTys, tRetTy) = splitFunTys $ exprType $ Var t
  let zTys = zip fTys tTys
  let changeArgs = map (\(x,y) -> not $ x `eqType` y) zTys
  return $ (not $ fRetTy `eqType` tRetTy, changeArgs)

