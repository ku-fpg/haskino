-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.CommProcPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.CommProcPass (commProcPass) where

import CoreMonad
import GhcPlugins
import Data.List
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.Dictionary (buildDictionaryT,
                                           buildDictionaryTyConT,
                                           PassCoreM(..),
                                           thNameToId, thNameToTyCon)

import qualified System.Hardware.Haskino
import qualified System.Hardware.Haskino.Data
import qualified System.Hardware.Haskino.Expr

data XlatEntry = XlatEntry {  fromId   :: BindM Id
                            , toId     :: BindM Id
                            , xlatRet  :: Bool
                            , xlatArgs :: [Bool]
                           }

xlatList :: [XlatEntry]
xlatList = [  XlatEntry (thNameToId 'System.Hardware.Haskino.loop)
                        (thNameToId 'System.Hardware.Haskino.loopE)
                        False []
            , XlatEntry (thNameToId 'System.Hardware.Haskino.setPinMode)
                        (thNameToId 'System.Hardware.Haskino.setPinModeE)
                        False [True, False]
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalRead)
                        (thNameToId 'System.Hardware.Haskino.digitalReadE)
                        True  [True]
            , XlatEntry (thNameToId 'System.Hardware.Haskino.digitalWrite)
                        (thNameToId 'System.Hardware.Haskino.digitalWriteE)
                        False [True, True]
            , XlatEntry (thNameToId 'System.Hardware.Haskino.delayMillis)
                        (thNameToId 'System.Hardware.Haskino.delayMillisE)
                        False [True]
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
      case f of
          Var v -> do
              inList <- funcInXlatList v
              case inList of
                  Just xe -> commProcXlat xe e
                  Nothing -> do
                      e1' <- commProcExpr e1
                      e2' <- commProcExpr e2
                      return $ App e1' e2'
          _ -> do
              e1' <- commProcExpr e1
              e2' <- commProcExpr e2
              return $ App e1' e2'
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

nameString :: Name -> String
nameString = occNameString . nameOccName

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
  let zargs = zip (xlatArgs xe) args
  args' <- mapM commProcXlatArg zargs
  newId <- toId xe
  let f' = Var newId

  if xlatRet xe
  then do
    let (tyCon, [ty]) = splitTyConApp $ exprType e
    let tyConTy = mkTyConTy tyCon

    exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
    let exprTy = mkTyConApp exprTyCon [ty]

    functId <- thNameToId '(<$>)
    functTyCon <- thNameToTyCon ''Data.Functor.Functor
    functDict <- buildDictionaryTyConT functTyCon tyConTy

    -- Build the abs_ function
    absId <- thNameToId 'System.Hardware.Haskino.abs_

    let abs = App (Var absId) (Type ty)
    -- Build the <$> applied to the abs_ and the original app
    return $ mkCoreApps (Var functId) [Type tyConTy, Type exprTy, Type ty, functDict, abs, mkCoreApps f' args']
  else
    return $ mkCoreApps f' args'

commProcXlatArg :: (Bool, CoreExpr) -> BindM CoreExpr
commProcXlatArg (xlat, e) =
  if xlat
  then do
    let ty = exprType e
    repId <- thNameToId 'System.Hardware.Haskino.rep_
    exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
    repDict <- buildDictionaryTyConT exprBTyCon ty
    return $ mkCoreApps (Var repId) [Type ty, repDict, e]
  else return e

