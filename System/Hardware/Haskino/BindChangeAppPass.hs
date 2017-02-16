-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangeAppPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangeAppPass (bindChangeAppPass) where

import CoreMonad
import GhcPlugins
import Var
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.Dictionary (buildDictionaryT,
                                           buildDictionaryTyConT,
                                           PassCoreM(..),
                                           thNameToId, thNameToTyCon)

import qualified System.Hardware.Haskino
import qualified System.Hardware.Haskino.Expr

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        chngRet  :: [CoreBndr],
        chngArgs :: [CoreBndr]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
  liftCoreM m = BindM $ lift m
  getModGuts = gets pluginModGuts

bindChangeAppPass :: ModGuts -> CoreM ModGuts
bindChangeAppPass guts = do
    (cmds, procs) <- findCmdsProcs $ mg_binds guts
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeBind) x) (BindEnv guts procs (cmds ++ procs)))) guts

findCmdsProcs :: [CoreBind] -> CoreM ([CoreBndr],[CoreBndr])
findCmdsProcs [] = return ([], [])
findCmdsProcs (bndr@(NonRec b e) : bndrs) = do
    df <- getDynFlags
    let (argTys, retTy) = splitFunTys $ varType b
    let tyCon_m = splitTyConApp_maybe retTy
    (Just monadTyConName) <- thNameToGhcName ''System.Hardware.Haskino.Arduino
    monadTyConId <- lookupTyCon monadTyConName
    case tyCon_m of
        -- We are looking for return types of Arduino a
        -- Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") &&
        Just (retTyCon, [retTy']) | retTyCon == monadTyConId &&
                                    (showSDoc df (ppr retTy') == "()") -> do
            (cmds, procs) <- findCmdsProcs bndrs
            return (b:cmds, procs)
        Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") -> do
          let tyCon_m' = splitTyConApp_maybe retTy'
          case tyCon_m' of
              Just (retTyCon', [retTy'']) -> do
                  (cmds, procs) <- findCmdsProcs bndrs
                  return (cmds, b:procs)
              _ -> findCmdsProcs bndrs
        _ -> findCmdsProcs bndrs
findCmdsProcs ((Rec bs) : bndrs) = findCmdsProcs bndrs

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (bs, e') = collectBinders e
  e'' <- changeAppExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeBind (Rec bs) = do
  return $ Rec bs

changeAppExpr :: CoreExpr -> BindM CoreExpr
changeAppExpr e = do
  df <- liftCoreM getDynFlags
  chngRet <- gets chngRet
  chngArgs <- gets chngArgs
  s <- get
  case e of
    Var v -> return $ Var v
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
      case tyCon_m of
          Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") -> do
              let (Var vb) = b
              if vb `elem` chngArgs
              then do
                  args' <- mapM changeAppArg args
                  if vb `elem` chngRet
                  then do
                      let retTyConTy = mkTyConTy retTyCon

                      functId <- thNameToId '(<$>)
                      functTyCon <- thNameToTyCon ''Data.Functor.Functor
                      functDict <- buildDictionaryTyConT functTyCon retTyConTy

                      exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
                      let exprTyConApp = mkTyConApp exprTyCon [retTy']

                      absId <- thNameToId 'System.Hardware.Haskino.abs_

                      -- Rebuild the original nested app
                      let e' = mkCoreApps b args'
                      -- Build the abs_ function
                      let abs = App (Var absId) (Type retTy')
                      -- Build the <$> applied to the abs_ and the original app
                      return $ mkCoreApps (Var functId) [Type retTyConTy, Type exprTyConApp, Type retTy', functDict, abs, e']
                  else return $ mkCoreApps b args'
              else defaultRet
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

changeAppArg :: CoreExpr -> BindM CoreExpr
changeAppArg e = do
    let ty = exprType e
    repId <- thNameToId 'System.Hardware.Haskino.rep_
    exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
    repDict <- buildDictionaryTyConT exprBTyCon ty
    return $ mkCoreApps (Var repId) [Type ty, repDict, e]

varString :: Id -> String
varString = occNameString . nameOccName . Var.varName

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
