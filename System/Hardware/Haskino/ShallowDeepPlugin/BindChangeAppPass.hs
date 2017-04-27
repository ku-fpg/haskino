-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeAppPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind call site type change pass:
-- forall (f: a -> .. b -> Arduino a).
-- f aa .. ab
--   =
-- abs_ <$> f (rep aa) .. (rep ab)
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeAppPass (bindChangeAppPass) where

import CoreMonad
import GhcPlugins
import Var
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeAppBind) x) (BindEnv guts procs (cmds ++ procs)))) guts

findCmdsProcs :: [CoreBind] -> CoreM ([CoreBndr],[CoreBndr])
findCmdsProcs [] = return ([], [])
findCmdsProcs (bndr@(NonRec b e) : bndrs) = do
    (cmds, procs) <- findCmdsProcs bndrs
    (c, p) <- findCmdsProcsInBind b e
    return (c ++ cmds, p ++ procs)
findCmdsProcs ((Rec bs) : bndrs) = do
    (cmds, procs) <- findCmdsProcs bndrs
    (c, p) <- findCmdsProcs' bs
    return (c ++ cmds, p ++ procs)

findCmdsProcs' :: [(Id, CoreExpr)] -> CoreM ([CoreBndr],[CoreBndr])
findCmdsProcs' [] = return ([], [])
findCmdsProcs' ((b, e) : bs) = do
    (cmds, procs) <- findCmdsProcs' bs
    (c, p) <- findCmdsProcsInBind b e
    return (c ++ cmds, p ++ procs)

findCmdsProcsInBind :: Id -> CoreExpr -> CoreM ([CoreBndr],[CoreBndr])
findCmdsProcsInBind b e = do
    df <- getDynFlags
    let (argTys, retTy) = splitFunTys $ varType b
    let tyCon_m = splitTyConApp_maybe retTy
    (Just monadTyConName) <- thNameToGhcName monadTyConTH
    monadTyCon <- lookupTyCon monadTyConName
    (Just unitTyConName) <- thNameToGhcName unitTyConTH
    unitTyCon <- lookupTyCon unitTyConName
    let unitTyConTy = mkTyConTy unitTyCon
    case tyCon_m of
        -- We are looking for return types of Arduino a
        Just (retTyCon, [retTy']) | retTyCon == monadTyCon &&
                                    retTy' `eqType` unitTyConTy ->
            return ([b],[])
        Just (retTyCon, [retTy']) | retTyCon == monadTyCon -> do
          let tyCon_m' = splitTyConApp_maybe retTy'
          case tyCon_m' of
              Just (retTyCon', [retTy'']) -> return ([], [b])
              _ -> return ([], [])
        _ -> return ([], [])

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
      monadTyConId <- thNameToTyCon monadTyConTH
      case tyCon_m of
          Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
              let (Var vb) = b
              if vb `elem` chngArgs
              then do
                  args' <- mapM repExpr args
                  if vb `elem` chngRet
                  then do
                      -- Rebuild the original nested app
                      let e' = mkCoreApps b args'
                      absExpr <- fmapAbsExpr (mkTyConTy retTyCon) retTy' e'
                      return $ absExpr
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
