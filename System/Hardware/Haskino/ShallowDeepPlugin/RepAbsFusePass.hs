-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Rep Abs Fusion Pass - Performs the equivelent of the following rules:
--    forall x.
--    rep_(abs_(x))
--      =
--    x
--
--    forall (m :: Arduino (Expr Bool)).
--    rep_ <$> (abs_ <$> m)
--      =
--    m
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass (repAbsFusePass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Data.List
import Control.Monad.Reader
import OccName
import Var

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import Data.Boolean
import System.Hardware.Haskino

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts
      }

newtype BindM a = BindM { runBindM :: ReaderT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader BindEnv)

instance PassCoreM BindM where
  liftCoreM = BindM . ReaderT . const
  getModGuts = BindM $ ReaderT (return . pluginModGuts)

repAbsFusePass :: ModGuts -> CoreM ModGuts
repAbsFusePass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeFuse) x) (BindEnv guts))) guts

changeFuse :: CoreBind -> BindM CoreBind
changeFuse bndr@(NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeFuseExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeFuse bndr@(Rec bs) = do
  bs' <- changeFuse' bs
  return $ Rec bs'

changeFuse' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeFuse' [] = return []
changeFuse' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- changeFuseExpr e'
  let e''' = mkLams lbs e''
  bs' <- changeFuse' bs
  return $ (b, e''') : bs'

changeFuseExpr :: CoreExpr -> BindM CoreExpr
changeFuseExpr e = do
  df <- liftCoreM getDynFlags
  repId <- thNameToId repNameTH
  absId <- thNameToId absNameTH
  fmapId <- thNameToId fmapNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    -- Look for case of rep_(abs_(x))
    (Var fv) :$ (Type _) :$ _ :$ ((Var fv') :$ (Type _) :$ e') | fv == repId && fv' == absId -> do
        e'' <- changeFuseExpr e'
        return e''
    -- Look for case of rep_ <$> (abs_ <$> m)
    (Var fv) :$ (Type _) :$ (Type _) :$ (Type _) :$ _ :$ ((Var fv1) :$ Type _ :$ _) :$ ((Var fv2) :$ (Type _) :$ (Type _) :$ (Type _) :$ _ :$ (Var fv3 :$ Type _) :$ e'' )| fv == fmapId && fv1 == repId && fv2 == fmapId && fv3 == absId -> do
        e''' <- changeFuseExpr e''
        return e'''
    App e1 e2 -> do
        e1' <- changeFuseExpr e1
        e2' <- changeFuseExpr e2
        return $ App e1' e2'
{-
    App e1 e2 -> do
        let (f, args) = collectArgs e
        let defaultReturn = do
--              args' <- mapM changeFuseExpr args
--              return $ mkCoreApps f args'
              e1' <- changeFuseExpr e1
              e2' <- changeFuseExpr e2
              return $ App e1' e2'
        case f of
          Var fv -> do
            if fv == repId
            -- Look for case of rep_(abs_(x))
            then do
              case args of
                 [Type _, _, e''] -> do
                    let (f', args') = collectArgs e''
                    case f' of
                      Var fv' -> do
                        if fv' == absId
                        then do
                          case args' of
                            [Type _, e'''] -> return e'''
                            _ -> defaultReturn
                        else defaultReturn
                      _ -> defaultReturn
                 _ -> defaultReturn
            else if fv == fmapId
              -- Look for case of rep_ <$> (abs_ <$> m)
              then do
                case args of
                    [Type _, Type _, Type _, _, e1, e2] -> do
                      liftCoreM $ putMsgS "%%%%%%%%%%%%%"
                      let (f1, args1) = collectArgs e1
                      let (f2, args2) = collectArgs e2
                      case f1 of
                        Var f1v -> do
                          liftCoreM $ putMsg $ ppr f1v
                          case f2 of
                            Var f2v -> do
                              liftCoreM $ putMsg $ ppr f2v
                              if f1v == repId && f2v == fmapId
                              then do
                                case args2 of
                                  [Type _, Type _, Type _, _, e3, e4] -> do
                                    let (f3, _) = collectArgs e3
                                    case f3 of
                                      Var f3v -> do
                                        liftCoreM $ putMsg $ ppr f3v
                                        if f3v == absId
                                        then do
                                          liftCoreM $ putMsgS "Got here"
                                          return e4
                                        else defaultReturn
                                      _ -> defaultReturn
                              else defaultReturn
                            _ -> defaultReturn
                        _ -> defaultReturn
                    _ -> defaultReturn
              else defaultReturn
          _ -> defaultReturn
-}
    Lam tb e -> do
      e' <- changeFuseExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeFuseExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeFuseExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeFuseExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeFuseExpr e
      alts' <- changeFuseExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeFuseExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeFuseExpr e
      return $ Cast e' co

changeFuseExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeFuseExpr' [] = return []
changeFuseExpr' ((b, e) : bs) = do
  e' <- changeFuseExpr e
  bs' <- changeFuseExpr' bs
  return $ (b, e') : bs'

changeFuseExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeFuseExprAlts [] = return []
changeFuseExprAlts ((ac, b, a) : as) = do
  a' <- changeFuseExpr a
  bs' <- changeFuseExprAlts as
  return $ (ac, b, a') : bs'
