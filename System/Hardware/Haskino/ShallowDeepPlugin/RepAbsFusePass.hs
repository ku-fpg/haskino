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
changeFuse bndr@(Rec bs) = return bndr 

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
    App e1 e2 -> do
        let (f, args) = collectArgs e
        let defaultReturn = do
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
                      let (f1, args1) = collectArgs e1
                      let (f2, args2) = collectArgs e2
                      case f1 of
                        Var f1v -> do
                          case f2 of
                            Var f2v -> do
                              if f1v == repId && f2v == fmapId
                              then do
                                case args2 of
                                  [Type _, Type _, Type _, _, e3, e4] -> do
                                    let (f3, _) = collectArgs e3
                                    case f3 of
                                      Var f3v -> do
                                        if f3v == absId
                                        then return e4
                                        else defaultReturn
                                      _ -> defaultReturn
                              else defaultReturn
                            _ -> defaultReturn
                        _ -> defaultReturn
                    _ -> defaultReturn
              else defaultReturn
          _ -> defaultReturn
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
