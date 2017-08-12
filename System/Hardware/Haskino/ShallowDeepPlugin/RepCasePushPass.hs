-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.repCasePushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Push rep through case alternatives
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepCasePushPass (repCasePushPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader
import Var

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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

repCasePushPass :: ModGuts -> CoreM ModGuts
repCasePushPass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM repCasePush) x) (BindEnv guts))) guts

repCasePush :: CoreBind -> BindM CoreBind
repCasePush bndr@(NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- repCasePushExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
repCasePush bndr@(Rec bs) = do
  bs' <- repCasePush' bs
  return $ Rec bs'

repCasePush' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
repCasePush' [] = return []
repCasePush' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- repCasePushExpr e'
  let e''' = mkLams lbs e''
  bs' <- repCasePush' bs
  return $ (b, e''') : bs'

repCasePushExpr :: CoreExpr -> BindM CoreExpr
repCasePushExpr e = do
  df <- liftCoreM getDynFlags
  thenId <- thNameToId bindThenNameTH
  fmapId <- thNameToId fmapNameTH
  absId  <- thNameToId absNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    -- Look for expressions of the form:
    -- rep (Case )
    (Var repV) :$ (Type rTy) :$ dict :$ e1 -> do
      e1' <- repCasePushExpr e1
      let (ls,e1'') = collectLets e1'
      case e1'' of
        (Case ce ctb cty calts) -> do
          calts' <- mapM (repCasePushAlt cty) calts
          cty' <- thNameTyToTyConApp exprTyConTH cty
          return $ mkLets ls $ Case ce ctb cty' calts'
        _ -> return ( (Var repV) :$ (Type rTy) :$ dict :$ e1' )
    App e1 e2 -> do
      e1' <- repCasePushExpr e1
      e2' <- repCasePushExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- repCasePushExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- repCasePushExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- repCasePushExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- repCasePushExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- repCasePushExpr e
      alts' <- repCasePushExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- repCasePushExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- repCasePushExpr e
      return $ Cast e' co

repCasePushAlt :: Type -> GhcPlugins.Alt CoreBndr -> BindM (GhcPlugins.Alt CoreBndr)
repCasePushAlt ty (ac, b, a) = do
  a' <- repCasePushExpr a
  repId <- thNameToId repNameTH
  repDict <- thNameTyToDict exprClassTyConTH ty
  let repE = mkCoreApps (Var repId) [Type ty, repDict, a']
  liftCoreM $ putMsgS "==============="
  liftCoreM $ putMsg $ ppr a
  liftCoreM $ putMsgS "---------------"
  liftCoreM $ putMsg $ ppr repE
  return (ac, b, repE)

repCasePushExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
repCasePushExpr' [] = return []
repCasePushExpr' ((b, e) : bs) = do
  e' <- repCasePushExpr e
  bs' <- repCasePushExpr' bs
  return $ (b, e') : bs'

repCasePushExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
repCasePushExprAlts [] = return []
repCasePushExprAlts ((ac, b, a) : as) = do
  a' <- repCasePushExpr a
  bs' <- repCasePushExprAlts as
  return $ (ac, b, a') : bs'

