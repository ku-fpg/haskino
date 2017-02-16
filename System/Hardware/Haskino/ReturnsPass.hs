-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ReturnsPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- return express transformation pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ReturnsPass (returnsPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader
import Var

import System.Hardware.Haskino.Dictionary (buildDictionaryT, 
                                           buildDictionaryTyConT, 
                                           PassCoreM(..), 
                                           thNameToId, thNameToTyCon)

import qualified System.Hardware.Haskino
import qualified System.Hardware.Haskino.Expr

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

returnsPass :: ModGuts -> CoreM ModGuts
returnsPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeBind) x) (BindEnv guts))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (bs, e') = collectBinders e
  e'' <- changeRetExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeBind (Rec bs) = do
  return $ Rec bs

changeRetExpr :: CoreExpr -> BindM CoreExpr
changeRetExpr e = do
  df <- liftCoreM getDynFlags
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (b, args) = collectArgs e
      if (showSDoc df (ppr b) == "return") && (showSDoc df (ppr $ head args) == "TYPE: Arduino")
      then do
          e' <- changeReturn e
          return e'
      else do
          e1' <- changeRetExpr e1
          e2' <- changeRetExpr e2
          return $ App e1' e2'
    Lam tb e -> do
      e' <- changeRetExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeRetExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeRetExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeRetExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeRetExpr e
      alts' <- changeRetExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeRetExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeRetExpr e
      return $ Cast e' co

varString :: Id -> String
varString = occNameString . nameOccName . Var.varName

changeRetExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeRetExpr' [] = return []
changeRetExpr' ((b, e) : bs) = do
  e' <- changeRetExpr e
  bs' <- changeRetExpr' bs
  return $ (b, e') : bs'

changeRetExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeRetExprAlts [] = return []
changeRetExprAlts ((ac, b, a) : as) = do
  a' <- changeRetExpr a
  bs' <- changeRetExprAlts as
  return $ (ac, b, a') : bs'

{-
  The following performs this transform:

    forall (ExprB a => e :: a) 
    return e
      =
    abs_ <$> (return (rep_ e))

-}
changeReturn :: CoreExpr -> BindM CoreExpr
changeReturn e = do
    let (f, args) = collectArgs e
    liftCoreM $ putMsg $ ppr args
    case args of
      [Type ty1, Var d, Type ty2, ex] -> do
          repId <- thNameToId 'System.Hardware.Haskino.rep_
          exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
          repDict <- buildDictionaryTyConT exprBTyCon ty2

          let retArg = mkCoreApps (Var repId) [Type ty2, repDict, ex]

          exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
          let retTyConApp = mkTyConApp exprTyCon [ty2]

          let f' = mkCoreApps f [Type ty1, Var d, Type retTyConApp, retArg]

          functId <- thNameToId '(<$>)
          functTyCon <- thNameToTyCon ''Data.Functor.Functor
          functDict <- buildDictionaryTyConT functTyCon ty1

          absId <- thNameToId 'System.Hardware.Haskino.abs_
          let absLamba = mkCoreApps (Var absId ) [Type ty2]

          let retExp = mkCoreApps (Var functId) [Type ty1, Type retTyConApp, Type ty2, functDict, absLamba, f']
          return retExp
      _ -> return e
