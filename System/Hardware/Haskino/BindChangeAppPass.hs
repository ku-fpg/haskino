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

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        args :: [CoreBndr]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
  liftCoreM m = BindM $ lift m
  getModGuts = gets pluginModGuts

bindChangeAppPass :: ModGuts -> CoreM ModGuts
bindChangeAppPass guts =
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM changeBind) x) (BindEnv guts []))) guts

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
      case tyCon_m of
          Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") -> do
              if showSDoc df (ppr b) == "myRead1" || showSDoc df (ppr b) == "myWrite"
              then do
                args' <- mapM changeAppArg args             
                if showSDoc df (ppr b) == "myRead1" 
                then do
                  liftCoreM $ putMsg $ ppr retTy'
                  -- Get the a type from Expr a
                  case splitTyConApp_maybe retTy' of
                     Just(_, [retTy'']) -> do
                        -- Get the monad type from the type constructor
                        let retTyConTy = mkTyConTy retTyCon

                        functId <- thNameToId '(<$>)
                        functTyCon <- thNameToTyCon ''Data.Functor.Functor
                        functDict <- buildDictionaryTyConT functTyCon retTyConTy

                        absId <- thNameToId 'System.Hardware.Haskino.abs_

                        -- Rebuild the original nested app                 
                        let e' = mkCoreApps b args'
                        -- Build the abs_ function
                        let abs = App (Var absId) (Type retTy'')
                        -- Build the <$> applied to the abs_ and the original app
                        return $ mkCoreApps (Var functId) [Type retTyConTy, Type retTy', Type retTy'', functDict, abs, e']
                     _ -> return $ mkCoreApps b args'
                else return $ mkCoreApps b args'
              else do
                  e1' <- changeAppExpr e1
                  e2' <- changeAppExpr e2
                  return $ App e1' e2'
          _ -> do
              e1' <- changeAppExpr e1
              e2' <- changeAppExpr e2
              return $ App e1' e2'
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
  -- Lookup the GHC ID of rep_ function
  Just repName <- liftCoreM $ thNameToGhcName 'System.Hardware.Haskino.rep_
  repId <- liftCoreM $ lookupId repName
  -- Lookup the GHC type constructor of ExprB
  Just exprBName <- liftCoreM $ thNameToGhcName ''System.Hardware.Haskino.ExprB
  exprBTyCon <- liftCoreM $ lookupTyCon exprBName
  -- Make the type of the ExprB for the specified type
  let repTyConApp = GhcPlugins.mkTyConApp exprBTyCon [ty]
  -- Build the ExprB dictionary argument to apply
  repDict <- buildDictionaryT repTyConApp
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
