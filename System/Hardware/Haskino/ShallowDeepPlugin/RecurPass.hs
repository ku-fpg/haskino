-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RecurPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Recursion Transformation Pass
-- if b then t else e ==> ifThenElse[Unit]E (rep b) t e
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RecurPass (recurPass) where

import CoreMonad
import GhcPlugins
import Data.List
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

data BindEnv
    = BindEnv
      { pluginModGuts :: ModGuts,
        funcId        :: [Id]
      }

newtype BindM a = BindM { runBindM :: StateT BindEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadState BindEnv)

instance PassCoreM BindM where
    liftCoreM m = BindM $ lift m
    getModGuts = gets pluginModGuts

recurPass :: ModGuts -> CoreM ModGuts
recurPass guts = do
    bindsOnlyPass (\x -> fst <$> (runStateT (runBindM $ (mapM recurBind) x) (BindEnv guts []))) guts

recurBind :: CoreBind -> BindM CoreBind
recurBind bndr@(NonRec b e) = return bndr
recurBind (Rec bs) = do
  bs' <- recurBind' bs
  return $ Rec bs'

recurBind' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
recurBind' [] = return []
recurBind' ((b, e) : bs) = do
  s <- get
  put s {funcId = [b]}
  liftCoreM $ putMsgS "Rec Bind"
  liftCoreM $ putMsg $ ppr b
  liftCoreM $ putMsg $ ppr $ exprType e
  (_, hasAp) <- checkForRecur b e 
  bs' <- recurBind' bs
  return $ (b, e) : bs'

checkForRecur :: Id -> CoreExpr -> BindM (CoreExpr, Bool)
checkForRecur id e = do
    funcId <- gets funcId
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    bindId <- thNameToId '(>>=)
    thenId <- thNameToId '(>>)
    fmapId <- thNameToId '(<$>)
    apId <- thNameToId '($)
    case f of
      Var fv -> do
        -- Check if we have reached the bottom of the bind chain or if 
        -- there is another level.
        if fv == bindId || fv == thenId
        then do
            -- Check if the next level has an recur
            (e'', absFlag) <- checkForRecur id $ last args
            let e''' = mkCoreApps f args        
            return $ (mkLams bs e''', absFlag)
        else 
            -- We are at the bottom of the bind chain.....
            -- Check for recursive call.
            -- Either in the form of (Var funcId) $ arg ...
            if fv == head funcId 
            then do
                liftCoreM $ putMsg $ ppr fv
                liftCoreM $ putMsg $ ppr args
                return (e, True)
            else if fv == apId
                 then case args of
                     [_, _, _, Var fv', _] | fv' == head funcId -> return (e, True)
                     _                                          -> return (e, False) 
                 else return (e, False)
      Case e' tb ty alts -> do
        liftCoreM $ putMsgS "Found Case"
        alts' <- checkAltsForRecur alts
        return (Case e' tb ty alts', False)
      _ -> return (e, False)


checkAltsForRecur :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
checkAltsForRecur [] = return []
checkAltsForRecur ((ac, b, a) : as) = do
  funcId <- gets funcId
  (_, hasAp) <- checkForRecur (head funcId) a 
  liftCoreM $ putMsgS "#######"
  liftCoreM $ putMsg $ ppr hasAp
  liftCoreM $ putMsg $ ppr ac
  liftCoreM $ putMsg $ ppr b
  bs' <- checkAltsForRecur as
  return $ (ac, b, a) : bs'

