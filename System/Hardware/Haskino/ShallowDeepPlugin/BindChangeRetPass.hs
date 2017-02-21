-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeRetPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind return type change pass
-- f :: a -> ... -> c -> d ==> f :: a  -> ... -> c -> Expr d
-- It does this by inserting a rep_ <$> to the last expresion of the bind
-- chain for the local bind.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeRetPass (bindChangeRetPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.ShallowDeepPlugin.Dictionary (buildDictionaryT, 
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

bindChangeRetPass :: ModGuts -> CoreM ModGuts
bindChangeRetPass guts = 
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeBind) x) (BindEnv guts))) guts

changeBind :: CoreBind -> BindM CoreBind
changeBind bndr@(NonRec b e) = do
    df <- liftCoreM getDynFlags
    let (argTys, retTy) = splitFunTys $ varType b
    let tyCon_m = splitTyConApp_maybe retTy
    case tyCon_m of
        -- We are looking for return types of Arduino a
        Just (retTyCon, [retTy']) | (showSDoc df (ppr retTyCon) == "Arduino") &&
                                    (showSDoc df (ppr retTy') /= "()") -> do
            let tyCon_m' = splitTyConApp_maybe retTy'
            case tyCon_m' of
                -- We do not want types of Arduino (Expr a), so we look for an
                -- empty list of types, and just a TyCon from the tyCon_m'.
                Just (retTyCon', []) -> do
                    let (bs, e') = collectBinders e

                    exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
                    let exprTyConApp = mkTyConApp exprTyCon [retTy']

                    -- Change the return
                    e'' <- changeReturn e'

                    -- Change binding type
                    let b' = setVarType b $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])
                    return (NonRec b' (mkCoreLams bs e''))
                _ -> return bndr
        _ -> return bndr
changeBind (Rec bs) = do
    return $ Rec bs

changeReturn :: CoreExpr -> BindM CoreExpr
changeReturn e = do
    df <- liftCoreM getDynFlags
    let (bs, e') = collectBinders e
    let (f, args) = collectArgs e'
    if (showSDoc df (ppr f) == ">>=") || (showSDoc df (ppr f) == ">>")
    then do
        la' <- changeReturn $ last args
        let args' = init args ++ [la']
        return $ mkCoreApps f args'
    else do
        let ty = exprType e'
        let Just tyCon'  = tyConAppTyCon_maybe ty
        let ty' = mkTyConTy tyCon'
        let Just [ty''] = tyConAppArgs_maybe ty 

        repId <- thNameToId 'System.Hardware.Haskino.rep_
        exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
        repDict <- buildDictionaryTyConT exprBTyCon ty''

        exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
        let exprTyConApp = mkTyConApp exprTyCon [ty'']

        functId <- thNameToId '(<$>)
        functTyCon <- thNameToTyCon ''Data.Functor.Functor
        functDict <- buildDictionaryTyConT functTyCon ty'

        let repApp = mkCoreApps (Var repId) [Type ty'', repDict]
        let repExpr = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, 
                                                          functDict, repApp, e']

        return $ mkLams bs repExpr
