-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangeRetPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangeRetPass (bindChangeRetPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Control.Monad.Reader

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

                  -- Change the return
                  e'' <- changeReturn e'

                  -- Rebuild the Lambdas with the changed return
                  return (NonRec b (mkCoreLams bs e''))
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
        case args of
          [Type ty1, Var d, Type ty2, ex] | showSDoc df (ppr f) == "return" -> do
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
              return $ mkLams bs retExp
          _ -> return $ mkLams bs e'
