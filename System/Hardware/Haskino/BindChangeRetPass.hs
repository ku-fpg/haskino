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
                  repId <- thNameToId 'System.Hardware.Haskino.rep_
                  exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
                  repDict <- buildDictionaryTyConT exprBTyCon retTy'

                  exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
                  let exprTyConApp = mkTyConApp exprTyCon [retTy']

                  functId <- thNameToId '(<$>)
                  functTyCon <- thNameToTyCon ''Data.Functor.Functor
                  functDict <- buildDictionaryTyConT functTyCon (mkTyConTy retTyCon)

                  -- Change the return
                  e'' <- changeReturn e'

                  -- Build the modified expresion
                  --let repApp = mkCoreApps (Var repId) [Type retTy', repDict]
                  --let functApp = mkCoreApps (Var functId) [Type retTyConTy, Type retTy', Type exprTyConApp, functDict, repApp, e'']

                  -- Change binding type
                  let b' = setVarType b $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])
                  liftCoreM $ putMsgS "!!!!@@@@"
                  liftCoreM $ putMsg $ ppr $ varType b'
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
        -- Need to strip off labmdas and look for return inside in the case 
        -- statment.
        -- I'm missing the case of (\x -> return x).
        case args of
          [Type ty1, Var d, Type ty2, ex] | showSDoc df (ppr f) == "return" -> do
              repId <- thNameToId 'System.Hardware.Haskino.rep_
              exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
              repDict <- buildDictionaryTyConT exprBTyCon ty2
        
              let retArg = mkCoreApps (Var repId) [Type ty2, repDict, ex]
              let retLam = mkLams bs retArg

              exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
              -- Make the type of the Expr for the specified type
              let retTyConApp = mkTyConApp exprTyCon [ty2]

              return $ mkCoreApps f [Type ty1, Var d, Type retTyConApp, retLam]
          _ -> do
              let (_, retTy) = splitFunTys $ exprType f
              -- Get the Arduino Type Con
              let Just tyCon'  = tyConAppTyCon_maybe retTy
              let ty' = mkTyConTy tyCon'
              liftCoreM $ putMsgS "&&&&&&&&&&&"
              liftCoreM $ putMsg $ ppr retTy
              liftCoreM $ putMsg $ ppr ty'
              -- Get the Arduino Type Arg
              case tyConAppArgs_maybe retTy of
                Just [ty''] -> do
                  liftCoreM $ putMsg $ ppr ty''
                  repId <- thNameToId 'System.Hardware.Haskino.rep_
                  exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
                  repDict <- buildDictionaryTyConT exprBTyCon ty''

                  exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
                  -- Make the type of the Expr for the specified type
                  let exprTyConApp = mkTyConApp exprTyCon [ty'']

                  functId <- thNameToId '(<$>)
                  functTyCon <- thNameToTyCon ''Data.Functor.Functor
                  functDict <- buildDictionaryTyConT functTyCon ty'

                  let repApp = mkCoreApps (Var repId) [Type ty'', repDict]
                  let repExpr = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, 
                                                          functDict, repApp, mkLams bs e']
                  return repExpr
                _ -> return $ mkLams bs e'
