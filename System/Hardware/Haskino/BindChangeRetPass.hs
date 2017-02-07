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

import System.Hardware.Haskino.Dictionary (buildDictionaryT, PassCoreM(..), )

import qualified System.Hardware.Haskino

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
                  -- Lookup the GHC ID of rep_ function
                  Just repName <- liftCoreM $ thNameToGhcName 'System.Hardware.Haskino.rep_
                  repId <- liftCoreM $ lookupId repName
                  -- Lookup the GHC type constructor of ExprB
                  Just exprBName <- liftCoreM $ thNameToGhcName ''System.Hardware.Haskino.ExprB
                  exprBTyCon <- liftCoreM $ lookupTyCon exprBName
                  -- Make the type of the ExprB for the specified type
                  let repTyConApp = mkTyConApp exprBTyCon [retTy']
                  -- Build the ExprB dictionary argument to apply
                  repDict <- buildDictionaryT repTyConApp

                  -- Lookup the GHC type constructor of Expr
                  Just exprName <- liftCoreM $ thNameToGhcName ''System.Hardware.Haskino.Expr
                  exprTyCon <- liftCoreM $ lookupTyCon exprName
                  -- Make the type of the Expr for the specified type
                  let exprTyConApp = mkTyConApp exprTyCon [retTy']

                  -- Lookup the GHC ID of <$> function
                  Just functAppName <- liftCoreM $ thNameToGhcName '(<$>)
                  functId <- liftCoreM $ lookupId functAppName
                  -- Lookup the GHC type constructor of Functor
                  Just functName <- liftCoreM $ thNameToGhcName ''Data.Functor.Functor
                  functTyCon <- liftCoreM $ lookupTyCon functName
                  -- Make the type of the Functor for the specified type
                  let retTyConTy = mkTyConTy retTyCon
                  let functTyConApp = mkTyConApp functTyCon [retTyConTy]
                  -- Build the Functor dictionary argument to apply
                  functDict <- buildDictionaryT functTyConApp

                  -- Build the modified expresion
                  let repApp = mkCoreApps (Var repId) [Type retTy', repDict]
                  let functApp = mkCoreApps (Var functId) [Type retTyConTy, Type retTy', Type exprTyConApp, functDict, repApp, e']

                  -- Change binding type
                  let b' = setVarType b $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])

                  return (NonRec b' (mkCoreLams bs functApp))
              _ -> return bndr
      _ -> return bndr
changeBind (Rec bs) = do
  return $ Rec bs
