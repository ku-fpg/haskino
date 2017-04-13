-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Local bind argument and return type change pass
-- f :: a -> ... -> c -> d ==> f :: Expr a  -> ... -> Expr c -> Expr d
-- It does this by changing the type of the argument, and then replacing
-- each occurnace of (rep_ a) of the argument 'a' with just the type 
-- changed argument itself.
-- It does this by inserting a rep_ <$> to the last expresion of the bind
-- chain for the local bind to change the return type.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass (bindChangeArgRetPass) where

import CoreMonad
import GhcPlugins
import Var
import Data.Functor
import Control.Monad.State

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

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

bindChangeArgRetPass :: ModGuts -> CoreM ModGuts
bindChangeArgRetPass guts = do
    bindsL' <- (\x -> fst <$> (runStateT (runBindM $ (mapM changeBind) x) (BindEnv guts []))) (mg_binds guts)
    return (guts { mg_binds = concat bindsL' })

changeBind :: CoreBind -> BindM [CoreBind]
changeBind bndr@(NonRec b e) = do
  df <- liftCoreM getDynFlags
  let (argTys, retTy) = splitFunTys $ varType b
  let (bs, e') = collectBinders e
  let tyCon_m = splitTyConApp_maybe retTy
  monadTyConId <- thNameToTyCon monadTyConTH
  unitTyConId <- thNameToTyCon ''()
  let unitTyConTy = mkTyConTy unitTyConId
  case tyCon_m of
      -- We are looking for return types of Arduino a
      Just (retTyCon, [retTy']) | retTyCon == monadTyConId -> do
          -- Change the binds and arg types to Expr a
          zipBsArgTys <- mapM changeArg (zip bs argTys)
          let (bs', argTys') = unzip zipBsArgTys

          -- Put arg types into state
          s <- get
          put s{args = bs'}

          -- Change any apps of the args in the body
          e'' <- changeArgAppsExpr e'

          -- Generate args for new shallow body
          deepArgs <- mapM repExpr (map Var bs)

          -- If it is not a unit type return, change return type
          if not (retTy' `eqType` unitTyConTy)
          then do
              exprTyCon <- thNameToTyCon exprTyConTH
              let exprTyConApp = mkTyConApp exprTyCon [retTy']

              -- Change the return
              e''' <- fmapRepBindReturn e''

              -- Change the top level bind type
              let b' = setVarType b $ mkFunTys argTys (mkTyConApp retTyCon [exprTyConApp])

              -- Apply the abs <$> to the new shallow body
              let shallowE = mkCoreApps (Var b') deepArgs
              absExpr <- fmapAbsExpr (mkTyConTy retTyCon) retTy' shallowE

              -- return [NonRec b absExpr, NonRec b' $ mkLams bs' e''']
              return [NonRec b' $ mkLams bs' e''']
          else if length bs > 0 
              then do
                  -- Change the top level bind type
                  let b' = setVarType b $ mkFunTys argTys' retTy

                  let shallowE = mkCoreApps (Var b') deepArgs
                  -- return [NonRec b shallowE, NonRec b' $ mkLams bs' e'']
                  return [NonRec b' $ mkLams bs' e'']
              else return [bndr]
      _ -> return [bndr]
changeBind (Rec bs) = do
  return [Rec bs]

changeArg :: (CoreBndr, Type) -> BindM (CoreBndr, Type)
changeArg (b, ty) = do
  let tyCon_m = splitTyConApp_maybe ty
  case tyCon_m of
    Just (_, []) -> do
        -- ToDo:  Check that type is a valid Haskino Expr Type?
        -- Lookup the GHC type constructor of Expr
        exprTyCon <- thNameToTyCon exprTyConTH
        -- Make the type of the Expr for the specified type
        let ty' = mkTyConApp exprTyCon [ty]
        let b' = setVarType b ty'
        return (b', ty')
    _       -> return (b, ty)

changeArgAppsExpr :: CoreExpr -> BindM CoreExpr
changeArgAppsExpr e = do
  df <- liftCoreM getDynFlags
  s <- get
  repId <- thNameToId repNameTH
  case e of
    -- Replace any occurances of the parameters "p" with "abs_ p"
    Var v | v `elem` (args s) -> absExpr e
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- changeArgAppsExpr e1
      e2' <- changeArgAppsExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- changeArgAppsExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeArgAppsExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeArgAppsExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeArgAppsExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeArgAppsExpr e
      alts' <- changeArgAppsExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeArgAppsExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeArgAppsExpr e
      return $ Cast e' co

changeArgAppsExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeArgAppsExpr' [] = return []
changeArgAppsExpr' ((b, e) : bs) = do
  e' <- changeArgAppsExpr e
  bs' <- changeArgAppsExpr' bs
  return $ (b, e') : bs'

changeArgAppsExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeArgAppsExprAlts [] = return []
changeArgAppsExprAlts ((ac, b, a) : as) = do
  a' <- changeArgAppsExpr a
  bs' <- changeArgAppsExprAlts as
  return $ (ac, b, a') : bs'

