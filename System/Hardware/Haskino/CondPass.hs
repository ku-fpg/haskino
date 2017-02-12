-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.CondPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Conditional Transformation Pass
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.CondPass (condPass) where

import CoreMonad
import GhcPlugins
import Data.List 
import Data.Functor
import Control.Monad.Reader

import System.Hardware.Haskino.Dictionary (buildDictionaryT, 
                                           buildDictionaryTyConT, 
                                           PassCoreM(..), 
                                           thNameToId, thNameToTyCon)

import qualified System.Hardware.Haskino
import qualified System.Hardware.Haskino.Data
import qualified System.Hardware.Haskino.Expr

data CondEnv
    = CondEnv
      { pluginModGuts :: ModGuts
      }

newtype CondM a = CondM { runCondM :: ReaderT CondEnv CoreM a }
    deriving (Functor, Applicative, Monad
             ,MonadIO, MonadReader CondEnv)

instance PassCoreM CondM where
  liftCoreM = CondM . ReaderT . const
  getModGuts = CondM $ ReaderT (return . pluginModGuts)

condPass :: ModGuts -> CoreM ModGuts
condPass guts = do
    bindsOnlyPass (\x -> (runReaderT (runCondM $ (mapM condBind) x) (CondEnv guts))) guts

condBind :: CoreBind -> CondM CoreBind
condBind bndr@(NonRec b e) = do
  e' <- condExpr e
  return (NonRec b e')
condBind (Rec bs) = do
  bs' <- condExpr' bs
  return $ Rec bs'

condBind' :: [(Id, CoreExpr)] -> CondM [(Id, CoreExpr)]
condBind' [] = return []
condBind' ((b, e) : bs) = do
  e' <- condExpr e
  bs' <- condBind' bs
  return $ (b, e') : bs'

condExpr :: CoreExpr -> CondM CoreExpr
condExpr e = do
  df <- liftCoreM getDynFlags
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      e1' <- condExpr e1
      e2' <- condExpr e2
      return $ App e1' e2'
    Lam tb e -> do
      e' <- condExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- condExpr body
      bind' <- case bind of 
                  (NonRec v e) -> do
                    e' <- condExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- condExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body' 
    Case e tb ty alts | showSDoc df (ppr ty) == "Arduino ()" -> do
      e' <- condExpr e
      alts' <- condExprAlts alts
      if length alts' == 2 
      then case alts' of
        [(ac1, _, _), _] -> do
          case ac1 of 
            DataAlt d -> do
              Just falseName <- liftCoreM $ thNameToGhcName 'Prelude.False
              if (getName d) == falseName
              then condTransformUnit ty e' alts'
              else return $ Case e' tb ty alts'
            _ -> return $ Case e' tb ty alts'
      else return $ Case e' tb ty alts'
    Case e tb ty alts | isPrefixOf "Arduino " (showSDoc df (ppr ty)) -> do
      e' <- condExpr e
      alts' <- condExprAlts alts
      if length alts' == 2 
      then case alts' of
        [(ac1, _, _), _] -> do
          case ac1 of 
            DataAlt d -> do
              Just falseName <- liftCoreM $ thNameToGhcName 'Prelude.False
              if (getName d) == falseName
              then condTransform ty e' alts'
              else return $ Case e' tb ty alts'
            _ -> return $ Case e' tb ty alts'
      else return $ Case e' tb ty alts'
    Case e tb ty alts -> do
      e' <- condExpr e
      alts' <- condExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- condExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- condExpr e
      return $ Cast e' co

nameString :: Name -> String 
nameString = occNameString . nameOccName

condExpr' :: [(Id, CoreExpr)] -> CondM [(Id, CoreExpr)]
condExpr' [] = return []
condExpr' ((b, e) : bs) = do
  e' <- condExpr e
  bs' <- condExpr' bs
  return $ (b, e') : bs'

condExprAlts :: [GhcPlugins.Alt CoreBndr] -> CondM [GhcPlugins.Alt CoreBndr]
condExprAlts [] = return []
condExprAlts ((ac, b, a) : as) = do
  a' <- condExpr a
  bs' <- condExprAlts as
  return $ (ac, b, a') : bs'

{-
  The following performs this transform:

    forall (b :: Bool) (t :: ArduinoConditional a => Arduino a) (e :: ArduinoConditional a => Arduino a).
    if b then t else e
      =
    rep_ <$> ifThenElseE (abs_ b) (abs_ <$> t) (abs_ <$> e)

-}
condTransform :: Type -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> CondM CoreExpr
condTransform ty e alts = do
  case alts of
    [(_, _, e1),(_, _, e2)] -> do
      -- Get the Arduino Type Con
      let Just tyCon'  = tyConAppTyCon_maybe ty
      let ty' = mkTyConTy tyCon'
      -- Get the Arduino Type Arg
      let Just [ty''] = tyConAppArgs_maybe ty
      -- Get the conditional type
      let bTy = exprType e

      ifThenElseId <- thNameToId 'System.Hardware.Haskino.ifThenElseE
      condTyCon <- thNameToTyCon ''System.Hardware.Haskino.ArduinoConditional
      condDict <- buildDictionaryTyConT condTyCon ty''

      absId <- thNameToId 'System.Hardware.Haskino.abs_
      repId <- thNameToId 'System.Hardware.Haskino.rep_

      exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
      repDict <- buildDictionaryTyConT exprBTyCon bTy

      functId <- thNameToId '(<$>)
      functTyCon <- thNameToTyCon ''Data.Functor.Functor
      functDict <- buildDictionaryTyConT functTyCon ty'

      exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr.Expr
      -- Make the type of the Expr for the specified type
      let exprTyConApp = GhcPlugins.mkTyConApp exprTyCon [ty'']

      -- Build the First Arg to ifThenElseE
      let arg1 = mkCoreApps (Var repId) [Type bTy, repDict, e]

      -- Build the Second Arg to ifThenElseE
      arg2 <- changeReturn e1
      -- let arg2 = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, functDict, (Var repId), e1']

      -- Build the Third Arg to ifThenElseE
      arg3 <- changeReturn e2
      -- let arg3 = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, functDict, (Var repId), e2']

      -- Build the ifThenElse Expr
      let ifteExpr = mkCoreApps (Var ifThenElseId) [Type ty'', condDict, arg1, arg2, arg3]

      -- Build the rep wrapped ifThenElse
      --let repIfteExpr = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, functDict, App (Var absId) (Type ty''), ifteExpr]

      return ifteExpr

{-
  The following performs this transform:

    forall (b :: Bool) (t :: Arduino ()) (e :: Arduino ()).
    if b then t else e
      =
    ifThenElseUnitE (rep_ b) t e

-}
condTransformUnit :: Type -> CoreExpr -> [GhcPlugins.Alt CoreBndr] -> CondM CoreExpr
condTransformUnit ty e alts = do
  case alts of
    [(_, _, e1),(_, _, e2)] -> do
      -- Get the conditional type
      let bTy = exprType e

      ifThenElseId <- thNameToId 'System.Hardware.Haskino.ifThenElseE

      repId <- thNameToId 'System.Hardware.Haskino.rep_
      exprBTyCon <- thNameToTyCon ''System.Hardware.Haskino.ExprB
      repDict <- buildDictionaryTyConT exprBTyCon bTy

      -- Build the First Arg to ifThenElseUnitE
      let arg1 = mkCoreApps (Var repId) [Type bTy, repDict, e]

      return $ mkCoreApps (Var ifThenElseId) [arg1, e1, e2]

changeReturn :: CoreExpr -> CondM CoreExpr
changeReturn e = do
    df <- liftCoreM getDynFlags
    let (f, args) = collectArgs e
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

            exprTyCon <- thNameToTyCon ''System.Hardware.Haskino.Expr
            let retTyConApp = mkTyConApp exprTyCon [ty2]

            return $ mkCoreApps f [Type ty1, Var d, Type retTyConApp, retArg]
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
                let exprTyConApp = mkTyConApp exprTyCon [ty'']

                functId <- thNameToId '(<$>)
                functTyCon <- thNameToTyCon ''Data.Functor.Functor
                functDict <- buildDictionaryTyConT functTyCon ty'

                let repApp = mkCoreApps (Var repId) [Type ty'', repDict]
                let repExpr = mkCoreApps (Var functId) [Type ty', Type ty'', Type exprTyConApp, functDict, repApp, e]
                return repExpr
              _ -> return e
