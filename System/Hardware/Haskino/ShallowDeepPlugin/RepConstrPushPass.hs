-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.repConstrPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Push rep through case alternatives
-- This pass is used to push rep_ expressions through case alternatives.
-- This will only be used to translate case expressions that match on a
-- type that is not in the ExprB type class of the deep expression langauge,
-- but the result type of the case is of a ExprB type:
-- Its transformation does the following:
--
-- forall (a1 :: ExprB a => a) ... (an :: ExprB a => a)
-- rep (case c of e1 -> a1 ... cn -> an)
--    =
-- case c of e1 -> rep_ a1 ... cn -> rep_ an
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepConstrPushPass (repConstrPushPass) where

import Control.Monad.Reader
import CoreMonad
import GhcPlugins

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

constrList :: [BindM Id]
constrList = [  ( thNameToId 'Just )
              , ( thNameToId 'Nothing )
             ]

repConstrPushPass :: ModGuts -> CoreM ModGuts
repConstrPushPass guts =
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM repConstrPush) x) (BindEnv guts))) guts

repConstrPush :: CoreBind -> BindM CoreBind
repConstrPush (NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- repConstrPushExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
repConstrPush (Rec bs) = do
  bs' <- repConstrPush' bs
  return $ Rec bs'

repConstrPush' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
repConstrPush' [] = return []
repConstrPush' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- repConstrPushExpr e'
  let e''' = mkLams lbs e''
  bs' <- repConstrPush' bs
  return $ (b, e''') : bs'

repConstrPushExpr :: CoreExpr -> BindM CoreExpr
repConstrPushExpr e = do
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    -- Look for expressions of the form:
    -- rep (Constructor )
    (Var repV) :$ (Type rTy) :$ dict :$ e1 -> do
      e1' <- repConstrPushExpr e1
      let defaultRet = return ( (Var repV) :$ (Type rTy) :$ dict :$ e1' )
      let (ls,e1'') = collectLets e1'
      case e1'' of
        (Var constr) :$ (Type cTy) :$ e2 -> do
            isConstr <- funcInConstrList constr
            if isConstr
            then do
                let (f, args) = collectArgs e1''
                liftCoreM $ putMsgS "********************"
                liftCoreM $ putMsg $ ppr constr
                liftCoreM $ putMsgS $ show $ length args
                exprTyCon <- thNameToTyCon exprTyConTH
                let exprTyConApp = mkTyConApp exprTyCon [cTy]
                let la = length args
                case la of
                    2 -> do 
                      e2' <- repExpr e2
                      return $ ((Var constr) :$ (Type exprTyConApp) :$ e2')
                    _ -> defaultRet
            else defaultRet
        (Var constr) :$ (Type cTy) -> do
            isConstr <- funcInConstrList constr
            if isConstr
            then do
                let (f, args) = collectArgs e1''
                liftCoreM $ putMsgS "********************"
                liftCoreM $ putMsg $ ppr constr
                liftCoreM $ putMsgS $ show $ length args
                exprTyCon <- thNameToTyCon exprTyConTH
                let exprTyConApp = mkTyConApp exprTyCon [cTy]
                return $ ((Var constr) :$ (Type exprTyConApp))
            else defaultRet
        _ -> defaultRet
    App e1 e2 -> do
      e1' <- repConstrPushExpr e1
      e2' <- repConstrPushExpr e2
      return $ App e1' e2'
    Lam tb el -> do
      e' <- repConstrPushExpr el
      return $ Lam tb e'
    Let bind body -> do
      body' <- repConstrPushExpr body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- repConstrPushExpr el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- repConstrPushExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- repConstrPushExpr ec
      alts' <- repConstrPushExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- repConstrPushExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- repConstrPushExpr ec
      return $ Cast e' co

repConstrPushAlt :: Type -> GhcPlugins.Alt CoreBndr -> BindM (GhcPlugins.Alt CoreBndr)
repConstrPushAlt ty (ac, b, a) = do
  a' <- repConstrPushExpr a
  repId <- thNameToId repNameTH
  repDict <- thNameTyToDict exprClassTyConTH ty
  let repE = mkCoreApps (Var repId) [Type ty, repDict, a']
  return (ac, b, repE)

repConstrPushExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
repConstrPushExpr' [] = return []
repConstrPushExpr' ((b, e) : bs) = do
  e' <- repConstrPushExpr e
  bs' <- repConstrPushExpr' bs
  return $ (b, e') : bs'

repConstrPushExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
repConstrPushExprAlts [] = return []
repConstrPushExprAlts ((ac, b, a) : as) = do
  a' <- repConstrPushExpr a
  bs' <- repConstrPushExprAlts as
  return $ (ac, b, a') : bs'

funcInConstrList :: Id -> BindM Bool
funcInConstrList idf = do
  funcInConstrList' idf constrList
    where
      funcInConstrList' :: Id -> [BindM Id] -> BindM Bool
      funcInConstrList' _ [] = return False
      funcInConstrList' idf' (xl:xls) = do
          fId <- xl
          if fId == idf'
          then return True
          else funcInConstrList' idf' xls
