-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Rep Push Pass
-- This pass is only partially completed.  It is intended to do the rep
-- pushing as do rules like:
--    forall (b1 :: Bool) (b2 :: Bool).
--    rep_ (b1 || b2)
--      =
--    (rep_ b1) ||* (rep_ b2)
-- However, hadling for rules for the comparison operators such as
-- (>*) proved much harder than expected due to coercions, so
-- it is at this point incomplete and will not work with such ops.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass (repPushPass) where

import CoreMonad
import GhcPlugins
import Data.Functor
import Data.List
import Control.Monad.Reader
import OccName
import Var

import System.Hardware.Haskino.ShallowDeepPlugin.Utils

import Data.Boolean
import System.Hardware.Haskino

data XlatEntry = XlatEntry {  fromId         :: BindM Id
                            , toId           :: BindM Id
                           }

-- The following talbe defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (thNameToId 'not)
                        (thNameToId 'Data.Boolean.notB)
            , XlatEntry (thNameToId '(||))
                        (thNameToId '(||*))
            , XlatEntry (thNameToId '(&&))
                        (thNameToId '(&&*))
            , XlatEntry (thNameToId '(==))
                        (thNameToId 'eqE)
            , XlatEntry (thNameToId '(/=))
                        (thNameToId 'neqE)
            , XlatEntry (thNameToId '(>))
                        (thNameToId 'greatE)
            , XlatEntry (thNameToId '(<))
                        (thNameToId 'lessE)
            , XlatEntry (thNameToId '(>=))
                        (thNameToId 'greateqE)
            , XlatEntry (thNameToId '(<))
                        (thNameToId 'lesseqE)
            , XlatEntry (thNameToId '(+))
                        (thNameToId '(+))
           ]

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

repPushPass :: ModGuts -> CoreM ModGuts
repPushPass guts = do
    bindsOnlyPass (\x -> (runReaderT (runBindM $ (mapM changeRep) x) (BindEnv guts))) guts

changeRep :: CoreBind -> BindM CoreBind
changeRep bndr@(NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeRepExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeRep (Rec bs) = do
  return $ Rec bs

changeRepExpr :: CoreExpr -> BindM CoreExpr
changeRepExpr e = do
  df <- liftCoreM getDynFlags
  repId <- thNameToId repNameTH
  case e of
    Var v -> return $ Var v
    Lit l -> return $ Lit l
    Type ty -> return $ Type ty
    Coercion co -> return $ Coercion co
    App e1 e2 -> do
      let (b, args) = collectArgs e
      let defaultReturn = do
            e1' <- changeRepExpr e1
            e2' <- changeRepExpr e2
            return $ App e1' e2'
      case b of
        Var v | v == repId -> do
          case args of
            [ty, d, e'] -> do
              let (b', args') = collectArgs e'
              case b' of
                Var v' -> do
                  inList <- funcInXlatList v'
                  case inList of
                    Just xe -> pushRep xe args'
                    _ -> defaultReturn
                _ -> defaultReturn
            _ -> defaultReturn
        _ -> defaultReturn
    Lam tb e -> do
      e' <- changeRepExpr e
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeRepExpr body
      bind' <- case bind of
                  (NonRec v e) -> do
                    e' <- changeRepExpr e
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeRepExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case e tb ty alts -> do
      e' <- changeRepExpr e
      alts' <- changeRepExprAlts alts
      return $ Case e' tb ty alts'
    Tick t e -> do
      e' <- changeRepExpr e
      return $ Tick t e'
    Cast e co -> do
      e' <- changeRepExpr e
      return $ Cast e' co

changeRepExpr' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeRepExpr' [] = return []
changeRepExpr' ((b, e) : bs) = do
  e' <- changeRepExpr e
  bs' <- changeRepExpr' bs
  return $ (b, e') : bs'

changeRepExprAlts :: [GhcPlugins.Alt CoreBndr] -> BindM [GhcPlugins.Alt CoreBndr]
changeRepExprAlts [] = return []
changeRepExprAlts ((ac, b, a) : as) = do
  a' <- changeRepExpr a
  bs' <- changeRepExprAlts as
  return $ (ac, b, a') : bs'

funcInXlatList :: Id -> BindM (Maybe XlatEntry)
funcInXlatList id = do
  funcInXlatList' id xlatList
    where
      funcInXlatList' :: Id -> [XlatEntry] -> BindM (Maybe XlatEntry)
      funcInXlatList' id [] = return Nothing
      funcInXlatList' id (xl:xls) = do
          fId <- fromId xl
          if fId == id
          then return $ Just xl
          else funcInXlatList' id xls

pushRep :: XlatEntry -> [CoreExpr] -> BindM CoreExpr
pushRep xe args = do
    fi <- fromId xe
    ti <- toId xe
    -- Break down the arguments from the old function
    -- into foralls, args, and return types.
    let (fromForAlls, fromFuncTy) = splitForAllTys $ idType fi
    let (fromArgTys, fromRetTy) = splitFunTys fromFuncTy
    let (toForAlls, toFuncTy) = splitForAllTys $ idType ti
    let (toArgTys, toRetTy) = splitFunTys toFuncTy
    -- Get the count of non-dictionary args in the new function
    let argCount  = countNonDictTypes toArgTys
    let dictCount = (length toArgTys) - argCount
    -- Get the original args based on the argCount
    let origArgs = drop ((length args) - argCount) args
    let dictTys = take dictCount toArgTys
    let nonDictTys = drop dictCount toArgTys

    let typeArgs = genForAllArgs toForAlls nonDictTys fromRetTy origArgs
    exprTypeArgs <- mapM (thNameTyToTyConApp exprTyConTH) typeArgs
    let exprTypeVars = map Type exprTypeArgs
    dictArgs <- genDictArgs dictTys nonDictTys fromRetTy toRetTy origArgs
    repArgs <- mapM repExpr origArgs
    repArgs' <- mapM changeRepExpr repArgs
    return $ mkCoreApps (Var ti) (exprTypeVars ++ dictArgs ++ repArgs')

genDictArgs :: [Type] -> [Type] -> Type -> Type -> [CoreExpr] -> BindM [CoreExpr]
genDictArgs [] _ _ _ _ = return []
genDictArgs (dty:dtys) tys frty trty args = do
    let (tyConTy, ty') = splitAppTy dty
    case findIndex (typeIn ty') tys of
      Just idx -> do
        -- Find the index of the from function arg which matches the 
        -- type required by the dictionary
        let dictTy = exprType $ args !! idx
        -- Get the type of the to function arg at the same index
        let toArgTy = tys !! idx
        -- Determine if it has a type constructor
        let tys_m = splitTyConApp_maybe toArgTy
        dictTy' <- case tys_m of
                     -- If it does, we are dealing with a function
                     -- of type Class a => Expr a -> ... -> retType
                     -- so the dictionary type is of a not Expr a
                     Just (tyCon, tys') -> return dictTy
                     -- If there is no TyCon, then we have a 
                     -- function of type Class a => a -> ... -> retType
                     -- so the dictionary type needs to be Expr a.
                     Nothing -> do
                         exprTyCon <- thNameToTyCon exprTyConTH
                         thNameTyToTyConApp exprTyConTH dictTy
        dict <- buildDictionaryTyConT (tyConAppTyCon tyConTy) dictTy'
        dicts <- genDictArgs dtys tys frty trty args
        return $ dict:dicts
      Nothing  -> do
        -- Determine if the return type has a type constructor
        let tys_m = splitTyConApp_maybe trty
        dictTy' <- case tys_m of
                     -- If it does, we are dealing with a function
                     -- of type Class a => Expr a -> ... -> retType
                     -- so the dictionary type is of a not Expr a
                     Just (tyCon, tys') -> return trty
                     -- If there is no TyCon, then we have a 
                     -- function of type Class a => a -> ... -> retType
                     -- so the dictionary type needs to be Expr a.
                     Nothing -> do
                         exprTyCon <- thNameToTyCon exprTyConTH
                         thNameTyToTyConApp exprTyConTH trty
        dict <- buildDictionaryTyConT (tyConAppTyCon tyConTy) dictTy'
        dicts <- genDictArgs dtys tys frty trty args
        return $ dict:dicts

genForAllArgs :: [TyVar] -> [Type] -> Type -> [CoreExpr] -> [Type]
genForAllArgs [] _ _ _ = []
genForAllArgs (tv:tvs) tys rty args =
    case findIndex (eqType (mkTyVarTy tv)) tys of
      Just idx -> (exprType $ args !! idx) : genForAllArgs tvs tys rty args
      Nothing  -> rty : genForAllArgs tvs tys rty args

countNonDictTypes :: [Type] -> Int
countNonDictTypes [] = 0
countNonDictTypes (ty:tys) = if isDictTy ty
                           then countNonDictTypes tys
                           else 1 + countNonDictTypes tys

typeIn :: Type -> Type -> Bool
typeIn t ty = 
    if t `eqType` ty 
    then True
    else
      let (tyCon, tys') = splitTyConApp ty in
      t `eqType` last tys'
