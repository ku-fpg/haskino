-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-- forall (f :: Arduino a) (g :: a -> Arduino (Expr b)) (k :: b -> Arduino c).
--     (f >>= (abs_ <$> g)) >>= k
--        =
--     (f >>= g) >>= k . abs_
-- 
--  And 
-- 
-- forall (f :: Arduino a).
--     (\x -> F[x]).abs
--        =
--     (\x' -> let x=abs(x') in F[x])
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
                            --, fromTyArgCount :: Int
                            , toId           :: BindM Id
                            , toTyCon        :: BindM TyCon
                           }

-- The following talbe defines the names of the Shallow DSL functions
-- to translate from and the Deep DSL functions to translate to.
xlatList :: [XlatEntry]
xlatList = [  XlatEntry (thNameToId 'not)
                        (thNameToId 'Data.Boolean.notB)
                        -- (thNameToTyCon exprTyConTH)
                        (thNameToTyCon ''Data.Boolean.Boolean)
            , XlatEntry (thNameToId '(||))
                        (thNameToId '(||*))
                        -- (thNameToTyCon exprTyConTH)
                        (thNameToTyCon ''Data.Boolean.Boolean)
            , XlatEntry (thNameToId '(&&))
                        (thNameToId '(&&*))
                       -- (thNameToTyCon exprTyConTH)
                        (thNameToTyCon ''Data.Boolean.Boolean)
            , XlatEntry (thNameToId '(+))
                        (thNameToId '(+))
                        -- (thNameToTyCon exprTyConTH)
                        (thNameToTyCon ''Prelude.Num)
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
repPushPass guts = 
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
                    Just xe -> do
                      liftCoreM $ putMsg $ ppr [ty, d, e']
                      pr <- pushRep xe args'
                      liftCoreM $ putMsg $ ppr pr
                      return pr
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
    liftCoreM $ putMsgS "-------------------"
    liftCoreM $ putMsg $ ppr args
    -- Break down the arguments from the old function
    -- into foralls, args, and return types.
    let (fromForAlls, fromFuncTy) = splitForAllTys $ idType fi
    let (toForAlls, toFuncTy) = splitForAllTys $ idType ti
    let (toArgTys, toRetTy) = splitFunTys toFuncTy
    -- Get the count of non-dictionary args in the new function
    let argCount  = countNonDictTypes toArgTys
    let dictCount = (length toArgTys) - argCount
    -- Get the original args based on the argCount
    let origArgs = drop ((length args) - argCount) args
    let dictTys = take dictCount toArgTys
    let nonDictTys = drop dictCount toArgTys

    -- liftCoreM $ putMsgS "*******************"
    -- liftCoreM $ putMsg $ ppr $ (fromArgTys, fromRetTy)
    -- liftCoreM $ putMsg $ ppr $ countNonDictTypes fromArgTys
    -- liftCoreM $ putMsgS "###################"
    -- liftCoreM $ putMsg $ ppr $ (toArgTys, toRetTy)
    -- liftCoreM $ putMsg $ ppr $ isDictTy $ head toArgTys
    -- liftCoreM $ putMsg $ ppr $ countNonDictTypes toArgTys
    -- liftCoreM $ putMsgS "*******************"
    let typeArgs = genForAllArgs toForAlls nonDictTys origArgs
    exprTypeArgs <- mapM (thNameTyToTyConApp exprTyConTH) typeArgs
    let exprTypeVars = map Type exprTypeArgs
    -- liftCoreM $ putMsgS "!!!!!!!!!!!!!!"
    dictArgs <- genDictArgs dictTys nonDictTys origArgs
    repArgs <- mapM repExpr origArgs
    repArgs' <- mapM changeRepExpr repArgs
    -- liftCoreM $ putMsg $ ppr $ typeArgs ++ dictArgs ++ repArgs'
    -- return $ head args
    return $ mkCoreApps (Var ti) (exprTypeVars ++ dictArgs ++ repArgs')

genDictArgs :: [Type] -> [Type] -> [CoreExpr] -> BindM [CoreExpr]
genDictArgs [] _  _ = return []
genDictArgs (dty:dtys) tys args = do
    -- let (tyCon, [ty']) = splitTyConApp dty
    let (tyConTy, ty') = splitAppTy dty
    -- liftCoreM $ putMsg $ ppr dty
    -- liftCoreM $ putMsg $ ppr tyConTy
    -- liftCoreM $ putMsg $ ppr ty'
    case findIndex (eqType ty') tys of
      Just idx -> do
        let dictTy = exprType $ args !! idx
        -- liftCoreM $ putMsg $ ppr (tyConAppTyCon tyConTy)
        -- liftCoreM $ putMsg $ ppr $ args
        -- liftCoreM $ putMsg $ ppr $ idx
        -- liftCoreM $ putMsg $ ppr dictTy thNameToTyCon
        exprTyCon <- thNameToTyCon exprTyConTH
        liftCoreM $ putMsgS "*******************"
        liftCoreM $ putMsg $ ppr (tyConAppTyCon tyConTy)
        exprTy <- thNameTyToTyConApp exprTyConTH dictTy
        dict <- buildDictionaryTyConT (tyConAppTyCon tyConTy) exprTy
        -- dict <- buildDictionaryTyConT (tyConAppTyCon tyConTy) dictTy
        -- liftCoreM $ putMsgS "Here I am"
        -- liftCoreM $ putMsg $ ppr dict
        dicts <- genDictArgs dtys tys args
        return $ dict:dicts
      Nothing  -> error "Can't find tyVar"

genForAllArgs :: [TyVar] -> [Type] -> [CoreExpr] -> [Type]
genForAllArgs [] _ _  = []
genForAllArgs (tv:tvs) tys args = 
    case findIndex (eqType (mkTyVarTy tv)) tys of
      Just idx -> (exprType $ args !! idx) : genForAllArgs tvs tys args
      Nothing  -> error "Can't find tyVar"

countNonDictTypes :: [Type] -> Int
countNonDictTypes [] = 0
countNonDictTypes (ty:tys) = if isDictTy ty
                           then countNonDictTypes tys
                           else 1 + countNonDictTypes tys



