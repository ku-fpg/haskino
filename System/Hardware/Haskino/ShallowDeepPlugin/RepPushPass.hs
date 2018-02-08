-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Rep Push Pass
-- This pass is used to transform shallow expressions into the
-- deep expression language.  It uses rules like the following:
--
--    forall (b1 :: Bool) (b2 :: Bool).
--    rep_ (b1 || b2)
--      =
--    (rep_ b1) ||* (rep_ b2)
--
-- Each of the from and to operations (in the example above '||' and
-- '||*' are specified in the xlatList data table.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass (repPushPass) where

import Control.Monad.Reader
import CoreMonad
import Data.Bits            as DB
import Data.Boolean
import Data.Boolean.Numbers as BN
import Data.Boolean.Bits    as BB
import Data.List
import GhcPlugins

import System.Hardware.Haskino
import System.Hardware.Haskino.ShallowDeepPlugin.Utils


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
            , XlatEntry (thNameToId '(<=))
                        (thNameToId 'lesseqE)
            , XlatEntry (thNameToId '(+))
                        (thNameToId '(+))
            , XlatEntry (thNameToId '(-))
                        (thNameToId '(-))
            , XlatEntry (thNameToId '(/))
                        (thNameToId '(/))
            , XlatEntry (thNameToId '(*))
                        (thNameToId '(*))
            , XlatEntry (thNameToId 'Prelude.div)
                        (thNameToId 'BN.div)
            , XlatEntry (thNameToId 'Prelude.rem)
                        (thNameToId 'BN.rem)
            , XlatEntry (thNameToId 'Prelude.quot)
                        (thNameToId 'BN.quot)
            , XlatEntry (thNameToId 'Prelude.mod)
                        (thNameToId 'BN.mod)
            , XlatEntry (thNameToId 'negate)
                        (thNameToId 'negate)
            , XlatEntry (thNameToId 'abs)
                        (thNameToId 'abs)
            , XlatEntry (thNameToId 'signum)
                        (thNameToId 'signum)
            , XlatEntry (thNameToId '(DB..&.))
                        (thNameToId '(BB..&.))
            , XlatEntry (thNameToId '(DB..|.))
                        (thNameToId '(BB..|.))
            , XlatEntry (thNameToId 'DB.xor)
                        (thNameToId 'BB.xor)
            , XlatEntry (thNameToId 'DB.complement)
                        (thNameToId 'BB.complement)
            , XlatEntry (thNameToId 'DB.setBit)
                        (thNameToId 'BB.setBit)
            , XlatEntry (thNameToId 'DB.clearBit)
                        (thNameToId 'BB.clearBit)
            , XlatEntry (thNameToId 'DB.testBit)
                        (thNameToId 'BB.testBit)
            , XlatEntry (thNameToId 'DB.finiteBitSize)
                        (thNameToId 'BB.bitSize)
            , XlatEntry (thNameToId 'DB.isSigned)
                        (thNameToId 'BB.isSigned)
            , XlatEntry (thNameToId 'DB.shiftL)
                        (thNameToId 'BB.shiftL)
            , XlatEntry (thNameToId 'DB.shiftR)
                        (thNameToId 'BB.shiftR)
            , XlatEntry (thNameToId 'DB.rotateL)
                        (thNameToId 'BB.rotateL)
            , XlatEntry (thNameToId 'DB.rotateR)
                        (thNameToId 'BB.rotateR)
            , XlatEntry (thNameToId '(++))
                        (thNameToId '(++*))
            , XlatEntry (thNameToId '(:))
                        (thNameToId '(*:))
            , XlatEntry (thNameToId '(!!))
                        (thNameToId '(!!*))
            , XlatEntry (thNameToId 'head)
                        (thNameToId 'headE)
            , XlatEntry (thNameToId 'tail)
                        (thNameToId 'tailE)
            , XlatEntry (thNameToId 'length)
                        (thNameToId 'len)
            , XlatEntry (thNameToId 'drop)
                        (thNameToId 'dropE)
            , XlatEntry (thNameToId 'take)
                        (thNameToId 'takeE)
            , XlatEntry (thNameToId 'Data.List.reverse)
                        (thNameToId 'reverseE)
            , XlatEntry (thNameToId 'null)
                        (thNameToId 'nullE)
            , XlatEntry (thNameToId 'showB)
                        (thNameToId 'showE)
            , XlatEntry (thNameToId 'fromIntegral)
                        (thNameToId 'fromIntegralE)
           ]
-- TBD add floating to above

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
changeRep (NonRec b e) = do
  let (bs, e') = collectBinders e
  e'' <- changeRepExpr e'
  let e''' = mkLams bs e''
  return (NonRec b e''')
changeRep (Rec bs) = do
  bs' <- changeRep' bs
  return $ Rec bs'

changeRep' :: [(Id, CoreExpr)] -> BindM [(Id, CoreExpr)]
changeRep' [] = return []
changeRep' ((b, e) : bs) = do
  let (lbs, e') = collectBinders e
  e'' <- changeRepExpr e'
  let e''' = mkLams lbs e''
  bs' <- changeRep' bs
  return $ (b, e''') : bs'

changeRepExpr :: CoreExpr -> BindM CoreExpr
changeRepExpr e = do
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
            [_ty, _dict, e'] -> do
              let (b', args') = collectArgs e'
              case b' of
                Var v' -> do
                  inList <- funcInXlatList v'
                  case inList of
                    Just xe -> pushRep xe args' (exprType e)
                    _ -> defaultReturn
                _ -> defaultReturn
            _ -> defaultReturn
        _ -> defaultReturn
    Lam tb el -> do
      e' <- changeRepExpr el
      return $ Lam tb e'
    Let bind body -> do
      body' <- changeRepExpr body
      bind' <- case bind of
                  (NonRec v el) -> do
                    e' <- changeRepExpr el
                    return $ NonRec v e'
                  (Rec rbs) -> do
                    rbs' <- changeRepExpr' rbs
                    return $ Rec rbs'
      return $ Let bind' body'
    Case ec tb ty alts -> do
      e' <- changeRepExpr ec
      alts' <- changeRepExprAlts alts
      return $ Case e' tb ty alts'
    Tick t et -> do
      e' <- changeRepExpr et
      return $ Tick t e'
    Cast ec co -> do
      e' <- changeRepExpr ec
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
funcInXlatList idf = do
  funcInXlatList' idf xlatList
    where
      funcInXlatList' :: Id -> [XlatEntry] -> BindM (Maybe XlatEntry)
      funcInXlatList' _ [] = return Nothing
      funcInXlatList' idf' (xl:xls) = do
          fId <- fromId xl
          if fId == idf'
          then return $ Just xl
          else funcInXlatList' idf' xls

pushRep :: XlatEntry -> [CoreExpr] -> Type -> BindM CoreExpr
pushRep xe args origRTy = do
    fi <- fromId xe
    ti <- toId xe
    -- Break down the arguments from the old function
    -- into foralls, args, and return types.
    let (_fromForAlls, fromFuncTy) = splitForAllTys $ idType fi
    let (_fromArgTys, fromRetTy) = splitFunTys fromFuncTy
    let (toForAlls, toFuncTy) = splitForAllTys $ idType ti
    let (toArgTys, toRetTy) = splitFunTys toFuncTy
    -- Get the count of non-dictionary args in the new function
    let argCount  = countNonDictTypes toArgTys
    let dictCount = (length toArgTys) - argCount
    -- Get the original args based on the argCount
    let someArgs = take ((length args) - argCount) args
    let origArgs = drop ((length args) - argCount) args
    let dictTys = take dictCount toArgTys
    let nonDictTys = drop dictCount toArgTys

    let typeArgs = genForAllArgs toForAlls nonDictTys fromRetTy origArgs
    exprTypeArgs <- mapM (thNameTyToTyConApp exprTyConTH) typeArgs
    let exprTypeVars = map Type exprTypeArgs
    dictArgs <- genDictArgs dictTys nonDictTys fromRetTy toRetTy origArgs origRTy
    repArgs <- mapM repExpr origArgs
    repArgs' <- mapM changeRepExpr repArgs
    return $ mkCoreApps (Var ti) (exprTypeVars ++ dictArgs ++ repArgs')

genDictArgs :: [Type] -> [Type] -> Type -> Type -> [CoreExpr] -> Type -> BindM [CoreExpr]
genDictArgs [] _ _ _ _ _ = return []
genDictArgs (dty:dtys) tys frty trty args orty = do
    let (tyConTy, ty') = splitAppTys dty
    dictTys <- mapM findTypeMatch ty'
    dict <- buildDictionaryTyConTs (tyConAppTyCon tyConTy) dictTys
    dicts <- genDictArgs dtys tys frty trty args orty
    return $ dict:dicts
  where
    findTypeMatch :: Type -> BindM Type
    findTypeMatch fty =
      case findIndex (typeIn fty) tys of
        Just idx -> do
          -- Find the index of the from function arg which matches the
          -- type required by the dictionary
          let fromArgTy = exprType $ args !! idx
          -- Get the base type only of the from type, removing any
          -- Expr if it already exists.
          let dictTy = case splitTyConApp_maybe fromArgTy of
                         Just (_, [ty']) -> ty'
                         _               -> fromArgTy
          -- Get the type of the to function arg at the same index
          let toArgTy = tys !! idx
          -- Determine if it has a type constructor
          let tys_m = splitTyConApp_maybe toArgTy
          case tys_m of
             -- If it does, we are dealing with a function
             -- of type Class a => Expr a -> ... -> retType
             -- so the dictionary type is of a not Expr a
             Just (_tyCon, _tys') -> return dictTy
             -- If there is no TyCon, then we have a
             -- function of type Class a => a -> ... -> retType
             -- so the dictionary type needs to be Expr a.
             Nothing -> thNameTyToTyConApp exprTyConTH dictTy
        Nothing  -> do
          -- Determine if the return type has a type constructor
          let tys_m = splitTyConApp_maybe trty
          -- Get the base type only of the from type, removing any
          -- Expr if it already exists.
          let orty' = case splitTyConApp_maybe orty of
                         Just (_, [ty']) -> ty'
                         _               -> orty
          case tys_m of
             -- If it does, we are dealing with a function
             -- of type Class a => Expr a -> ... -> Expr retType
             -- so the dictionary type is of a not Expr a
             Just (_tyCon, _tys') -> return orty'
             -- If there is no TyCon, then we have a
             -- function of type Class a => a -> ... -> retType
             -- so the dictionary type needs to be Expr a.
             Nothing -> thNameTyToTyConApp exprTyConTH orty'

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
      let (_, tys') = splitTyConApp ty in
      t `eqType` last tys'
