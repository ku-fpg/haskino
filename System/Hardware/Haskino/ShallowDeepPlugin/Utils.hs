-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin.Utils
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Shallow Deep Plugin Utility Functions
-------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin.Utils (buildDictionaryT,
                                           buildDictionaryTyConT,
                                           PassCoreM(..),
                                           thNameToId,
                                           thNameToTyCon,
                                           -- DSL specific names
                                           exprClassTyConTH,
                                           exprTyConTH,
                                           monadCondTyConTH,
                                           monadTyConTH,
                                           absNameTH,
                                           repNameTH, 
                                           ifThenElseNameTH,
                                           ifThenElseUnitNameTH,
                                           -- General Haskell names
                                           bindNameTH,
                                           functTyConTH,
                                           unitTyConTH,
                                           bindThenNameTH,
                                           falseNameTH,
                                           fmapNameTH,
                                           returnNameTH) where

import CoreMonad
import GhcPlugins
import HscTypes
import Data.Char
import TcRnMonad
import TcSMonad
import TcSimplify
import TcEvidence
import ErrUtils
import DsBinds
import DsMonad (initDsTc)
import Control.Arrow (first, second)
import Control.Monad
import Data.Functor
import Encoding (zEncodeString)
import qualified Language.Haskell.TH as TH

import System.Hardware.Haskino.ShallowDeepPlugin.Typechecker (initTcFromModGuts)

-- The following line contain the imports specific to the DSL language
-- being trnasformed, as well as Template Haskell definintions of the
-- DSL Monad and Expr types, names for the Worker/Wrapper abs/rep,
-- and names of the conditionals in the DSL.
import qualified System.Hardware.Haskino

exprClassTyConTH     = ''System.Hardware.Haskino.ExprB
exprTyConTH          = ''System.Hardware.Haskino.Expr
monadCondTyConTH     = ''System.Hardware.Haskino.ArduinoConditional
monadTyConTH         = ''System.Hardware.Haskino.Arduino
absNameTH            = 'System.Hardware.Haskino.abs_
repNameTH            = 'System.Hardware.Haskino.rep_
ifThenElseNameTH     = 'System.Hardware.Haskino.ifThenElseE
ifThenElseUnitNameTH = 'System.Hardware.Haskino.ifThenElseUnitE

-- The following lines contain definitions of Template Haskell namde
-- for standard Haskell functions.
functTyConTH         = ''Data.Functor.Functor
unitTyConTH          = ''()
bindNameTH           = '(>>=)
bindThenNameTH       = '(>>)
falseNameTH          = 'Prelude.False
fmapNameTH           = '(<$>)
returnNameTH         = 'Prelude.return

class (Monad m, MonadIO m) => PassCoreM m where
    -- | 'CoreM' can be lifted to this monad.
    liftCoreM  :: CoreM a -> m a
    getModGuts :: m ModGuts

thNameToId :: PassCoreM m => TH.Name -> m Id
thNameToId n = do
  (Just name) <- liftCoreM $ thNameToGhcName n
  liftCoreM $ lookupId name

thNameToTyCon :: PassCoreM m => TH.Name -> m TyCon
thNameToTyCon n = do
  (Just name) <- liftCoreM $ thNameToGhcName n
  liftCoreM $ lookupTyCon name

-- Adapted from HERMIT.Monad
runTcM :: PassCoreM m => TcM a -> m a
runTcM m = do
    env <- liftCoreM getHscEnv
    dflags <- liftCoreM getDynFlags
    guts <- getModGuts
    (msgs, mr) <- liftIO $ initTcFromModGuts env guts HsSrcFile False m
    let showMsgs (warns, errs) = showSDoc dflags $ vcat
                                                 $    text "Errors:" : pprErrMsgBagWithLoc errs
                                                   ++ text "Warnings:" : pprErrMsgBagWithLoc warns
    maybe (fail $ showMsgs msgs) return mr

newCondName :: PassCoreM m => String -> m Name
newCondName nm = mkSystemVarName <$> (liftCoreM getUniqueM) <*> return (mkFastString nm)

newIdH :: PassCoreM m => String -> Type -> m Id
newIdH name ty = do name' <- newCondName name
                    return $ mkLocalId name' ty

-- Adapted from HERMIT
buildDictionary :: PassCoreM m => Id -> m (Id, [CoreBind])
buildDictionary evar = do
    runTcM $ do
#if __GLASGOW_HASKELL__ > 710
        loc <- getCtLocM (GivenOrigin UnkSkol) Nothing
#else
        loc <- getCtLoc $ GivenOrigin UnkSkol
#endif
        let predTy = varType evar
#if __GLASGOW_HASKELL__ > 710
            nonC = mkNonCanonical $ CtWanted { ctev_pred = predTy, ctev_dest = EvVarDest evar, ctev_loc = loc }
            wCs = mkSimpleWC [cc_ev nonC]
        (_wCs', bnds) <- second evBindMapBinds <$> runTcS (solveWanteds wCs)
#else
            nonC = mkNonCanonical $ CtWanted { ctev_pred = predTy, ctev_evar = evar, ctev_loc = loc }
            wCs = mkSimpleWC [nonC]
        (_wCs', bnds) <- solveWantedsTcM wCs
#endif
        bnds1 <- initDsTc $ dsEvBinds bnds
        return (evar, bnds1)

buildDictionaryT :: PassCoreM m => Type -> m CoreExpr
buildDictionaryT ty = do
    dflags <- liftCoreM getDynFlags
    binder <- newIdH ("$d" ++ zEncodeString (filter (not . isSpace) (showPpr dflags ty))) ty
    (i,bnds) <- buildDictionary binder
    return $ case bnds of
                [NonRec v e] | i == v -> e -- the common case that we would have gotten a single non-recursive let
                _ -> mkCoreLets bnds (varToCoreExpr i)

buildDictionaryTyConT :: PassCoreM m => TyCon -> Type -> m CoreExpr
buildDictionaryTyConT tyCon ty =
    buildDictionaryT $ GhcPlugins.mkTyConApp tyCon [ty]
