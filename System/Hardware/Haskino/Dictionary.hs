-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Dictionary
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- GHC Dictionary Utility Functions
-------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.Dictionary (buildDictionaryT, PassCoreM(..)) where

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
import Encoding (zEncodeString)

import System.Hardware.Haskino.Typechecker (initTcFromModGuts)

class (Monad m, MonadIO m) => PassCoreM m where
    -- | 'CoreM' can be lifted to this monad.
    liftCoreM  :: CoreM a -> m a
    getModGuts :: m ModGuts

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
