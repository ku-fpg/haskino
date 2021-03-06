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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitForAll #-}

module System.Hardware.Haskino.ShallowDeepPlugin.Utils (absExpr,
                                           absVar,
                                           anyExprClassType,
                                           buildDictionaryT,
                                           buildDictionaryTyConT,
                                           buildDictionaryTyConTs,
                                           buildId,
                                           collectLets,
                                           fmapAbsExpr,
                                           fmapRepBindReturn,
                                           fmapRepExpr,
                                           isExprClassType,
                                           isBindTopLevel,
                                           modId,
                                           repExpr,
                                           stringToId,
                                           stringToId_maybe,
                                           thNameToId,
                                           thNameToTyCon,
                                           thNameTyToDict,
                                           thNameTysToDict,
                                           thNameTyToTyConApp,
                                           PassCoreM(..),
                                           pattern (:$),
                                           varString,
                                           -- DSL specific names
                                           eitherTyConTH,
                                           exprClassTyConTH,
                                           exprTyConTH,
                                           monadCondTyConTH,
                                           monadIterateTyConTH,
                                           monadTyConTH,
                                           absNameTH,
                                           eqNameTH,
                                           repNameTH,
                                           ifThenElseNameTH,
                                           ifThenElseEitherNameTH,
                                           ifBNameTH,
                                           iterateETH,
                                           leftNameTH,
                                           leftZeroNameTH,
                                           litZeroNameTH,
                                           rightNameTH,
                                           -- General Haskell names
                                           apNameTH,
                                           andNameTH,
                                           bindNameTH,
                                           functTyConTH,
                                           intTyConTH,
                                           listTyConTH,
                                           unitTyConTH,
                                           unitValueTH,
                                           bindThenNameTH,
                                           falseNameTH,
                                           fmapNameTH,
                                           monadClassTyConTH,
                                           notNameTH,
                                           returnNameTH) where

import           Control.Arrow       (second)
import           Data.Char
import           Data.Functor
import           DsBinds
import           DsMonad             (initDsTc)
import           Encoding            (zEncodeString)
import           ErrUtils
import           GhcPlugins
import qualified Language.Haskell.TH as TH
import           MonadUtils
import           OccName
import           TcRnMonad
import           TcSMonad
import           TcSimplify
import           TcEvidence
import           Var

import System.Hardware.Haskino.ShallowDeepPlugin.Typechecker (initTcFromModGuts)

-- The following line contain the imports specific to the DSL language
-- being trnasformed, as well as Template Haskell definintions of the
-- DSL Monad and Expr types, names for the Worker/Wrapper abs/rep,
-- and names of the conditionals in the DSL.
import qualified System.Hardware.Haskino

eitherTyConTH :: TH.Name
eitherTyConTH          = ''System.Hardware.Haskino.ExprEither
exprClassTyConTH :: TH.Name
exprClassTyConTH       = ''System.Hardware.Haskino.ExprB
exprTyConTH :: TH.Name
exprTyConTH            = ''System.Hardware.Haskino.Expr
monadCondTyConTH :: TH.Name
monadCondTyConTH       = ''System.Hardware.Haskino.ArduinoConditional
monadTyConTH :: TH.Name
monadTyConTH           = ''System.Hardware.Haskino.Arduino
monadIterateTyConTH :: TH.Name
monadIterateTyConTH    = ''System.Hardware.Haskino.ArduinoIterate
absNameTH :: TH.Name
absNameTH              = 'System.Hardware.Haskino.abs_
eqNameTH :: TH.Name
eqNameTH              = 'System.Hardware.Haskino.eqE
repNameTH :: TH.Name
repNameTH              = 'System.Hardware.Haskino.rep_
ifThenElseNameTH :: TH.Name
ifThenElseNameTH       = 'System.Hardware.Haskino.ifThenElseE
ifThenElseEitherNameTH :: TH.Name
ifThenElseEitherNameTH = 'System.Hardware.Haskino.ifThenElseEither
ifBNameTH :: TH.Name
ifBNameTH              = 'System.Hardware.Haskino.ifBE
iterateETH :: TH.Name
iterateETH             = 'System.Hardware.Haskino.iterateE
leftNameTH :: TH.Name
leftNameTH             = 'System.Hardware.Haskino.ExprLeft
leftZeroNameTH :: TH.Name
leftZeroNameTH         = 'System.Hardware.Haskino.exprLeft
litZeroNameTH :: TH.Name
litZeroNameTH         = 'System.Hardware.Haskino.litZero
rightNameTH :: TH.Name
rightNameTH            = 'System.Hardware.Haskino.ExprRight
litUnitNameTH :: TH.Name
litUnitNameTH          = 'System.Hardware.Haskino.LitUnit
litBNameTH :: TH.Name
litBNameTH             = 'System.Hardware.Haskino.LitB
litW8NameTH :: TH.Name
litW8NameTH            = 'System.Hardware.Haskino.LitW8
litW16NameTH :: TH.Name
litW16NameTH           = 'System.Hardware.Haskino.LitW16
litW32NameTH :: TH.Name
litW32NameTH           = 'System.Hardware.Haskino.LitW32
litI8NameTH :: TH.Name
litI8NameTH            = 'System.Hardware.Haskino.LitI8
litI16NameTH :: TH.Name
litI16NameTH           = 'System.Hardware.Haskino.LitI16
litI32NameTH :: TH.Name
litI32NameTH           = 'System.Hardware.Haskino.LitI32
litINameTH :: TH.Name
litINameTH             = 'System.Hardware.Haskino.LitI
litL8NameTH :: TH.Name
litL8NameTH            = 'System.Hardware.Haskino.LitList8
litFloatNameTH :: TH.Name
litFloatNameTH         = 'System.Hardware.Haskino.LitFloat
litPinModeNameTH :: TH.Name
litPinModeNameTH       = 'System.Hardware.Haskino.LitPinMode

exprClassNames :: [TH.Name]
exprClassNames = [litUnitNameTH, litBNameTH, litW8NameTH, litW16NameTH,
                  litW32NameTH, litI8NameTH, litI16NameTH, litI32NameTH,
                  litINameTH, litL8NameTH, litFloatNameTH, litPinModeNameTH]

isExprClassType :: PassCoreM m => Type -> m Bool
isExprClassType ty = do
    ectys <- exprClassTypes
    return $ any (eqType ty) ectys
  where
    exprClassTypes :: PassCoreM m => m [Type]
    exprClassTypes = mapM thNameToReturnBaseType exprClassNames

anyExprClassType :: PassCoreM m => [Type] -> m Bool
anyExprClassType tys = do
    anyM isExprClassType tys

isBindTopLevel :: Var -> Bool
isBindTopLevel b =
    case unfoldingInfo (idInfo b) of
        cu@CoreUnfolding{ } -> uf_is_top cu
        _                   -> False

-- The following lines contain definitions of Template Haskell namde
-- for standard Haskell functions.
functTyConTH :: TH.Name
functTyConTH         = ''Data.Functor.Functor
monadClassTyConTH :: TH.Name
monadClassTyConTH    = ''Prelude.Monad
unitTyConTH :: TH.Name
unitTyConTH          = ''()
unitValueTH :: TH.Name
unitValueTH          = '()
listTyConTH :: TH.Name
listTyConTH          = ''[]
intTyConTH :: TH.Name
intTyConTH           = ''Int
bindNameTH :: TH.Name
bindNameTH           = '(>>=)
bindThenNameTH :: TH.Name
bindThenNameTH       = '(>>)
falseNameTH :: TH.Name
falseNameTH          = 'Prelude.False
trueNameTH :: TH.Name
trueNameTH           = 'Prelude.True
fmapNameTH :: TH.Name
fmapNameTH           = '(<$>)
apNameTH :: TH.Name
apNameTH             = '($)
returnNameTH :: TH.Name
returnNameTH         = 'Prelude.return
notNameTH :: TH.Name
notNameTH            = 'not
andNameTH :: TH.Name
andNameTH            = '(&&)

-- An infix pattern synonym for `App` to make applications with multiple
-- arguments easier to manipulate:
infixl 0 :$
pattern (:$) :: forall t. Expr t -> Arg t -> Expr t
pattern f :$ x = App f x

class (Monad m, MonadIO m) => PassCoreM m where
    -- | 'CoreM' can be lifted to this monad.
    liftCoreM  :: CoreM a -> m a
    getModGuts :: m ModGuts

instance PassCoreM CoreM where
  liftCoreM = id
  getModGuts = error "Cannot get modguts from CoreM"

thNameToId :: PassCoreM m => TH.Name -> m Id
thNameToId n = do
  name_m <- liftCoreM $ thNameToGhcName n
  case name_m of
    (Just name) -> liftCoreM $ lookupId name
    _           -> error "Unable to Lookup ID"

stringToId_maybe :: PassCoreM m => String -> m (Maybe Id)
stringToId_maybe str = do
  let lookId x = do
        id' <- liftCoreM $ lookupId $ gre_name x
        return $ Just id'
  guts <- getModGuts
  let gres = lookupGlobalRdrEnv (mg_rdr_env guts) (mkVarOcc str)
  case gres of
    []   -> return Nothing
    [x]  -> lookId x
    x:_  -> lookId x -- Need to fix this, if there are multiples, need to
                     -- find one we are looking for.

stringToId :: PassCoreM m => String -> m Id
stringToId str = do
  id_m <- stringToId_maybe str
  case id_m of
    (Just id') -> return id'
    _          -> error $ "Error unable to Lookup ID " ++ str ++ "."

thNameToTyCon :: PassCoreM m => TH.Name -> m TyCon
thNameToTyCon n = do
  name_m <- liftCoreM $ thNameToGhcName n
  case name_m of
    (Just name) -> liftCoreM $ lookupTyCon name
    _           -> error "Unable to Lookup TyCon"

thNameTyToDict :: PassCoreM m => TH.Name -> Type -> m CoreExpr
thNameTyToDict n ty = do
  tyCon <- thNameToTyCon n
  buildDictionaryTyConT tyCon ty

thNameTysToDict :: PassCoreM m => TH.Name -> [Type] -> m CoreExpr
thNameTysToDict n tys = do
  tyCon <- thNameToTyCon n
  buildDictionaryTyConTs tyCon tys

thNameTyToTyConApp :: PassCoreM m => TH.Name -> Type -> m Type
thNameTyToTyConApp n ty = do
  tyCon <- thNameToTyCon n
  return $  mkTyConApp tyCon [ty]

thNameToReturnBaseType :: PassCoreM m => TH.Name -> m Type
thNameToReturnBaseType th = do
    id' <- thNameToId th
    let (_, retTy) = splitFunTys $ varType id'
    let (_, [baseTy]) = splitTyConApp retTy
    return baseTy

varString :: Id -> String
varString = occNameString . nameOccName . Var.varName

buildId :: PassCoreM m => String -> Type -> m Id
buildId varName' typ = do
  dunique <- liftCoreM getUniqueM
  let name = mkInternalName dunique (mkOccName OccName.varName varName') noSrcSpan
  return $ mkLocalVar VanillaId name typ vanillaIdInfo

modId :: PassCoreM m => Id -> String -> m Id
modId v s = do
  dunique <- liftCoreM getUniqueM
  guts <- getModGuts
  let newString = (varString v) ++ s
  let newName' = mkOccName OccName.varName newString
  let name = mkExternalName dunique (mg_module guts) newName' noSrcSpan
  let v' = setIdUnique (setVarName v name) dunique
  return v'

repExpr :: PassCoreM m => CoreExpr -> m CoreExpr
repExpr e = do
    case e of
        Let bind body -> do
            body' <- repExpr body
            return $ Let bind body'
        _ -> do
            let ty = exprType e
            repId <- thNameToId repNameTH
            repDict <- thNameTyToDict exprClassTyConTH ty
            return $ mkCoreApps (Var repId) [Type ty, repDict, e]

absExpr :: PassCoreM m => CoreExpr -> m CoreExpr
absExpr e = do
    case e of
        Let bind body -> do
            body' <- absExpr body
            return $ Let bind body'
        _ -> do
            let (_, [ty']) = splitTyConApp $ exprType e
            absId <- thNameToId absNameTH
            return $ mkCoreApps (Var absId) [Type ty', e]

absVar :: PassCoreM m => Id -> m CoreExpr
absVar v = do
    exprTyCon <- thNameToTyCon exprTyConTH
    let exprTyConApp = mkTyConApp exprTyCon [varType v]
    absId <- thNameToId absNameTH
    return $ mkCoreApps (Var absId) [Type exprTyConApp, Var v]

fmapAbsExpr :: PassCoreM m => Type -> Type -> CoreExpr -> m CoreExpr
fmapAbsExpr tyConTy ty e = do
    absId <- thNameToId absNameTH

    exprTyConApp <- thNameTyToTyConApp exprTyConTH ty

    fmapId <- thNameToId fmapNameTH
    functDict <- thNameTyToDict functTyConTH tyConTy

    let absApp = mkCoreApps (Var absId) [Type ty]
    return $ mkCoreApps (Var fmapId) [Type tyConTy, Type exprTyConApp, Type ty,
                                      functDict, absApp, e]

fmapRepExpr :: PassCoreM m => Type -> Type -> CoreExpr -> m CoreExpr
fmapRepExpr tyConTy ty e = do
    repId <- thNameToId repNameTH
    repDict <- thNameTyToDict exprClassTyConTH ty

    exprTyConApp <- thNameTyToTyConApp exprTyConTH ty

    fmapId <- thNameToId fmapNameTH
    functDict <- thNameTyToDict functTyConTH tyConTy

    let repApp = mkCoreApps (Var repId) [Type ty, repDict]
    return $ mkCoreApps (Var fmapId) [Type tyConTy, Type ty, Type exprTyConApp,
                                      functDict, repApp, e]

fmapRepBindReturn :: PassCoreM m => CoreExpr -> m CoreExpr
fmapRepBindReturn e = do
    ee <- fmapRepBindReturn' e
    case ee of
      Right e' -> return e'
      -- The following case should not happen.
      Left e' -> return e'
  where
    fmapRepBindReturn' :: PassCoreM m => CoreExpr -> m (Either CoreExpr CoreExpr)
    fmapRepBindReturn' e1 = do
        let (ls, e')  = collectLets e1
        let (bs, e'') = collectBinders e'
        let (f, args) = collectArgs e''
        bindId <- thNameToId bindNameTH
        thenId <- thNameToId bindThenNameTH
        case f of
          Var fv -> do
            if fv == bindId || fv == thenId
            then do
                rla <- fmapRepBindReturn' $ last args
                case rla of
                  -- If applicationg of rep_ at next was successful
                  Right la' -> do
                    let args' = init args ++ [la']
                    return $ Right $ mkLets ls $ mkLams bs (mkCoreApps f args')
                  -- Application of rep at next level was not successful
                  -- so apply it at this level.
                  Left _ -> fmapRepReturn ls bs e''
            else fmapRepReturn ls bs e''
          Case ec tb ty alts -> do
            alts' <- fmapRepBindReturnAlts alts
            return $ Right $ Case ec tb ty alts'
          Let lb lbody -> do
            lbody' <- fmapRepBindReturn lbody
            return $ Right $ Let lb lbody'
          _ -> return $ Right e1

    -- Apply rep_ <$> to the end of the bind chain if possible.
    -- If the end is a partially applied function, then rep_ <$>
    -- will need to be applied one level
    fmapRepReturn :: PassCoreM m => [CoreBind] -> [CoreBndr] -> CoreExpr -> m (Either CoreExpr CoreExpr)
    fmapRepReturn ls bs e' = do
        let tyCon_m = splitTyConApp_maybe $ exprType e'
        case tyCon_m of
          Just (tyCon,[ty]) -> do
              retExpr <- fmapRepExpr (mkTyConTy tyCon) ty e'
              return $ Right $ mkLets ls $ mkLams bs retExpr
          _ -> return $ Left $ e'

    fmapRepBindReturnAlts :: PassCoreM m => [GhcPlugins.Alt CoreBndr] -> m [GhcPlugins.Alt CoreBndr]
    fmapRepBindReturnAlts [] = return []
    fmapRepBindReturnAlts ((ac, b, a) : as) = do
      a' <- fmapRepBindReturn a
      as' <- fmapRepBindReturnAlts as
      return $ (ac, b, a') : as'

collectLets :: CoreExpr -> ([CoreBind], CoreExpr)
collectLets (Let b e) = let (bs,expr) = collectLets e in (b:bs, expr)
collectLets expr      = ([],expr)

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
    buildDictionaryTyConTs tyCon [ty]

buildDictionaryTyConTs :: PassCoreM m => TyCon -> [Type] -> m CoreExpr
buildDictionaryTyConTs tyCon tys =
    buildDictionaryT $ GhcPlugins.mkTyConApp tyCon tys
