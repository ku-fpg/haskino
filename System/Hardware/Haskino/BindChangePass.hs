-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.BindChangePass
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Worker-Wrapper push through lambda pass
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.BindChangePass (bindChangePass) where

import CoreMonad
import CoreSyn
import GhcPlugins
import OccName
import Var

bindChangePass :: ModGuts -> CoreM ModGuts
bindChangePass guts = bindsOnlyPass (mapM changeBind) guts

changeBind :: CoreBind -> CoreM CoreBind
changeBind bndr@(NonRec b e) = do
  df <- getDynFlags
  let (argTys, retTy) = splitFunTys $ exprType e
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
                        putMsg $ ppr b
                        putMsg $ ppr retTyCon'
                        return bndr
              _ -> return bndr
      _ -> return bndr
changeBind (Rec bs) = do
  return $ Rec bs
