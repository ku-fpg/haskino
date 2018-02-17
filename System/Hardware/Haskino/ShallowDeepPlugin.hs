-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.ShallowDeepPlugin
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- Shallow Deep Transformation Compiler Plugin
-------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module System.Hardware.Haskino.ShallowDeepPlugin (plugin) where

import StaticFlags   -- This is required on Windows
import CoreMonad
import GhcPlugins
import System.Hardware.Haskino.ShallowDeepPlugin.AbsLambdaPass
import System.Hardware.Haskino.ShallowDeepPlugin.AbsThenPass
import System.Hardware.Haskino.ShallowDeepPlugin.ApRemovePass
import System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass
import System.Hardware.Haskino.ShallowDeepPlugin.CommProcPass
import System.Hardware.Haskino.ShallowDeepPlugin.CondPass
import System.Hardware.Haskino.ShallowDeepPlugin.MutRecurPass
import System.Hardware.Haskino.ShallowDeepPlugin.RecurPass
import System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass
import System.Hardware.Haskino.ShallowDeepPlugin.RepCasePushPass
import System.Hardware.Haskino.ShallowDeepPlugin.RepConstrPushPass
import System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
import System.Hardware.Haskino.ShallowDeepPlugin.ReturnsPass
-- import System.Hardware.Haskino.ShallowDeepPlugin.CoreShow

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  liftIO initStaticOpts   -- This is required on Windows
  reinitializeGlobals
  let absLambdaToDo = [CoreDoPluginPass "AbsLambda" absLambdaPass]
  let apRemoveToDo = [CoreDoPluginPass "ApRemove" apRemovePass]
  let condToDo = [CoreDoPluginPass "CondTransform" condPass]
  let mutRecurToDo = [CoreDoPluginPass "MutRecursionTransform" mutRecurPass]
  let recurToDo = [CoreDoPluginPass "RecursionTransform" recurPass]
  let returnsToDo = [CoreDoPluginPass "ReturnsTransform" returnsPass]
  let bindArgRetAppToDo = [CoreDoPluginPass "BindArgRetAppTransform" bindChangeArgRetAppPass]
  let commProcToDo = [CoreDoPluginPass "CommProcTransform" commProcPass]
  let repConstrPushToDo = [CoreDoPluginPass "RepConstrPush" repConstrPushPass]
  let repCasePushToDo = [CoreDoPluginPass "RepCasePush" repCasePushPass]
  let repPushToDo = [CoreDoPluginPass "RepPush" repPushPass]
  let repAbsFuseToDo = [CoreDoPluginPass "RepAbsFuse" repAbsFusePass]
  let absThenToDo = [CoreDoPluginPass "AbsThen" absThenPass]
  -- The following passes are used for debugging.  dumpToDo dumps standard Core
  --   and showToDo dumps very detailed data on the Core.  To use them in the
  --   pass sequence they need to be uncommented.
  -- let dumpToDo = [CoreDoPluginPass "DumpPass" dumpPass]
  -- let showToDo = [CoreDoPluginPass "ShowPass" showPass]

  return $ [simplPass] ++ apRemoveToDo ++ condToDo ++ commProcToDo ++ returnsToDo ++
           bindArgRetAppToDo ++ repCasePushToDo ++ repConstrPushToDo ++ repPushToDo ++ 
           absLambdaToDo ++ repAbsFuseToDo ++ mutRecurToDo ++
           absThenToDo ++ todo -- ++ dumpToDo
--
-- This pass is needed to simplify inlined applications that may be introduced
-- by the compiler to inline single use let statements before it passes us
-- the Core.
simplPass :: CoreToDo
simplPass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 2,
            sm_rules = False,
            sm_inline = False,
            sm_case_case = False,
            sm_eta_expand = False
            }
{-
dumpPass :: ModGuts -> CoreM ModGuts
dumpPass guts = do
  putMsg $ ppr (mg_binds guts)
  return guts
-}
