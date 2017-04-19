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
import System.Hardware.Haskino.ShallowDeepPlugin.BindChangeAppPass
import System.Hardware.Haskino.ShallowDeepPlugin.BindChangeArgPass
import System.Hardware.Haskino.ShallowDeepPlugin.CommProcPass
import System.Hardware.Haskino.ShallowDeepPlugin.CondPass
import System.Hardware.Haskino.ShallowDeepPlugin.RecurPass
import System.Hardware.Haskino.ShallowDeepPlugin.RepAbsFusePass
import System.Hardware.Haskino.ShallowDeepPlugin.RepPushPass
import System.Hardware.Haskino.ShallowDeepPlugin.ReturnsPass
import System.Hardware.Haskino.ShallowDeepPlugin.CoreShow

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  liftIO initStaticOpts   -- This is required on Windows
  reinitializeGlobals
  let absLambdaToDo = [CoreDoPluginPass "AbsLambda" absLambdaPass]
  let condToDo = [CoreDoPluginPass "CondTransform" condPass]
  let recurToDo = [CoreDoPluginPass "RecursionTransform" recurPass]
  let returnsToDo = [CoreDoPluginPass "ReturnsTransform" returnsPass]
  let bindAppToDo = [CoreDoPluginPass "BindAppTransform" bindChangeAppPass]
  let bindArgRetToDo = [CoreDoPluginPass "BindArgRetTransform" bindChangeArgRetPass]
  let commProcToDo = [CoreDoPluginPass "CommProcTransform" commProcPass]
  let repPushToDo = [CoreDoPluginPass "RepPush" repPushPass]
  let repAbsFuseToDo = [CoreDoPluginPass "RepAbsFuse" repAbsFusePass]
  let dumpToDo = [CoreDoPluginPass "DumpPass" dumpPass]
  let showToDo = [CoreDoPluginPass "ShowPass" showPass]
  return $ [simplPass] ++ recurToDo ++ condToDo ++ commProcToDo ++ returnsToDo ++ 
           bindArgRetToDo ++ bindAppToDo ++
           repPushToDo ++ absLambdaToDo ++  
           repAbsFuseToDo ++ todo -- ++ dumpToDo
{-
  -- The following version of the return uses rules passes to do the repPush
  -- and repAbsFuse passes.  This version only works with optimization off
  -- right now, since doing the rules1Pass will also cause the optimzations
  -- to occur, and the unspecialized binds that the absLambaPass keys off
  -- of are eliminated by the optimizer.
  return $ [simplPass] ++ condToDo ++ commProcToDo ++ returnsToDo ++ 
           bindRetToDo ++ bindArgToDo ++ bindAppToDo ++
           [rules1Pass] ++ absLambdaToDo ++  
           [rules0Pass] ++ todo -- ++ dumpToDo
-}

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

rules0Pass :: CoreToDo
rules0Pass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 0,
            sm_rules = True,
            sm_inline = True,
            sm_case_case = False,
            sm_eta_expand = False
            }

rules1Pass :: CoreToDo
rules1Pass = CoreDoSimplify 2 SimplMode {
            sm_names = [],
            sm_phase = Phase 1,
            sm_rules = True,
            sm_inline = False,
            sm_case_case = False,
            sm_eta_expand = False
            }

dumpPass :: ModGuts -> CoreM ModGuts
dumpPass guts = do
  putMsg $ ppr (mg_binds guts)
  return guts

