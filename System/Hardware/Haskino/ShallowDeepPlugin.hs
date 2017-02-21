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

import StaticFlags
import CoreMonad
import GhcPlugins
import System.Hardware.Haskino.AbsLambdaPass 
import System.Hardware.Haskino.BindChangeAppPass 
import System.Hardware.Haskino.BindChangeArgPass 
import System.Hardware.Haskino.BindChangeRetPass 
import System.Hardware.Haskino.CondPass 
import System.Hardware.Haskino.CondRetPass 
import System.Hardware.Haskino.ReturnsPass 
import System.Hardware.Haskino.CommProcPass


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
  let condRetToDo = [CoreDoPluginPass "CondRetTransform" condRetPass]
  let returnsToDo = [CoreDoPluginPass "ReturnsTransform" returnsPass]
  let bindRetToDo = [CoreDoPluginPass "BindRetTransform" bindChangeRetPass]
  let bindAppToDo = [CoreDoPluginPass "BindAppTransform" bindChangeAppPass]
  let bindArgToDo = [CoreDoPluginPass "BindArgTransform" bindChangeArgPass]
  let commProcToDo = [CoreDoPluginPass "CommProcTransform" commProcPass]
  let dumpToDo = [CoreDoPluginPass "DumpPass" dumpPass]
  return $ condToDo ++ commProcToDo ++ returnsToDo ++
           [rules1Pass] ++ absLambdaToDo ++ condRetToDo ++ 
           bindRetToDo ++ bindArgToDo ++ bindAppToDo ++ 
           [rules1Pass] ++ absLambdaToDo ++ 
           [rules0Pass] ++ todo -- ++ dumpToDo

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

rules2Pass :: CoreToDo
rules2Pass = CoreDoSimplify 1 SimplMode {
            sm_names = [],
            sm_phase = Phase 2,
            sm_rules = True,
            sm_inline = False,
            sm_case_case = False,
            sm_eta_expand = False
            }

dumpPass :: ModGuts -> CoreM ModGuts
dumpPass guts = do
  putMsgS "In dumpPass"
  putMsg $ ppr (mg_binds guts)
  return guts      
