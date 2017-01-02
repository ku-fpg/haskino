-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Protocol
--                Based on System.Hardware.Arduino.Protocol
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the Haskino Firmware protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Show where

import Data.Bits (xor,shiftR)
import Data.Int (Int8, Int16, Int32)
import Data.List
import Data.Word (Word8, Word16, Word32)

import Control.Monad.State

import Control.Remote.Monad
import Control.Remote.Monad.Types as T

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

instance Show (Arduino a) where
  show = showArduino

data ShowState = ShowState {ix :: Int  
                          , ib :: Int
                          , block :: String    
                          , blocks :: [String]
                          , indent :: Int}    

showArduino :: Arduino a -> String
showArduino a = as    
  where
    (as, _) = runState (showCodeBlock a) (ShowState 0 0 "" [] 0)

showCommand :: ArduinoCommand -> State ShowState String
showCommand SystemReset = showCommand0 "SystemReset"
showCommand (SetPinModeE p m) = showCommand2 "SetPinModeE" p m
showCommand (DigitalWriteE p b) = showCommand2 "DigitalWriteE" p b
showCommand (DigitalPortWriteE p b m) = showCommand3 "DigitalPortWriteE" p b m
showCommand (AnalogWriteE p w) = showCommand2 "AnalogWriteE" p w
showCommand (ToneE p f (Just d)) = showCommand3 "ToneE" p f d
showCommand (ToneE p f Nothing) = showCommand (ToneE p f (Just 0))
showCommand (NoToneE p) = showCommand1 "NoToneE" p
showCommand (I2CWrite sa w8s) = showCommand2 "I2CWrite" sa w8s
showCommand I2CConfig = showCommand0 "I2CConfig"
showCommand (StepperSetSpeedE st sp) = showCommand2 "StepperSetSpeedE" st sp
showCommand (ServoDetachE sv) = showCommand1 "ServoDetachE" sv
showCommand (ServoWriteE sv w) = showCommand2 "ServoWriteE " sv w
showCommand (ServoWriteMicrosE sv w) = showCommand2 "ServoWriteMicrosE" sv w
showCommand (DeleteTaskE tid) = showCommand1 "DeleteTaskE" tid
showCommand (ScheduleTaskE tid tt) = showCommand2 "ScheduleTaskE" tid tt
showCommand ScheduleReset = showCommand0 "ScheduleReset"
showCommand (AttachIntE p t m) = showCommand3 "AttachIntE" p t m
showCommand (DetachIntE p) = showCommand1 "DetachIntE " p
showCommand (Interrupts) = showCommand0 "Interrupts"
showCommand (NoInterrupts) = showCommand0 "NoInterrupts"
showCommand (GiveSemE id) = showCommand1 "GiveSemE"  id
showCommand (TakeSemE id) = showCommand1 "TakeSemE" id
showCommand (CreateTaskE tid m) = do
    ts <- showCodeBlock m
    return $ "CreateTaskE " ++ show tid ++ "\n" ++ ts
showCommand (WriteRemoteRefB (RemoteRefB i) e) =
    showCommand2 "WriteRemoteRefB" i e
showCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) =
    showCommand2 "WriteRemoteRefW8" i e
showCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) =
    showCommand2 "WriteRemoteRefW16" i e
showCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) =
    showCommand2 "WriteRemoteRefW32" i e
showCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) =
    showCommand2 "WriteRemoteRefI8" i e
showCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) =
    showCommand2 "WriteRemoteRefI16" i e
showCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) =
    showCommand2 "WriteRemoteRefI32" i e
showCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) =
    showCommand2 "WriteRemoteRefL8" i e
showCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) =
    showCommand2 "WriteRemoteRefFloat" i e
showCommand (ModifyRemoteRefB (RemoteRefB i) f) =
    return $ "ModifyRemoteRefB " ++ show i ++ " " ++ show (f (RefB i))
showCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) =
    return $ "ModifyRemoteRefW8 " ++ show i ++ " " ++ show (f (RefW8 i))
showCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) =
    return $ "ModifyRemoteRefW16 " ++ show i ++ " " ++ show (f (RefW16 i))
showCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) =
    return $ "ModifyRemoteRefW32 " ++ show i ++ " " ++ show (f (RefW32 i))
showCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) =
    return $ "ModifyRemoteRefI8 " ++ show i ++ " " ++ show (f (RefI8 i))
showCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) =
    return $ "ModifyRemoteRefI16 " ++ show i ++ " " ++ show (f (RefI16 i))
showCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) =
    return $ "ModifyRemoteRefI32 " ++ show i ++ " " ++ show (f (RefI32 i))
showCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) =
    return $ "ModifyRemoteRefL8 " ++ show i ++ " " ++ show (f (RefList8 i))
showCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) =
    return $ "ModifyRemoteRefFloat " ++ show i ++ " " ++ show (f (RefFloat i))
showCommand (WhileRemoteRefB (RemoteRefB i) bf uf cb) =
    showWhileCommand (RefB i) i bf uf cb
showCommand (WhileRemoteRefW8 (RemoteRefW8 i) bf uf cb) =
    showWhileCommand (RefW8 i) i bf uf cb
showCommand (WhileRemoteRefW16 (RemoteRefW16 i) bf uf cb) =
    showWhileCommand (RefW16 i) i bf uf cb
showCommand (WhileRemoteRefW32 (RemoteRefW32 i) bf uf cb) =
    showWhileCommand (RefW32 i) i bf uf cb
showCommand (WhileRemoteRefI8 (RemoteRefI8 i) bf uf cb) =
    showWhileCommand (RefI8 i) i bf uf cb
showCommand (WhileRemoteRefI16 (RemoteRefI16 i) bf uf cb) =
    showWhileCommand (RefI16 i) i bf uf cb
showCommand (WhileRemoteRefI32 (RemoteRefI32 i) bf uf cb) =
    showWhileCommand (RefI32 i) i bf uf cb
showCommand (WhileRemoteRefFloat (RemoteRefFloat i) bf uf cb) =
    showWhileCommand (RefFloat i) i bf uf cb
showCommand (WhileRemoteRefL8 (RemoteRefL8 i) bf uf cb) = 
    showWhileCommand (RefList8 i) i bf uf cb
showCommand (Loop cb) = do
    c <- showCodeBlock cb
    return $ "Loop\n" ++ c
showCommand (LoopE cb) = do
    c <- showCodeBlock cb
    return $ "LoopE\n" ++ c
showCommand (ForInE ws f) = do
    s <- get
    p <- showCodeBlock $ f $ RemBindW8 $ ib s
    put s {ib = (ib s) + 1}
    return $ "ForIn " ++ show ws ++ " Bind" ++ show (ib s) ++ "\n" ++ p
showCommand (IfThenElseE e cb1 cb2) = do
    cs1 <- showCodeBlock cb1
    cs2 <- showCodeBlock cb2
    s <- get
    return $ "If " ++ show e ++ " Then\n" ++ cs1 ++ replicate (indent s) ' ' ++ "Else\n" ++ cs2

showCommandAndArgs :: [String] -> State ShowState String
showCommandAndArgs ss = do
    let c = head ss
    let as = tail ss
    let cmdAndArgs = c ++ " (" ++ intercalate ") (" as ++ ")"
    addToBlock cmdAndArgs 
    return cmdAndArgs

showCommand0 :: String -> State ShowState String
showCommand0 p = showCommandAndArgs [p]

showCommand1 :: (Show a) => String -> a -> State ShowState String
showCommand1 p e1 = showCommandAndArgs [p, show e1]

showCommand2 :: (Show a, Show b) => String -> a -> b -> State ShowState String
showCommand2 p e1 e2 = showCommandAndArgs [p, show e1, show e2]

showCommand3 :: (Show a, Show b, Show c) => String -> a -> b -> c -> State ShowState String
showCommand3 p e1 e2 e3 = showCommandAndArgs [p, show e1, show e2, show e3]

showWhileCommand :: Show a => Expr a -> Int -> (Expr a -> Expr Bool) -> (Expr a -> Expr a) -> Arduino () -> State ShowState String
showWhileCommand rr i bf uf cb = do
    sc <- showCodeBlock cb
    return $ "While " ++ show rr ++ " " ++ show i ++ " (" ++ show (bf rr) ++ ") (" ++ show (uf rr) ++ ")\n" ++ sc

addToBlock :: String -> State ShowState ()
addToBlock bs = do
    s <- get
    put s {block = (block s) ++ replicate (indent s) ' ' ++ bs ++ "\n"}

showCodeBlock :: Arduino a -> State ShowState String
showCodeBlock (Arduino commands) = do
    startNewBlock 
    showMonad commands
    endCurrentBlock
  where
      startNewBlock :: State ShowState ()
      startNewBlock = do
          s <- get
          put s {block = "", blocks = (block s) : (blocks s),
                 indent = indent s + 2}

      endCurrentBlock :: State ShowState String
      endCurrentBlock = do
          s <- get
          put s {block = head $ blocks s, blocks = tail $ blocks s,
                 indent = indent s - 2}
          return $ block s

      showShallowProcedure :: [String] -> b -> State ShowState b
      showShallowProcedure ss r = do
          let p = head ss
          let as = tail ss
          addToBlock $ p ++ " (" ++ intercalate ") (" as ++ ")"
          return r

      showDeepProcedure :: [String] -> State ShowState Int
      showDeepProcedure ss = do
          let p = head ss
          let as = tail ss
          s <- get
          addToBlock $ "RemBind " ++ show (ib s) ++ " <- " ++ p ++ " (" ++ intercalate "} (" as ++ ")"
          s <- get
          put s {ib = (ib s) + 1}
          return $ ib s

      showShallow0Procedure :: String -> b -> State ShowState b
      showShallow0Procedure p r = showShallowProcedure [p] r

      showShallow1Procedure :: Show a => String -> a -> b -> State ShowState b
      showShallow1Procedure p e1 r = 
        showShallowProcedure [p, show e1] r

      showShallow2Procedure :: (Show a, Show b) => String -> a -> b -> c -> State ShowState c
      showShallow2Procedure p e1 e2 r = 
        showShallowProcedure [p, show e1, show e2] r

      showShallow3Procedure :: (Show a, Show b, Show c) => String -> a -> b -> c -> d -> State ShowState d
      showShallow3Procedure p e1 e2 e3 r = 
        showShallowProcedure [p, show e1, show e2, show e3] r

      showShallow5Procedure :: (Show a, Show b, Show c, Show d, Show e) => String -> a -> b -> c -> d -> e -> f -> State ShowState f
      showShallow5Procedure p e1 e2 e3 e4 e5 r = 
        showShallowProcedure [p, show e1, show e2, show e3, show e4, show e5] r

      showDeep0Procedure :: String -> State ShowState Int
      showDeep0Procedure p = showDeepProcedure [p]

      showDeep1Procedure :: Show a => String -> Expr a -> State ShowState Int
      showDeep1Procedure p e1 = 
        showDeepProcedure [p, show e1]

      showDeep2Procedure :: (Show a, Show b) => String -> Expr a -> Expr b -> State ShowState Int
      showDeep2Procedure p e1 e2 = 
        showDeepProcedure [p, show e1, show e2]

      showDeep3Procedure :: (Show a, Show b, Show c) => String -> Expr a -> Expr b -> Expr c -> State ShowState Int
      showDeep3Procedure p e1 e2 e3 = 
        showDeepProcedure [p, show e1, show e2, show e3]

      showDeep5Procedure :: (Show a, Show b, Show c, Show d, Show e) => String -> Expr a -> Expr b -> Expr c -> Expr d -> Expr e -> State ShowState Int
      showDeep5Procedure p e1 e2 e3 e4 e5 = 
        showDeepProcedure [p, show e1, show e2, show e3, show e4, show e5]

      showNewRef :: String -> Expr a -> b -> State ShowState b
      showNewRef p e r = do
          s <- get
          addToBlock $ "bind" ++ show (ib s) ++ " <- " ++ p ++ show (ix s)
          put s {ib = (ib s) + 1, ix = (ix s) + 1}
          return r

      showProcedure :: ArduinoProcedure a -> State ShowState a
      showProcedure QueryFirmware = showShallow0Procedure "QueryFirmware" 0
      showProcedure QueryFirmwareE = do
          i <- showDeep0Procedure "QueryFirmwareE"
          return $ RemBindW16 i
      showProcedure QueryProcessor = showShallow0Procedure "QueryProcessor" UNKNOWN_PROCESSOR
      showProcedure QueryProcessorE = do
          i <- showDeep0Procedure "QueryProcessorE"
          return $ RemBindW8 i
      showProcedure Micros = showShallow0Procedure "Micros" 0
      showProcedure MicrosE = do
          i <- showDeep0Procedure "MicrosE"
          return $ RemBindW32 i
      showProcedure Millis = showShallow0Procedure "Millis" 0
      showProcedure MillisE = do
          i <- showDeep0Procedure "MillisE"
          return $ RemBindW32 i
      showProcedure (DelayMillis ms) = showShallow1Procedure "DelayMillis" ms ()
      showProcedure (DelayMillisE ms) = showShallow1Procedure "DelayMillisE" ms ()
      showProcedure (DelayMicros ms) = showShallow1Procedure "DelayMicros" ms ()
      showProcedure (DelayMicrosE ms) = showShallow1Procedure "DelayMicrosE" ms ()
      showProcedure (DigitalRead p) = showShallow1Procedure "DigitalRead" p False
      showProcedure (DigitalReadE p) = do
          i <- showDeep1Procedure "DigitalReadE" p
          return $ RemBindB i
      showProcedure (DigitalPortRead p m) = showShallow2Procedure "DigitalPortRead" p m 0
      showProcedure (DigitalPortReadE p m) = do
          i <- showDeep2Procedure "DigitalPortReadE" p m
          return $ RemBindW8 i
      showProcedure (AnalogRead p) = showShallow1Procedure "AnalogRead" p 0
      showProcedure (AnalogReadE p) = do
          i <- showDeep1Procedure "AnalogReadE" p
          return $ RemBindW16 i
      showProcedure (I2CRead p n) = showShallow2Procedure "I2CRead" p n []
      showProcedure (I2CReadE p n) = do
          i <- showDeep2Procedure "I2CReadE" p n
          return $ RemBindList8 i
      showProcedure (Stepper2Pin s p1 p2) = showShallow3Procedure "Stepper2Pin" s p1 p2 0
      showProcedure (Stepper2PinE s p1 p2) = do
          i <- showDeep3Procedure "Stepper2PinE" s p1 p2 
          return $ RemBindW8 i
      showProcedure (Stepper4Pin s p1 p2 p3 p4) = showShallow5Procedure "Stepper4Pin" s p1 p2 p3 p4 0
      showProcedure (Stepper4PinE s p1 p2 p3 p4) = do
          i <- showDeep5Procedure "Stepper4PinE" s p1 p2 p3 p4
          return $ RemBindW8 i
      showProcedure (StepperStepE st s) = showShallow2Procedure "StepperStepE" st s ()
      showProcedure (ServoAttach p) = showShallow1Procedure "ServoAttach" p 0
      showProcedure (ServoAttachE p) = do
          i <- showDeep1Procedure "ServoAttachE" p
          return $ RemBindW8 i
      showProcedure (ServoAttachMinMax p min max) = showShallow3Procedure "ServoAttachMinMax" p min max 0
      showProcedure (ServoAttachMinMaxE p min max) = do
          i <- showDeep3Procedure "ServoAttachMinMaxE" p min max
          return $ RemBindW8 i
      showProcedure (ServoRead sv) = showShallow1Procedure "ServoRead" sv 0
      showProcedure (ServoReadE sv) = do
          i <- showDeep1Procedure "ServoReadE" sv
          return $ RemBindI16 i
      showProcedure (ServoReadMicros sv) = showShallow1Procedure "ServoReadMicros" sv 0
      showProcedure (ServoReadMicrosE sv) = do
          i <- showDeep1Procedure "ServoReadMicrosE" sv
          return $ RemBindI16 i
      showProcedure QueryAllTasks = showShallow0Procedure "QueryAllTasks" []
      showProcedure QueryAllTasksE = do
          i <- showDeep0Procedure "QueryAllTasksE"
          return $ RemBindList8 i
      showProcedure (QueryTask t) = showShallow0Procedure "QueryTask" Nothing
      showProcedure (QueryTaskE t) = showShallow0Procedure "QueryTaskE" Nothing
      showProcedure (BootTaskE tids) = do
          i <- showDeep1Procedure "BootTaskE" tids
          return $ RemBindB i
      showProcedure (ReadRemoteRefB (RemoteRefB i)) = do
          i <- showDeep1Procedure "ReadRemoteRefB" (RefB i)
          return $ RemBindB i
      showProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefW8" (RefW8 i)
          return $ RemBindW8 i
      showProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefW16" (RefW16 i)
          return $ RemBindW16 i
      showProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefW32" (RefW32 i)
          return $ RemBindW32 i
      showProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefI8" (RefI8 i)
          return $ RemBindI8 i
      showProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefI16" (RefI16 i)
          return $ RemBindI16 i
      showProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefI32" (RefI32 i)
          return $ RemBindI32 i
      showProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) = do
          i <- showDeep1Procedure "ReadRemoteRefL8" (RefList8 i)
          return $ RemBindList8 i
      showProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) = do
          i <- showDeep1Procedure "ReadRemoteRefFloat" (RefFloat i)
          return $ RemBindFloat i
      showProcedure (NewRemoteRefB e) = do
          s <- get
          showNewRef "NewRemoteRefB" e (RemoteRefB  (ix s))
      showProcedure (NewRemoteRefW8 e) = do
          s <- get
          showNewRef "NewRemoteRefW8" e (RemoteRefW8 (ix s))
      showProcedure (NewRemoteRefW16 e) = do
          s <- get
          showNewRef "NewRemoteRefW16" e (RemoteRefW16 (ix s))
      showProcedure (NewRemoteRefW32 e) = do
          s <- get
          showNewRef "NewRemoteRefW32" e (RemoteRefW32 (ix s))
      showProcedure (NewRemoteRefI8 e) = do
          s <- get
          showNewRef "NewRemoteRefI8" e (RemoteRefI8 (ix s))
      showProcedure (NewRemoteRefI16 e) = do
          s <- get
          showNewRef "NewRemoteRefI16" e (RemoteRefI16 (ix s))
      showProcedure (NewRemoteRefI32 e) = do
          s <- get
          showNewRef "NewRemoteRefI32" e (RemoteRefI32 (ix s))
      showProcedure (NewRemoteRefL8 e) = do
          s <- get
          showNewRef "NewRemoteRefL8" e (RemoteRefL8 (ix s))
      showProcedure (NewRemoteRefFloat e) = do
          s <- get
          showNewRef "NewRemoteRefFloat" e (RemoteRefFloat (ix s))
      showProcedure (DebugE ws) = showShallow1Procedure "DebugE" ws ()
      showProcedure (Debug s) = showShallow1Procedure "Debug" s ()
      showProcedure DebugListen = showShallow0Procedure "DebugListen" ()
      showProcedure (Die msg msgs) = showShallow2Procedure "Die" msg msgs ()
      -- showProcedure (LiftIO _) = showShallow0Procedure "LiftIO m" ()

      showAppl :: RemoteApplicative ArduinoCommand ArduinoProcedure a -> State ShowState a
      showAppl (T.Command cmd) = do
          sc <- showCommand cmd
          addToBlock sc
          return ()
      showAppl (T.Procedure p) = showProcedure p
      showAppl (T.Ap a1 a2) = do
          f <- showAppl a1
          g <- showAppl a2
          return $ f g
      showAppl (T.Pure a) = do
          return a

      showMonad :: RemoteMonad ArduinoCommand ArduinoProcedure a -> State ShowState a
      showMonad (T.Appl app) = showAppl app
      showMonad (T.Bind m k) = do
          r <- showMonad m
          showMonad (k r)
      showMonad (T.Ap' m1 m2) = do
          f <- showMonad m1
          g <- showMonad m2
          return $ f g
