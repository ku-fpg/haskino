-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Protocol
--                Based on System.Hardware.Arduino.Protocol
-- Copyright   :  (c) University of Kansas
-- License     :  BSD3
-- Stability   :  experimental
--
-- String representation of the Haskino monad.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Show where

import           Data.Bits                        (shiftR, xor)
import           Data.Int                         (Int8, Int16, Int32)
import           Data.List
import           Data.Word                        (Word8, Word16, Word32)
import           Control.Remote.Applicative.Types as T
import           Control.Monad.State
import           Control.Remote.Monad
import           Control.Remote.Monad.Types       as T

import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Expr
import           System.Hardware.Haskino.Utils

instance Show (Arduino a) where
  show = showArduino

data ShowState = ShowState {ix        :: Int
                          , ib        :: Int
                          , block     :: String
                          , blocks    :: [String]
                          , indent    :: Int
                          , iterBinds :: [(Int, Int)]}

showArduino :: Arduino a -> String
showArduino a = as
  where
    ((_, as), _) = runState (showCodeBlock a) (ShowState 0 0 "" [] 0 [])

nextBind :: State ShowState Int
nextBind = do
    s <- get
    put s {ib = (ib s) + 1}
    return (ib s)

showCommand :: ArduinoPrimitive a -> State ShowState String
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
    (_, ts) <- showCodeBlock m
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
    showCommand2 "ModifyRemoteRefB" i f
showCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) =
    showCommand2 "ModifyRemoteRefW8" i f
showCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) =
    showCommand2 "ModifyRemoteRefW16" i f
showCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) =
    showCommand2 "ModifyRemoteRefW32" i f
showCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) =
    showCommand2 "ModifyRemoteRefI8" i f
showCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) =
    showCommand2 "ModifyRemoteRefI16" i f
showCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) =
    showCommand2 "ModifyRemoteRefI32" i f
showCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) =
    showCommand2 "ModifyRemoteRefL8" i f
showCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) =
    showCommand2 "ModifyRemoteRefFloat" i f
showCommand (Loop cb) = do
    (_, c) <- showCodeBlock cb
    return $ "Loop\n" ++ c
showCommand (LoopE cb) = do
    (_, c) <- showCodeBlock cb
    return $ "LoopE\n" ++ c
showCommand (ForInE ws f) = do
    s <- get
    (_, p) <- showCodeBlock $ f $ RemBindW8 $ ib s
    put s {ib = (ib s) + 1}
    return $ "ForIn " ++ show ws ++ " Bind" ++ show (ib s) ++ "\n" ++ p
showCommand (IfThenElseUnitE e cb1 cb2) = do
    (_, cs1) <- showCodeBlock cb1
    (_, cs2) <- showCodeBlock cb2
    s <- get
    return $ "If " ++ show e ++ " Then\n" ++ cs1 ++ replicate (indent s) ' ' ++ "Else\n" ++ cs2

showCommandAndArgs :: [String] -> State ShowState String
showCommandAndArgs ss = do
    let c = head ss
    let as = tail ss
    let cmdAndArgs = c ++ " (" ++ intercalate ") (" as ++ ")"
    return cmdAndArgs

showCommand0 :: String -> State ShowState String
showCommand0 p = showCommandAndArgs [p]

showCommand1 :: (Show a) => String -> a -> State ShowState String
showCommand1 p e1 = showCommandAndArgs [p, show e1]

showCommand2 :: (Show a, Show b) => String -> a -> b -> State ShowState String
showCommand2 p e1 e2 = showCommandAndArgs [p, show e1, show e2]

showCommand3 :: (Show a, Show b, Show c) => String -> a -> b -> c -> State ShowState String
showCommand3 p e1 e2 e3 = showCommandAndArgs [p, show e1, show e2, show e3]

addToBlock :: String -> State ShowState ()
addToBlock bs = do
    s <- get
    put s {block = (block s) ++ replicate (indent s) ' ' ++ bs ++ "\n"}

showCodeBlock :: Arduino a -> State ShowState (a, String)
showCodeBlock (Arduino commands) = do
    startNewBlock
    ret <- showMonad commands
    str <- endCurrentBlock
    return (ret, str)
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

      showIfThenElseProcedure :: Show a => Expr Bool -> Arduino a -> Arduino a -> State ShowState Int
      showIfThenElseProcedure b cb1 cb2 = do
          s <- get
          put s {ib = (ib s) + 1}
          (r1, cs1) <- showCodeBlock cb1
          let cs1' = cs1 ++ replicate (indent s + 2) ' ' ++ "return " ++ show r1 ++ "\n"
          (r2, cs2) <- showCodeBlock cb2
          let cs2' = cs2 ++ replicate (indent s + 2) ' ' ++ "return " ++ show r2 ++ "\n"
          s <- get
          addToBlock $ "RemBind " ++ show (ib s) ++ " <- " ++ "If " ++ show b ++ " Then\n" ++ cs1' ++ replicate (indent s) ' ' ++ "Else\n" ++ cs2'
          return $ ib s

      showIfThenElseEitherProcedure :: (Show a, Show b, ExprB a, ExprB b) => Expr Bool -> Arduino (ExprEither a b) -> Arduino (ExprEither a b) -> State ShowState (ExprEither a b)
      showIfThenElseEitherProcedure b cb1 cb2 = do
          s <- get
          let ibs = head $ iterBinds s
          (r1, cs1) <- showCodeBlock cb1
          let cs1' = case r1 of
                        ExprLeft a ->
                            cs1 ++ replicate (indent s + 2) ' ' ++ "return ExprLeft (" ++ show a ++ ")\n"
                        ExprRight b ->
                            cs1 ++ replicate (indent s + 2) ' ' ++ "return ExprRight (" ++ show b ++ ")\n"
          (r2, cs2) <- showCodeBlock cb2
          let cs2' = case r2 of
                        ExprLeft a ->
                            cs2 ++ replicate (indent s + 2) ' ' ++ "return ExprLeft (" ++ show a ++ ")\n"
                        ExprRight b ->
                            cs2 ++ replicate (indent s + 2) ' ' ++ "return ExprRight (" ++ show b ++ ")\n"
          addToBlock $ "If " ++ show b ++ " Then\n" ++ cs1' ++ replicate (indent s) ' ' ++ "Else\n" ++ cs2'
          (r2, cs2) <- showCodeBlock cb2
          return r2

      showWhileProcedure :: Show a => Int -> Expr a -> Expr a -> (Expr a -> Expr Bool) -> (Expr a -> Arduino (Expr a)) -> State ShowState ()
      showWhileProcedure b be iv bf bdf = do
          s <- get
          (r, cs) <- showCodeBlock (bdf be)
          let cs' = cs ++ replicate (indent s + 2) ' ' ++ "return " ++ show r ++ "\n"
          addToBlock $ "RemBind " ++ show b ++ " <- " ++ "While (" ++ show iv ++ ") (" ++ show (bf be) ++ ")\n" ++ cs'

      showIterateProcedure :: (Show a, Show b, ExprB a, ExprB b) => Int -> Expr a -> Int -> Expr b -> Expr a ->
                              (Expr a -> Arduino(ExprEither a b)) -> State ShowState Int
      showIterateProcedure b1 b1e b2 b2e iv bf = do
          s <- get
          put s {iterBinds = (b1, b2):iterBinds s}
          (r, cs) <- showCodeBlock (bf b1e)
          let cs' = cs ++ replicate (indent s + 2) ' ' ++ "return " ++ show r ++ "\n"
          addToBlock $ "RemBind " ++ show b2 ++ " <- " ++ "Iterate (" ++ show iv ++ ")\n" ++ cs
          s <- get
          put s {iterBinds = tail $ iterBinds s}
          return b2

      showNewRef :: String -> Expr a -> b -> State ShowState b
      showNewRef p e r = do
          s <- get
          addToBlock $ "bind" ++ show (ib s) ++ " <- " ++ p ++ show (ix s)
          put s {ib = (ib s) + 1, ix = (ix s) + 1}
          return r

      showProcedure :: ArduinoPrimitive a -> State ShowState a
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
      showProcedure (IfThenElseBoolE e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindB i
      showProcedure (IfThenElseWord8E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindW8 i
      showProcedure (IfThenElseWord16E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindW16 i
      showProcedure (IfThenElseWord32E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindW32 i
      showProcedure (IfThenElseInt8E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindI8 i
      showProcedure (IfThenElseInt16E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindI16 i
      showProcedure (IfThenElseInt32E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindI32 i
      showProcedure (IfThenElseL8E e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindList8 i
      showProcedure (IfThenElseFloatE e cb1 cb2) = do
          i <- showIfThenElseProcedure e cb1 cb2
          return $ RemBindFloat i
      -- The following IfThenElse* functions generated by toold/GenEitherTypes.hs
      showProcedure (IfThenElseUnitUnit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitBool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitW8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitW16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitW32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitI8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitI16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitI32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitL8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseUnitFloat e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolUnit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolBool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolW8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolW16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolW32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolI8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolI16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolI32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolL8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseBoolFloat e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW8Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW16Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseW32Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI8Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI16Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseI32Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8Unit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8Bool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8W8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8W16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8W32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8I8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8I16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8I32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8L8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseL8Float e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatUnit e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatBool e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatW8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatW16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatW32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatI8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatI16 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatI32 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatL8 e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (IfThenElseFloatFloat e cb1 cb2) =
          showIfThenElseEitherProcedure e cb1 cb2
      showProcedure (WhileBoolE iv bf bdf) = do
          i <- nextBind
          let bi = RemBindB i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileWord8E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindW8 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileWord16E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindW16 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileWord32E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindW32 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileInt8E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindI8 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileInt16E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindI16 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileInt32E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindI32 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileL8E iv bf bdf) = do
          i <- nextBind
          let bi = RemBindList8 i
          showWhileProcedure i bi iv bf bdf
          return bi
      showProcedure (WhileFloatE iv bf bdf) = do
          i <- nextBind
          let bi = RemBindFloat i
          showWhileProcedure i bi iv bf bdf
          return bi
      -- The following Iterate*E functions generated by toold/GenEitherTypes.hs
      showProcedure (IterateUnitUnitE iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitBoolE iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitW8E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitW16E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitW32E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitI8E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitI16E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitI32E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitL8E iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateUnitFloatE iv bf) = do
          i <- nextBind
          let bi = RemBindUnit i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolUnitE iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolBoolE iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolW8E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolW16E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolW32E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolI8E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolI16E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolI32E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolL8E iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateBoolFloatE iv bf) = do
          i <- nextBind
          let bi = RemBindB i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8W8E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8W16E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8W32E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8I8E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8I16E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8I32E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8L8E iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW8FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindW8 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16W8E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16W16E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16W32E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16I8E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16I16E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16I32E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16L8E iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW16FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindW16 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32W8E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32W16E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32W32E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32I8E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32I16E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32I32E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32L8E iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateW32FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindW32 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8W8E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8W16E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8W32E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8I8E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8I16E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8I32E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8L8E iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI8FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindI8 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16W8E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16W16E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16W32E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16I8E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16I16E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16I32E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16L8E iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI16FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindI16 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32W8E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32W16E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32W32E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32I8E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32I16E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32I32E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32L8E iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateI32FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindI32 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8UnitE iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8BoolE iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8W8E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8W16E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8W32E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8I8E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8I16E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8I32E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8L8E iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateL8FloatE iv bf) = do
          i <- nextBind
          let bi = RemBindList8 i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatUnitE iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindUnit j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatBoolE iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindB j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatW8E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindW8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatW16E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindW16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatW32E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindW32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatI8E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindI8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatI16E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindI16 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatI32E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindI32 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatL8E iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindList8 j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (IterateFloatFloatE iv bf) = do
          i <- nextBind
          let bi = RemBindFloat i
          j <- nextBind
          let bj = RemBindFloat j
          showIterateProcedure i bi j bj iv bf
          return bj
      showProcedure (DebugE ws) = showShallow1Procedure "DebugE" ws ()
      showProcedure (Debug s) = showShallow1Procedure "Debug" s ()
      showProcedure DebugListen = showShallow0Procedure "DebugListen" ()
      showProcedure (Die msg msgs) = showShallow2Procedure "Die" msg msgs ()
      -- showProcedure (LiftIO _) = showShallow0Procedure "LiftIO m" ()

      showAppl :: RemoteApplicative ArduinoPrimitive a -> State ShowState a
      showAppl (T.Primitive p) = case knownResult p of
                                   Just a -> do
                                     sc <- showCommand p
                                     addToBlock sc
                                     return a
                                   Nothing -> showProcedure p
      showAppl (T.Ap a1 a2) = do
          f <- showAppl a1
          g <- showAppl a2
          return $ f g
      showAppl (T.Pure a) = do
          return a

      showMonad :: RemoteMonad ArduinoPrimitive a -> State ShowState a
      showMonad (T.Appl app) = showAppl app
      showMonad (T.Bind m k) = do
          r <- showMonad m
          showMonad (k r)
      showMonad (T.Ap' m1 m2) = do
          f <- showMonad m1
          g <- showMonad m2
          return $ f g
