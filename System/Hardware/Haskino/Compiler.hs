-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Compiler
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- 'C' code generator for the Haskino Library.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Compiler(compileExpr, runTest) where 

import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)

import Control.Monad.State

import Control.Remote.Monad
import Control.Remote.Monad.Types as T

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

-- type CompileState = (Int, Int, String)
data CompileState = CompileState { ix :: Int  
                     , ib :: Int  
                     , cmds :: String
                     , binds :: String
                     , blocks :: [String]
                     , tasks :: [String] 
                     } deriving (Show)   

{-
packageCommand (ServoDetachE sv) ix _ = 
    (buildCommand SRVO_CMD_DETACH (packageExpr sv), ix)
packageCommand (ServoWriteE sv w) ix _ = 
    (buildCommand SRVO_CMD_WRITE (packageExpr sv ++ packageExpr w), ix)
packageCommand (ServoWriteMicrosE sv w) ix _ = 
    (buildCommand SRVO_CMD_WRITE_MICROS (packageExpr sv ++ packageExpr w), ix)
packageCommand (DeleteTaskE tid) ix _ =
    (buildCommand SCHED_CMD_DELETE_TASK (packageExpr tid), ix)
packageCommand (ScheduleTaskE tid tt) ix _ =
    (buildCommand SCHED_CMD_SCHED_TASK (packageExpr tid ++ packageExpr tt), ix)
packageCommand ScheduleReset ix _ =
    (buildCommand SCHED_CMD_RESET [], ix)
packageCommand (AttachIntE p t m) ix _ =
    (buildCommand SCHED_CMD_ATTACH_INT (packageExpr p ++ packageExpr t ++ packageExpr m), ix)
packageCommand (DetachIntE p) ix _ =
    (buildCommand SCHED_CMD_DETACH_INT (packageExpr p), ix)
packageCommand (Interrupts) ix _ =
    (buildCommand SCHED_CMD_INTERRUPTS [], ix)
packageCommand (NoInterrupts) ix _ =
    (buildCommand SCHED_CMD_NOINTERRUPTS [], ix)
packageCommand (GiveSemE id) ix _ =
    (buildCommand SCHED_CMD_GIVE_SEM (packageExpr id), ix)
packageCommand (TakeSemE id) ix _ =
    (buildCommand SCHED_CMD_TAKE_SEM (packageExpr id), ix)
packageCommand (CreateTaskE tid m) ix _ =
    ((framePackage cmd) `B.append` (genAddToTaskCmds td), ix')
  where
    (td, ix', ib') = packageCodeBlock m ix 0
    taskSize = fromIntegral (B.length td)
    cmd = buildCommand SCHED_CMD_CREATE_TASK ((packageExpr tid) ++ (packageExpr (LitW16 taskSize)) ++ (packageExpr (LitW16 (fromIntegral (ib' + 1)))))                                   
    -- Max command data size is max frame size - 7 
    -- command - 1 byte,checksum - 1 byte,frame flag - 1 byte
    -- task ID - 2 bytes (lit + constant), size - 2 bytes (lit + constant)
    maxCmdSize = maxFirmwareSize - 7
    genAddToTaskCmds tds | fromIntegral (B.length tds) > maxCmdSize = 
        addToTask (B.take maxCmdSize tds) 
            `B.append` (genAddToTaskCmds (B.drop maxCmdSize tds))
    genAddToTaskCmds tds = addToTask tds
    addToTask tds' = framePackage $ buildCommand SCHED_CMD_ADD_TO_TASK ((packageExpr tid) ++ 
                                                                          (packageExpr (LitW8 (fromIntegral (B.length tds')))) ++ 
                                                                          (B.unpack tds'))
packageCommand (WriteRemoteRefB (RemoteRefB i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (ModifyRemoteRefB (RemoteRefB i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefB i))), ix)
packageCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefW8 i))), ix)
packageCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW16 i))), ix)
packageCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW32 i))), ix)
packageCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefI8 i))), ix)
packageCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI16 i))), ix)
packageCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI32 i))), ix)
packageCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefList8 i))), ix)
packageCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefFloat i))), ix)
packageCommand (WhileRemoteRefB (RemoteRefB i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefB i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefB i))
packageCommand (WhileRemoteRefW8 (RemoteRefW8 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefW8 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefW8 i))
packageCommand (WhileRemoteRefW16 (RemoteRefW16 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefW16 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefW16 i))
packageCommand (WhileRemoteRefW32 (RemoteRefW32 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefW32 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefW32 i))
packageCommand (WhileRemoteRefI8 (RemoteRefI8 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefI8 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefI8 i))
packageCommand (WhileRemoteRefI16 (RemoteRefI16 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefI16 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefI16 i))
packageCommand (WhileRemoteRefI32 (RemoteRefI32 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefI32 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefI32 i))
packageCommand (WhileRemoteRefFloat (RemoteRefFloat i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefFloat i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefFloat i))
packageCommand (WhileRemoteRefL8 (RemoteRefL8 i) bf uf cb) ix ib =
    (buildCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefList8 i)) ++ [fromIntegral $ length ufe] ++ ufe ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
    ufe = packageExpr (uf (RefList8 i))
packageCommand (LoopE cb) ix ib =
    (buildCommand BC_CMD_LOOP (B.unpack pc), ix')
  where
    (pc, ix', _) = packageCodeBlock cb ix ib
packageCommand (ForInE ws f) ix ib =
    (buildCommand BC_CMD_FORIN ((packageExpr ws) ++ (packageExpr (RemBindW8 ib)) ++ (B.unpack pc)), ix')
  where
    (pc, ix', _) = packageCodeBlock (f (RemBindW8 ib)) ix (ib+1)
packageCommand (IfThenElse e cb1 cb2) ix ib =
    (buildCommand BC_CMD_IF_THEN_ELSE (thenSize ++ pe ++ (B.unpack pc1) ++ (B.unpack pc2)), ix'')
  where
    pe = packageExpr e
    (pc1, ix', _) = packageCodeBlock cb1 ix ib
    (pc2, ix'', _) = packageCodeBlock cb2 ix' ib
    thenSize = word16ToBytes $ fromIntegral (B.length pc1)
-}

myTest :: Arduino ()
myTest =  do
  loopE $ do
    setPinModeE 2 INPUT 
    setPinModeE 3 OUTPUT
    a <- millisE
    return ()

runTest :: String
runTest = "{\n" ++ binds s ++ cmds s ++ "}\n"
  where
    (_, s) = runState (compileCodeBlock myTest) (CompileState 0 0 "" "" [] [])

compileLine :: String -> State CompileState ()
compileLine s = do
    st <- get 
    put st { cmds = cmds st ++ s ++ "\n"}
    return ()

compileNoExprCommand :: String -> State CompileState ()
compileNoExprCommand s =
    compileLine (s ++ "()")

compile1ExprCommand :: String -> Expr a -> State CompileState ()
compile1ExprCommand s e =
    compileLine (s ++ "(" ++ compileExpr e ++ ")")

compile2ExprCommand :: String -> Expr a -> Expr b -> State CompileState ()
compile2ExprCommand s e1 e2 =
    compileLine (s ++ "(" ++ compileExpr e1 ++ "," ++ 
                                      compileExpr e2 ++ ")")

compile3ExprCommand :: String -> Expr a -> Expr b -> Expr c -> State CompileState ()
compile3ExprCommand s e1 e2 e3 =
    compileLine (s ++ "(" ++ compileExpr e1 ++ "," ++
                                      compileExpr e2 ++ "," ++ 
                                      compileExpr e3 ++ ")")

compileCommand :: ArduinoCommand -> State CompileState ()
compileCommand SystemReset = return ()
compileCommand (SetPinModeE p m) = compile2ExprCommand "pinMode" p m
compileCommand (DigitalWriteE p b) = compile2ExprCommand "digitalWrite" p b
compileCommand (DigitalPortWriteE p b m) = 
    compile3ExprCommand "digitalPortWrite" p b m -- ToDo runtime
compileCommand (AnalogWriteE p w) = compile2ExprCommand "analogWrite" p w
compileCommand (ToneE p f (Just d)) = compile3ExprCommand "tone" p f d
compileCommand (ToneE p f Nothing) = compile3ExprCommand "tone" p f (lit (0::Word32))
compileCommand (NoToneE p) = compile1ExprCommand "noTone" p
compileCommand (I2CWrite sa w8s) = compile2ExprCommand "i2cWrite" sa w8s -- ToDo: runtime
compileCommand I2CConfig = compileNoExprCommand "i2cConfig" -- ToDo: runtime
compileCommand (StepperSetSpeedE st sp) = 
    compile2ExprCommand "stepperSetSpeed" st sp -- ToDo: runtime
compileCommand (LoopE cb) = do
    compileLine "while (1)"
    compileCodeBlock cb
    return ()
compileCommand (IfThenElse e cb1 cb2) = do
    compileLine $ "if (" ++ compileExpr e ++ ")"
    compileCodeBlock cb1
    compileLine "else"
    compileCodeBlock cb2
    return ()

compileSimpleProcedure :: String -> String -> State CompileState Int
compileSimpleProcedure t p = do
    s <- get
    let b = ib s
    put s {ib = b + 1, 
           cmds = cmds s ++ "bind" ++ show b ++ " = " ++ p ++ ";\n",
           binds = binds s ++ t ++ " bind" ++ show b ++ ";\n"}
    return b

compileNoExprProcedure :: String -> String -> State CompileState Int
compileNoExprProcedure t p = do
    b <- compileSimpleProcedure t (p ++ "()")
    return b

compile1ExprProcedure :: String -> String -> Expr a -> State CompileState Int
compile1ExprProcedure t p e = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e ++ ")")
    return b

compile2ExprProcedure :: String -> String -> 
                         Expr a -> Expr a -> State CompileState Int
compile2ExprProcedure t p e1 e2 = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e1 ++ "," ++ 
                                               compileExpr e2 ++ ")")
    return b

compile3ExprProcedure :: String -> String -> 
                         Expr a -> Expr a  -> Expr a -> State CompileState Int
compile3ExprProcedure t p e1 e2 e3 = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e1 ++ "," ++
                                               compileExpr e2 ++ "," ++ 
                                               compileExpr e3 ++ ")")
    return b

compileProcedure :: ArduinoProcedure a -> State CompileState a
compileProcedure MillisE = do
    b <- compileNoExprProcedure "uint32_t" "millis"
    return $ remBind b

compileCodeBlock :: Arduino a -> State CompileState a
compileCodeBlock (Arduino commands) = do
    compileLine "{"
    r <- compileMonad commands
    s <- get
    compileLine $ binds s
    put s {binds = ""}
    compileLine "}"
    return r
  where 
      compileMonad :: RemoteMonad ArduinoCommand ArduinoProcedure a -> 
                      State CompileState a
      compileMonad (T.Appl app) = compileAppl app 
      compileMonad (T.Bind m k) = do
        r <- compileMonad m
        compileMonad (k r)
      compileMonad (T.Ap' m1 m2) = do
        f <- compileMonad m1
        g <- compileMonad m2
        return (f g)

      compileAppl :: RemoteApplicative ArduinoCommand ArduinoProcedure a -> 
                     State CompileState a
      compileAppl (T.Command cmd) = compileCommand cmd
      compileAppl (T.Procedure p) = compileProcedure p
      compileAppl (T.Ap a1 a2) = do
        f <- compileAppl a1
        g <- compileAppl a2
        return (f g)
      compileAppl (T.Pure a) = return a

{-
compileCodeBlock :: Arduino a -> Int -> Int -> (String, Int, Int)
compileCodeBlock (Arduino commands) ix ib = (cmds', ix', ib')
  where
      (_, cmds', ix', ib') = compileMonad commands ix ib ""

      packProcedure :: ArduinoProcedure a -> Int -> Int -> B.ByteString -> (a, B.ByteString, Int, Int)
      packProcedure QueryFirmware ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure QueryFirmware ib)), ix, ib)
      packProcedure QueryFirmwareE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure QueryFirmwareE ib)), ix, ib+1)
      packProcedure QueryProcessor ix ib cmds = (UNKNOWN_PROCESSOR, B.append cmds (lenPackage (packageProcedure QueryProcessor ib)), ix, ib)
      packProcedure QueryProcessorE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure QueryProcessorE ib)), ix, ib+1)
      packProcedure Micros ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure Micros ib)), ix, ib)
      packProcedure MicrosE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure MicrosE ib)), ix, ib+1)
      packProcedure Millis ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure Millis ib)), ix, ib)
      packProcedure MillisE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure MillisE ib)), ix, ib+1)
      packProcedure (DelayMillis ms) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (DelayMillis ms) ib)), ix, ib)
      packProcedure (DelayMillisE ms) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (DelayMillisE ms) ib)), ix, ib)
      packProcedure (DelayMicros ms) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (DelayMicros ms) ib)), ix, ib)
      packProcedure (DelayMicrosE ms) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (DelayMicrosE ms) ib)), ix, ib)  
      packProcedure (DigitalRead p) ix ib cmds = (False, B.append cmds (lenPackage (packageProcedure (DigitalRead p) ib)), ix, ib)
      packProcedure (DigitalReadE p) ib ix cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (DigitalReadE p) ib)), ix, ib+1)
      packProcedure (DigitalPortRead p m) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (DigitalPortRead p m) ib)), ix, ib)
      packProcedure (DigitalPortReadE p m) ib ix cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (DigitalPortReadE p m) ib)), ix, ib+1)
      packProcedure (AnalogRead p) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (AnalogRead p) ib)), ix, ib)
      packProcedure (AnalogReadE p) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (AnalogReadE p) ib)), ix, ib+1)
      packProcedure (I2CRead p n) ix ib cmds = ([], B.append cmds (lenPackage (packageProcedure (I2CRead p n) ib)), ix, ib)
      packProcedure (I2CReadE p n) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (I2CReadE p n) ib)), ix, ib+1)
      packProcedure (Stepper2Pin s p1 p2) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (Stepper2Pin s p1 p2) ib)), ix, ib)
      packProcedure (Stepper2PinE s p1 p2) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (Stepper2PinE s p1 p2) ib)), ix, ib+1)
      packProcedure (Stepper4Pin s p1 p2 p3 p4) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (Stepper4Pin s p1 p2 p3 p4) ib)), ix, ib)
      packProcedure (Stepper4PinE s p1 p2 p3 p4) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (Stepper4PinE s p1 p2 p3 p4) ib)), ix, ib+1)
      packProcedure (StepperStepE st s) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (StepperStepE st s) ib)), ix, ib+1)
      packProcedure (ServoAttach p) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (ServoAttach p) ib)), ix, ib)
      packProcedure (ServoAttachE p) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ServoAttachE p) ib)), ix, ib+1)
      packProcedure (ServoAttachMinMax p min max) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (ServoAttachMinMax p min max) ib)), ix, ib)
      packProcedure (ServoAttachMinMaxE p min max) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ServoAttachMinMaxE p min max) ib)), ix, ib+1)
      packProcedure (ServoRead sv) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (ServoRead sv) ib)), ix, ib)
      packProcedure (ServoReadE sv) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ServoReadE sv) ib)), ix, ib+1)
      packProcedure (ServoReadMicros sv) ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure (ServoReadMicros sv) ib)), ix, ib)
      packProcedure (ServoReadMicrosE sv) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ServoReadMicrosE sv) ib)), ix, ib+1)
      packProcedure QueryAllTasks ix ib cmds = ([], B.append cmds (lenPackage (packageProcedure QueryAllTasks ib)), ix, ib)
      packProcedure QueryAllTasksE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure QueryAllTasksE ib)), ix, ib+1)
      packProcedure (QueryTask t) ix ib cmds = (Nothing, B.append cmds (lenPackage (packageProcedure (QueryTask t) ib)), ix, ib)
      packProcedure (QueryTaskE t) ix ib cmds = (Nothing, B.append cmds (lenPackage (packageProcedure (QueryTaskE t) ib)), ix, ib+1)
      packProcedure (BootTaskE tids) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (BootTaskE tids) ib)), ix, ib+1) 
      packProcedure (ReadRemoteRefB (RemoteRefB i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefB (RemoteRefB i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ib)), ix, ib+1)
      packProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ib)), ix, ib+1)
      packProcedure (NewRemoteRefB e) ix ib cmds = (RemoteRefB ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefB e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefW8 e) ix ib cmds = (RemoteRefW8 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW8 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefW16 e) ix ib cmds = (RemoteRefW16 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW16 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefW32 e) ix ib cmds = (RemoteRefW32 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW32 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefI8 e) ix ib cmds = (RemoteRefI8 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI8 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefI16 e) ix ib cmds = (RemoteRefI16 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI16 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefI32 e) ix ib cmds = (RemoteRefI32 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI32 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefL8 e) ix ib cmds = (RemoteRefL8 ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefL8 e) ix ib)), ix+1, ib+1)
      packProcedure (NewRemoteRefFloat e) ix ib cmds = (RemoteRefFloat ix, B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefFloat e) ix ib)), ix+1, ib+1)
      packProcedure (DebugE s) ix ib cmds = ((), B.append cmds (lenPackage (packageProcedure (DebugE s) ib)), ix, ib+1) 
      -- For sending as part of a Scheduler task, debug and die make no sense.  
      -- Instead of signalling an error, at this point they are just ignored.
      packProcedure (Debug _) ix ib cmds = ((), cmds, ix, ib)
      packProcedure DebugListen ix ib cmds = ((), cmds, ix, ib)
      packProcedure (Die _ _) ix ib cmds = ((), cmds, ix, ib)
-}
{-
packageProcedure :: ArduinoProcedure a -> Int -> B.ByteString
packageProcedure QueryFirmware ib    = buildCommand BS_CMD_REQUEST_VERSION [fromIntegral ib]
packageProcedure QueryFirmwareE ib   = buildCommand BS_CMD_REQUEST_VERSION [fromIntegral ib]
packageProcedure QueryProcessor ib   = buildCommand BS_CMD_REQUEST_TYPE [fromIntegral ib]
packageProcedure QueryProcessorE ib  = buildCommand BS_CMD_REQUEST_TYPE [fromIntegral ib]
packageProcedure Micros ib           = buildCommand BS_CMD_REQUEST_MICROS [fromIntegral ib]
packageProcedure MicrosE ib          = buildCommand BS_CMD_REQUEST_MICROS [fromIntegral ib]
packageProcedure Millis ib           = buildCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib]
packageProcedure MillisE ib          = buildCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib]
packageProcedure (DigitalRead p) ib  = buildCommand DIG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr $ lit p))
packageProcedure (DigitalReadE pe) ib = buildCommand DIG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr pe))
packageProcedure (DigitalPortRead p m) ib  = buildCommand DIG_CMD_READ_PORT ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit m)))
packageProcedure (DigitalPortReadE pe me) ib = buildCommand DIG_CMD_READ_PORT ((fromIntegral ib) : ((packageExpr pe) ++ (packageExpr me)))
packageProcedure (AnalogRead p) ib   = buildCommand ALG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr $ lit p))
packageProcedure (AnalogReadE pe) ib = buildCommand ALG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr pe))
packageProcedure (I2CRead sa cnt) ib = buildCommand I2C_CMD_READ ((fromIntegral ib) : ((packageExpr $ lit sa) ++ (packageExpr $ lit cnt)))
packageProcedure (I2CReadE sae cnte) ib = buildCommand I2C_CMD_READ ((fromIntegral ib) : ((packageExpr sae) ++ (packageExpr cnte)))
packageProcedure (Stepper2Pin s p1 p2) ib = buildCommand STEP_CMD_2PIN ((fromIntegral ib) : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2)))
packageProcedure (Stepper2PinE s p1 p2) ib = buildCommand STEP_CMD_2PIN ((fromIntegral ib) : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)))
packageProcedure (Stepper4Pin s p1 p2 p3 p4) ib = buildCommand STEP_CMD_4PIN ((fromIntegral ib) : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2) ++ (packageExpr $ lit p3) ++ (packageExpr $ lit p4)))
packageProcedure (Stepper4PinE s p1 p2 p3 p4) ib = buildCommand STEP_CMD_4PIN ((fromIntegral ib) : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)++ (packageExpr p3) ++ (packageExpr p4)))
packageProcedure (StepperStepE st s) ib = buildCommand STEP_CMD_STEP ((fromIntegral ib) : ((packageExpr st) ++ (packageExpr s)))
packageProcedure (ServoAttach p) ib = buildCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
packageProcedure (ServoAttachE p) ib = buildCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr p) ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
packageProcedure (ServoAttachMinMax p min max) ib = buildCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit min) ++ (packageExpr $ lit max)))
packageProcedure (ServoAttachMinMaxE p min max) ib = buildCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr p)++ (packageExpr min) ++ (packageExpr max)))
packageProcedure (ServoRead sv) ib = buildCommand SRVO_CMD_READ ((fromIntegral ib) : ((packageExpr $ lit sv)))
packageProcedure (ServoReadE sv) ib = buildCommand SRVO_CMD_READ ((fromIntegral ib) : ((packageExpr sv)))
packageProcedure (ServoReadMicros sv) ib = buildCommand SRVO_CMD_READ_MICROS ((fromIntegral ib) : ((packageExpr $ lit sv)))
packageProcedure (ServoReadMicrosE sv) ib = buildCommand SRVO_CMD_READ_MICROS ((fromIntegral ib) : ((packageExpr sv)))
packageProcedure QueryAllTasks ib    = buildCommand SCHED_CMD_QUERY_ALL [fromIntegral ib]
packageProcedure QueryAllTasksE ib   = buildCommand SCHED_CMD_QUERY_ALL [fromIntegral ib]
packageProcedure (QueryTask tid) ib  = buildCommand SCHED_CMD_QUERY ((fromIntegral ib) : (packageExpr $ lit tid))
packageProcedure (QueryTaskE tide) ib = buildCommand SCHED_CMD_QUERY ((fromIntegral ib) : (packageExpr tide))
packageProcedure (DelayMillis ms) ib  = buildCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib) : (packageExpr $ lit ms))
packageProcedure (DelayMillisE ms) ib = buildCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib) : (packageExpr ms))
packageProcedure (DelayMicros ms) ib  = buildCommand BC_CMD_DELAY_MICROS ((fromIntegral ib) : (packageExpr $ lit ms))
packageProcedure (DelayMicrosE ms) ib = buildCommand BC_CMD_DELAY_MICROS ((fromIntegral ib) : (packageExpr ms))
packageProcedure (BootTaskE tids) ib = buildCommand SCHED_CMD_BOOT_TASK ((fromIntegral ib) : (packageExpr tids))
packageProcedure (ReadRemoteRefB (RemoteRefB i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_BOOL, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_LIST8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ib = buildCommand REF_CMD_READ [fromIntegral $ fromEnum REF_FLOAT, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (DebugE s) ib = buildCommand BS_CMD_DEBUG ((fromIntegral ib) : (packageExpr s))
packageProcedure DebugListen ib = B.empty

packageRemoteBinding :: ArduinoProcedure a -> Int -> Int -> B.ByteString
packageRemoteBinding (NewRemoteRefB e)  ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_BOOL, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefW8 e)  ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_WORD8, fromIntegral ib, fromIntegral ix] ++(packageExpr e))
packageRemoteBinding (NewRemoteRefW16 e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_WORD16, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefW32 e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_WORD32, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefI8 e)  ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_INT8, fromIntegral ib, fromIntegral ix] ++(packageExpr e))
packageRemoteBinding (NewRemoteRefI16 e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_INT16, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefI32 e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_INT32, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefL8 e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_LIST8, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefFloat e) ix ib = buildCommand REF_CMD_NEW ([fromIntegral $ fromEnum REF_FLOAT, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
-}

compileSubExpr :: String -> Expr a -> String
compileSubExpr ec e = ec ++ "(" ++ compileExpr e ++ ")"

compileTwoSubExpr :: String -> Expr a -> Expr b -> String
compileTwoSubExpr ec e1 e2 = ec ++ "(" ++ compileExpr e1 ++ 
                             "," ++ compileExpr e2 ++ ")"

compileInfixSubExpr :: String -> Expr a -> Expr b -> String
compileInfixSubExpr ec e1 e2 = "(" ++ compileExpr e1 ++ " " ++ ec ++ 
                               " " ++ compileExpr e2 ++ ")"

compileSign :: Expr a -> String
compileSign e = "(" ++ compileExpr e ++ " == 0 ? 0 : 1)"

compileIfSubExpr :: Expr a -> Expr b -> Expr b -> String
compileIfSubExpr e1 e2 e3 = compileExpr e1 ++ " ? " ++ 
                            compileExpr e2 ++ " : " ++ compileExpr e3

compileNeg :: Expr a -> String
compileNeg = compileSubExpr "-"
 
compileComp :: Expr a -> String
compileComp = compileSubExpr "~"
 
compileBind :: Int -> String
compileBind b = "bind" ++ show b
 
compileBAnd :: Expr a -> Expr a -> String
compileBAnd = compileInfixSubExpr "&&"
 
compileBOr :: Expr a -> Expr a -> String
compileBOr = compileInfixSubExpr "||"
 
compileEqual :: Expr a -> Expr a -> String
compileEqual = compileInfixSubExpr "=="
 
compileLess :: Expr a -> Expr a -> String
compileLess = compileInfixSubExpr "<"
 
compileAdd :: Expr a -> Expr a -> String
compileAdd = compileInfixSubExpr "+"
 
compileSub :: Expr a -> Expr a -> String
compileSub = compileInfixSubExpr "-"
 
compileMult :: Expr a -> Expr a -> String
compileMult = compileInfixSubExpr "*"
 
compileDiv :: Expr a -> Expr a -> String
compileDiv = compileInfixSubExpr "/"
 
compileMod :: Expr a -> Expr a -> String
compileMod = compileInfixSubExpr "%"
 
compileAnd :: Expr a -> Expr a -> String
compileAnd = compileInfixSubExpr "&"
 
compileOr :: Expr a -> Expr a -> String
compileOr = compileInfixSubExpr "|"
 
compileXor :: Expr a -> Expr a -> String
compileXor = compileInfixSubExpr "^"
 
compileShiftLeft :: Expr a -> Expr b -> String
compileShiftLeft = compileInfixSubExpr "<<"
 
compileShiftRight :: Expr a -> Expr b -> String
compileShiftRight = compileInfixSubExpr ">>"

compileToInt :: Expr a -> String
compileToInt e = "((int_32) " ++ compileExpr e ++ ")"

compileFromInt :: String -> Expr a -> String
compileFromInt t e = "((" ++ t ++ ") " ++ compileExpr e ++ ")"
 
compileRef :: String -> Int -> String
compileRef t n = "ref[" ++ show n ++ "]." ++ t

compileExpr :: Expr a -> String
compileExpr (LitB b) = if b then "1" else "0"
compileExpr (ShowB e) = compileSubExpr "showBool" e
compileExpr (RefB n) = compileRef "boolVal" n
compileExpr (RemBindB b) = "bind" ++ show b
compileExpr (NotB e) = compileSubExpr "!" e 
compileExpr (AndB e1 e2) = compileBAnd e1 e2 
compileExpr (OrB e1 e2) = compileBOr e1 e2  
compileExpr (EqB e1 e2) = compileEqual e1 e2  
compileExpr (LessB e1 e2) = compileLess e1 e2  
compileExpr (IfB e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (EqW8 e1 e2) = compileEqual e1 e2 
compileExpr (LessW8 e1 e2) = compileLess e1 e2
compileExpr (EqW16 e1 e2) = compileEqual e1 e2 
compileExpr (LessW16 e1 e2) = compileLess e1 e2 
compileExpr (EqW32 e1 e2) = compileEqual e1 e2 
compileExpr (LessW32 e1 e2) = compileLess e1 e2 
compileExpr (EqI8 e1 e2) = compileEqual e1 e2 
compileExpr (LessI8 e1 e2) = compileLess e1 e2 
compileExpr (EqI16 e1 e2) = compileEqual e1 e2 
compileExpr (LessI16 e1 e2) = compileLess e1 e2 
compileExpr (EqI32 e1 e2) = compileEqual e1 e2 
compileExpr (LessI32 e1 e2) = compileLess e1 e2 
compileExpr (EqL8 e1 e2) = compileTwoSubExpr "list8Equal" e1 e2
compileExpr (LessL8 e1 e2) = compileTwoSubExpr "list8Less" e1 e2
compileExpr (EqFloat e1 e2) = compileEqual e1 e2 
compileExpr (LessFloat e1 e2) = compileLess e1 e2 
compileExpr (LitW8 w) = show w
compileExpr (ShowW8 e) = compileSubExpr "showWord8" e
compileExpr (RefW8 n) = compileRef "word8Val" n
compileExpr (RemBindW8 b) = compileBind b
compileExpr (FromIntW8 e) = compileFromInt "uint_8" e
compileExpr (ToIntW8 e) = compileToInt e
compileExpr (NegW8 e) = compileNeg e -- ToDo:  Check arduino compiler
compileExpr (SignW8 e) = compileSign e
compileExpr (AddW8 e1 e2) = compileAdd e1 e2 
compileExpr (SubW8 e1 e2) = compileSub e1 e2 
compileExpr (MultW8 e1 e2) = compileMult e1 e2 
compileExpr (DivW8 e1 e2) = compileDiv e1 e2 
compileExpr (RemW8 e1 e2) = compileMod e1 e2 
compileExpr (QuotW8 e1 e2) = compileDiv e1 e2 
compileExpr (ModW8 e1 e2) = compileMod e1 e2 
compileExpr (AndW8 e1 e2) = compileAnd e1 e2 
compileExpr (OrW8 e1 e2) = compileOr e1 e2 
compileExpr (XorW8 e1 e2) = compileXor e1 e2 
compileExpr (CompW8 e) = compileComp e 
compileExpr (ShfLW8 e1 e2) = compileShiftLeft e1 e2 
compileExpr (ShfRW8 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfW8 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBW8 e1 e2) = compileTwoSubExpr "testBW8" e1 e2 
compileExpr (SetBW8 e1 e2) = compileTwoSubExpr "setBW8" e1 e2
compileExpr (ClrBW8 e1 e2) = compileTwoSubExpr "clrBW8" e1 e2 
compileExpr (LitW16 w) = show w
compileExpr (ShowW16 e) = compileSubExpr "showWord16" e
compileExpr (RefW16 n) = compileRef "word16Val" n
compileExpr (RemBindW16 b) = compileBind b
compileExpr (FromIntW16 e) = compileFromInt "uint_16" e
compileExpr (ToIntW16 e) = compileToInt e
compileExpr (NegW16 e) = compileNeg e
compileExpr (SignW16 e) = compileSign e
compileExpr (AddW16 e1 e2) = compileAdd e1 e2 
compileExpr (SubW16 e1 e2) = compileSub e1 e2 
compileExpr (MultW16 e1 e2) = compileMult e1 e2 
compileExpr (DivW16 e1 e2) = compileDiv e1 e2 
compileExpr (RemW16 e1 e2) = compileMod e1 e2 
compileExpr (QuotW16 e1 e2) = compileDiv e1 e2 
compileExpr (ModW16 e1 e2) = compileMod e1 e2 
compileExpr (AndW16 e1 e2) = compileAnd e1 e2 
compileExpr (OrW16 e1 e2) = compileOr e1 e2 
compileExpr (XorW16 e1 e2) = compileXor e1 e2 
compileExpr (CompW16 e) = compileComp e 
compileExpr (ShfLW16 e1 e2) = compileShiftLeft e1 e2 
compileExpr (ShfRW16 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfW16 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBW16 e1 e2) = compileTwoSubExpr "testBW16" e1 e2 
compileExpr (SetBW16 e1 e2) = compileTwoSubExpr "setBW16" e1 e2 
compileExpr (ClrBW16 e1 e2) = compileTwoSubExpr "clrBW16" e1 e2 
compileExpr (LitW32 w) = show w
compileExpr (ShowW32 e) = compileSubExpr "showWord32" e
compileExpr (RefW32 n) = compileRef "word32Val" n
compileExpr (RemBindW32 b) = compileBind b
compileExpr (FromIntW32 e) = ""
compileExpr (ToIntW32 e) = ""
compileExpr (NegW32 e) = compileNeg e
compileExpr (SignW32 e) = compileSign e
compileExpr (AddW32 e1 e2) = compileAdd e1 e2 
compileExpr (SubW32 e1 e2) = compileSub e1 e2 
compileExpr (MultW32 e1 e2) = compileMult e1 e2 
compileExpr (DivW32 e1 e2) = compileDiv e1 e2 
compileExpr (RemW32 e1 e2) = compileMod e1 e2 
compileExpr (QuotW32 e1 e2) = compileDiv e1 e2 
compileExpr (ModW32 e1 e2) = compileMod e1 e2 
compileExpr (AndW32 e1 e2) = compileAnd e1 e2 
compileExpr (OrW32 e1 e2) = compileOr e1 e2 
compileExpr (XorW32 e1 e2) = compileXor e1 e2 
compileExpr (CompW32 e) = compileComp e
compileExpr (ShfLW32 e1 e2) = compileShiftLeft e1 e2 
compileExpr (ShfRW32 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfW32 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBW32 e1 e2) = compileTwoSubExpr "testBW32" e1 e2
compileExpr (SetBW32 e1 e2) = compileTwoSubExpr "setBW32" e1 e2
compileExpr (ClrBW32 e1 e2) = compileTwoSubExpr "clrBW32" e1 e2
compileExpr (LitI8 w) = show w
compileExpr (ShowI8 e) = compileSubExpr "showInt8" e
compileExpr (RefI8 n) = compileRef "int8Val" n
compileExpr (RemBindI8 b) = compileBind b
compileExpr (FromIntI8 e) = compileFromInt "int_8" e
compileExpr (ToIntI8 e) = compileToInt e
compileExpr (NegI8 e) = compileNeg e
compileExpr (SignI8 e) = compileSubExpr "sign8" e
compileExpr (AddI8 e1 e2) = compileAdd e1 e2 
compileExpr (SubI8 e1 e2) = compileSub e1 e2 
compileExpr (MultI8 e1 e2) = compileMult e1 e2 
compileExpr (DivI8 e1 e2) = compileTwoSubExpr "div8" e1 e2 
compileExpr (RemI8 e1 e2) = compileMod e1 e2 
compileExpr (QuotI8 e1 e2) = compileDiv e1 e2 
compileExpr (ModI8 e1 e2) = compileTwoSubExpr "mod8" e1 e2
compileExpr (AndI8 e1 e2) = compileAdd e1 e2 
compileExpr (OrI8 e1 e2) = compileOr e1 e2 
compileExpr (XorI8 e1 e2) = compileXor e1 e2 
compileExpr (CompI8 e) = compileComp e 
compileExpr (ShfLI8 e1 e2) = compileShiftLeft e1 e2  -- ToDo: need runtinme??
compileExpr (ShfRI8 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfI8 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBI8 e1 e2) = compileTwoSubExpr "testBI8" e1 e2 
compileExpr (SetBI8 e1 e2) = compileTwoSubExpr "setBI8" e1 e2
compileExpr (ClrBI8 e1 e2) = compileTwoSubExpr "clrBI8" e1 e2
compileExpr (LitI16 w) = show w
compileExpr (ShowI16 e) = compileSubExpr "showInt16" e
compileExpr (RefI16 n) = compileRef "int16Val" n
compileExpr (RemBindI16 b) = compileBind b
compileExpr (FromIntI16 e) = compileFromInt "int_16" e
compileExpr (ToIntI16 e) = compileToInt e
compileExpr (NegI16 e) = compileNeg e
compileExpr (SignI16 e) = compileSubExpr "sign16" e
compileExpr (AddI16 e1 e2) = compileAdd e1 e2 
compileExpr (SubI16 e1 e2) = compileSub e1 e2 
compileExpr (MultI16 e1 e2) = compileMult e1 e2 
compileExpr (DivI16 e1 e2) = compileTwoSubExpr "div16" e1 e2  
compileExpr (RemI16 e1 e2) = compileMod e1 e2 
compileExpr (QuotI16 e1 e2) = compileDiv e1 e2 
compileExpr (ModI16 e1 e2) = compileTwoSubExpr "mod16" e1 e2  
compileExpr (AndI16 e1 e2) = compileAnd e1 e2 
compileExpr (OrI16 e1 e2) = compileOr e1 e2 
compileExpr (XorI16 e1 e2) = compileXor e1 e2 
compileExpr (CompI16 e) = compileComp e 
compileExpr (ShfLI16 e1 e2) = compileShiftLeft e1 e2 
compileExpr (ShfRI16 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfI16 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBI16 e1 e2) = compileTwoSubExpr "testBI16" e1 e2
compileExpr (SetBI16 e1 e2) = compileTwoSubExpr "setBI16" e1 e2 
compileExpr (ClrBI16 e1 e2) = compileTwoSubExpr "clrBI16" e1 e2
compileExpr (LitI32 w) = show w
compileExpr (ShowI32 e) = compileSubExpr "showInt32" e
compileExpr (RefI32 n) = compileRef "int32Val" n
compileExpr (RemBindI32 b) = compileBind b
compileExpr (NegI32 e) = compileNeg e
compileExpr (SignI32 e) = compileSubExpr "sign32" e
compileExpr (AddI32 e1 e2) = compileAdd e1 e2 
compileExpr (SubI32 e1 e2) = compileSub e1 e2 
compileExpr (MultI32 e1 e2) = compileMult e1 e2 
compileExpr (DivI32 e1 e2) = compileTwoSubExpr "div32" e1 e2  
compileExpr (RemI32 e1 e2) = compileMod e1 e2 
compileExpr (QuotI32 e1 e2) = compileDiv e1 e2
compileExpr (ModI32 e1 e2) = compileTwoSubExpr "mod32" e1 e2 
compileExpr (AndI32 e1 e2) = compileAnd e1 e2 
compileExpr (OrI32 e1 e2) = compileOr e1 e2 
compileExpr (XorI32 e1 e2) = compileXor e1 e2 
compileExpr (CompI32 e) = compileComp e
compileExpr (ShfLI32 e1 e2) = compileShiftLeft e1 e2 
compileExpr (ShfRI32 e1 e2) = compileShiftRight e1 e2 
compileExpr (IfI32 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TestBI32 e1 e2) = compileTwoSubExpr "testBI32" e1 e2 
compileExpr (SetBI32 e1 e2) = compileTwoSubExpr "setBI32" e1 e2 
compileExpr (ClrBI32 e1 e2) = compileTwoSubExpr "clrBI32" e1 e2  
compileExpr (LitList8 ws) = "{" ++ (show $ length ws) ++ compListLit ws
  where
    compListLit :: [Word8] -> String
    compListLit [] = "}"
    compListLit (w : ws) = "," ++ show w ++ compListLit ws
compileExpr (RefList8 n) = compileRef "list8Val" n
compileExpr (RemBindList8 b) = compileBind b
compileExpr (IfL8 e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (ElemList8 e1 e2) = compileTwoSubExpr "list8Elem" e1 e2
compileExpr (LenList8 e) = compileSubExpr "list8Len" e
compileExpr (ConsList8 e1 e2) = compileTwoSubExpr "list8Cons" e1 e2
compileExpr (ApndList8 e1 e2) = compileTwoSubExpr "list8Apnd" e1 e2
-- ToDo:
-- compileExpr (PackList8 es) = [exprLCmdVal EXPRL_PACK, fromIntegral $ length es] ++ (foldl (++) [] (map compileExpr es))
compileExpr (LitFloat f) = show f -- ToDo:  Is this correct?
compileExpr (ShowFloat e1 e2) = compileTwoSubExpr "showF" e1 e2
compileExpr (RefFloat n) = compileRef "floatVal" n
compileExpr (RemBindFloat b) = compileBind b
compileExpr (FromIntFloat e) = compileFromInt "float" e
compileExpr (NegFloat e) = compileNeg e
compileExpr (SignFloat e) = compileSubExpr "signF" e
compileExpr (AddFloat e1 e2) = compileAdd e1 e2 
compileExpr (SubFloat e1 e2) = compileSub e1 e2 
compileExpr (MultFloat e1 e2) = compileMult e1 e2 
compileExpr (DivFloat e1 e2) = compileDiv e1 e2 
compileExpr (IfFloat e1 e2 e3) = compileIfSubExpr e1 e2 e3
compileExpr (TruncFloat e) = compileSubExpr "trunc" e 
compileExpr (FracFloat e) = compileSubExpr "frac" e
compileExpr (RoundFloat e) = compileSubExpr "round" e 
compileExpr (CeilFloat e) = compileSubExpr "ceil" e 
compileExpr (FloorFloat e) = compileSubExpr "floor" e 
compileExpr PiFloat = "M_PI"
compileExpr (ExpFloat e) = compileSubExpr "exp" e 
compileExpr (LogFloat e) = compileSubExpr "log" e 
compileExpr (SqrtFloat e) = compileSubExpr "sqrt" e 
compileExpr (SinFloat e) = compileSubExpr "sin" e 
compileExpr (CosFloat e) = compileSubExpr "cos" e  
compileExpr (TanFloat e) = compileSubExpr "tan" e  
compileExpr (AsinFloat e) = compileSubExpr "asin" e 
compileExpr (AcosFloat e) = compileSubExpr "acos" e 
compileExpr (AtanFloat e) = compileSubExpr "atan" e 
compileExpr (Atan2Float e1 e2) = compileTwoSubExpr "atan2" e1 e2 
compileExpr (SinhFloat e) = compileSubExpr "sinh" e 
compileExpr (CoshFloat e) = compileSubExpr "cosh" e 
compileExpr (TanhFloat e) = compileSubExpr "tanh" e 
compileExpr (PowerFloat e1 e2) = compileTwoSubExpr "pow" e1 e2 
compileExpr (IsNaNFloat e) = compileSubExpr "isnan" e 
compileExpr (IsInfFloat e) = compileSubExpr "isinf" e 

