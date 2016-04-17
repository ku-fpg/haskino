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

module System.Hardware.Haskino.Compiler(compileProgram, runTest) where 

import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Boolean

import Control.Monad.State

import Control.Remote.Monad
import Control.Remote.Monad.Types as T

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

data CompileState = CompileState {level :: Int 
                     , ix :: Int  
                     , ib :: Int  
                     , cmds :: String
                     , binds :: String
                     , refs :: String
                     , forwards :: String
                     , cmdList :: [String]
                     , bindList :: [String]
                     , tasksToDo :: [(Arduino (), String)]
                     , tasksDone :: [String] 
                     }    

data CompileType = 
    BoolType
  | Word8Type 
  | Word16Type
  | Word32Type
  | Int8Type
  | Int16Type
  | Int32Type
  | List8Type
  | FloatType

compileTypeToString :: CompileType -> String
compileTypeToString BoolType   = "bool"
compileTypeToString Word8Type  = "uint8_t"
compileTypeToString Word16Type = "uint16_t"
compileTypeToString Word32Type = "uint32_t"
compileTypeToString Int8Type   = "int8_t"
compileTypeToString Int16Type  = "int16_t"
compileTypeToString Int32Type  = "int32_t"
compileTypeToString List8Type  = "byte *"
compileTypeToString FloatType  = "float"

refName :: String
refName = "ref"

bindName :: String
bindName = "bind"

indentString :: String
indentString = "    "

indent :: Int -> String
indent k = concat $ replicate k indentString

myTask :: Arduino ()
myTask = do
  digitalWriteE 2 true
  digitalWriteE 3 false
  let l = lit [1,2,3,4]
  r <- newRemoteRef 0
  forInE l (\x -> modifyRemoteRef r (\a -> a + x))

myTest :: Arduino ()
myTest =  do
  r <- newRemoteRefB true
  createTaskE 1 myTask
  a <- millisE
  loopE $ do
    setPinModeE 2 INPUT 
    setPinModeE 3 OUTPUT
    b <- millisE
    return ()

runTest :: IO ()
runTest = compileProgram myTest "test.c"

compileProgram :: Arduino () -> FilePath -> IO ()
compileProgram p f = writeFile f prog
  where
    (_, st) = runState compileTasks  
                (CompileState 0 0 0 "" "" "" "" [] [] [(p, "haskinoMain")] [])
    prog = "#include \"HaskinoRuntime.h\"\n\n" ++ 
           forwards st ++ "\n" ++ refs st ++ "\n" ++ (concat $ tasksDone st)

compileTasks :: State CompileState ()
compileTasks = do
    s <- get
    let tasks = tasksToDo s
    if null tasks 
    then return ()
    else do
        put s {tasksToDo = tail tasks}
        compileTask (fst $ head tasks) (snd $ head tasks)
        compileTasks

compileTask :: Arduino () -> String -> State CompileState ()
compileTask t name = do
    s <- get
    put s {level = 0, ib = 0, cmds = "", binds = "", cmdList = [], bindList = []}
    compileLine $ "void " ++ name ++ "()"
    compileForward $ "void " ++ name ++ "();"
    compileCodeBlock t
    compileLine "" 
    s <- get
    put s {tasksDone = cmds s : tasksDone s}
    return ()

compileLine :: String -> State CompileState ()
compileLine s = do
    st <- get 
    put st { cmds = cmds st ++ (indent $ level st) ++ s ++ "\n"}
    return ()

compileAllocBind :: String -> State CompileState ()
compileAllocBind s = do
    st <- get 
    put st { binds = binds st ++ (indent $ level st) ++  s ++ "\n"}
    return ()

compileAllocRef :: String -> State CompileState ()
compileAllocRef s = do
    st <- get 
    put st { refs = refs st ++ s ++ "\n"}
    return ()

compileForward :: String -> State CompileState ()
compileForward s = do
    st <- get 
    put st { forwards = forwards st ++ s ++ "\n"}
    return ()

compileNoExprCommand :: String -> State CompileState ()
compileNoExprCommand s =
    compileLine (s ++ "();")

compile1ExprCommand :: String -> Expr a -> State CompileState ()
compile1ExprCommand s e =
    compileLine (s ++ "(" ++ compileExpr e ++ ");")

compile2ExprCommand :: String -> Expr a -> Expr b -> State CompileState ()
compile2ExprCommand s e1 e2 =
    compileLine (s ++ "(" ++ compileExpr e1 ++ "," ++ 
                                      compileExpr e2 ++ ");")

compile3ExprCommand :: String -> Expr a -> Expr b -> Expr c -> State CompileState ()
compile3ExprCommand s e1 e2 e3 =
    compileLine (s ++ "(" ++ compileExpr e1 ++ "," ++
                                      compileExpr e2 ++ "," ++ 
                                      compileExpr e3 ++ ");")

compileWriteRef :: Int -> Expr a -> State CompileState ()
compileWriteRef ix e = compileLine $ refName ++ show ix ++ " = " ++ compileExpr e ++ ";"

compileWhileCommand :: Expr a -> Expr b -> Arduino () -> State CompileState ()
compileWhileCommand be ue cb = do
    compileLine $ "for (;" ++ compileExpr be ++ ";" ++ compileExpr ue ++ ")"
    compileCodeBlock cb
    return ()

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
compileCommand (ServoDetachE sv) = 
    compile1ExprCommand "servoDetach" sv -- ToDo: runtime
compileCommand (ServoWriteE sv w) = 
    compile2ExprCommand "servoWrite" sv w -- ToDo: runtime
compileCommand (ServoWriteMicrosE sv w) = 
    compile2ExprCommand "servoWriteMicros" sv w -- ToDo: runtime
compileCommand (DeleteTaskE tid) = 
    compile1ExprCommand "deleteTask" tid -- ToDo: runtime
compileCommand (CreateTaskE (LitW8 tid) m) = do -- ToDo: runtime
    let taskName = "task" ++ show tid
    compileLine $ "createTask(" ++ show tid ++ ", " ++ taskName ++ "());"
    s <- get
    put s {tasksToDo = (m, taskName) : (tasksToDo s)}
compileCommand (ScheduleTaskE tid tt) = 
    compile2ExprCommand "scheduleTask" tid tt -- ToDo: runtime
compileCommand ScheduleReset = 
    compileNoExprCommand "scheduleReset" -- ToDo: runtime
compileCommand (AttachIntE p t m) = 
    compile3ExprCommand "attachInt" p t m -- ToDo: runtime + task name
compileCommand (DetachIntE p) = 
    compile1ExprCommand "detachInt" p -- ToDo: runtime
compileCommand Interrupts = 
    compileNoExprCommand "interrupts"
compileCommand NoInterrupts = 
    compileNoExprCommand "noInterrupts"
compileCommand (GiveSemE id) = 
    compile1ExprCommand "giveSem" id -- ToDo: runtime
compileCommand (TakeSemE id) = 
    compile1ExprCommand "takeSem" id -- ToDo: runtime
compileCommand (WriteRemoteRefB (RemoteRefB i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) = compileWriteRef i e
compileCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) = compileWriteRef i e
compileCommand (ModifyRemoteRefB (RemoteRefB i) f) = 
    compileWriteRef i (f (RefB i))
compileCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) = 
    compileWriteRef i (f (RefW8 i))
compileCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) = 
    compileWriteRef i (f (RefW16 i))
compileCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) = 
    compileWriteRef i (f (RefW32 i))
compileCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) = 
    compileWriteRef i (f (RefI8 i))
compileCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) = 
    compileWriteRef i (f (RefI16 i))
compileCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) = 
    compileWriteRef i (f (RefI32 i))
compileCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) = 
    compileWriteRef i (f (RefList8 i))
compileCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) = 
    compileWriteRef i (f (RefFloat i))
compileCommand (WhileRemoteRefB (RemoteRefB i) bf uf cb) = 
    compileWhileCommand (bf (RefB i)) (uf (RefB i)) cb
compileCommand (WhileRemoteRefW8 (RemoteRefW8 i) bf uf cb) = 
    compileWhileCommand (bf (RefW8 i)) (uf (RefW8 i)) cb
compileCommand (WhileRemoteRefW16 (RemoteRefW16 i) bf uf cb) = 
    compileWhileCommand (bf (RefW16 i)) (uf (RefW16 i)) cb
compileCommand (WhileRemoteRefW32 (RemoteRefW32 i) bf uf cb) = 
    compileWhileCommand (bf (RefW32 i)) (uf (RefW32 i)) cb
compileCommand (WhileRemoteRefI8 (RemoteRefI8 i) bf uf cb) = 
    compileWhileCommand (bf (RefI8 i)) (uf (RefI8 i)) cb
compileCommand (WhileRemoteRefI16 (RemoteRefI16 i) bf uf cb) = 
    compileWhileCommand (bf (RefI16 i)) (uf (RefI16 i)) cb
compileCommand (WhileRemoteRefI32 (RemoteRefI32 i) bf uf cb) = 
    compileWhileCommand (bf (RefI32 i)) (uf (RefI32 i)) cb
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
compileCommand (ForInE ws f) = do
    s <- get
    let belem = ib s
    let blist = belem + 1
    compileAllocBind $ compileTypeToString Word8Type ++ " bind" ++ show belem ++ ";"
    compileAllocBind $ compileTypeToString List8Type ++ " bind" ++ show blist ++ ";"
    put s {ib = belem + 1}
    let belemName = bindName ++ show belem
    let blistName = bindName ++ show blist
    compileLine $ "for (int i=0, " ++ blistName ++ " = " ++ compileExpr ws ++ ","
    compileLine $ "     " ++ belemName ++ " = list8Elem(" ++ blistName ++ ", 0);"
    compileLine $ "     i = list8Len(" ++ blistName ++ ");"
    compileLine $ "     i++, " ++ belemName ++ " = list8Elem(" ++ blistName ++ ", i))"
    compileCodeBlock (f (RemBindW8 belem))
    return ()

compileSimpleProcedure :: CompileType -> String -> State CompileState Int
compileSimpleProcedure t p = do
    s <- get
    let b = ib s
    compileLine $ bindName ++ show b ++ " = " ++ p ++ ";"
    compileAllocBind $ compileTypeToString t ++ " bind" ++ show b ++ ";"
    s <- get
    put s {ib = b + 1}
    return b

compileNoExprProcedure :: CompileType -> String -> State CompileState Int
compileNoExprProcedure t p = do
    b <- compileSimpleProcedure t (p ++ "()")
    return b

compile1ExprProcedure :: CompileType -> String -> Expr a -> State CompileState Int
compile1ExprProcedure t p e = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e ++ ")")
    return b

compile2ExprProcedure :: CompileType -> String -> 
                         Expr a -> Expr a -> State CompileState Int
compile2ExprProcedure t p e1 e2 = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e1 ++ "," ++ 
                                               compileExpr e2 ++ ")")
    return b

compile3ExprProcedure :: CompileType -> String -> 
                         Expr a -> Expr b  -> Expr c -> State CompileState Int
compile3ExprProcedure t p e1 e2 e3 = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e1 ++ "," ++
                                               compileExpr e2 ++ "," ++ 
                                               compileExpr e3 ++ ")")
    return b

compile5ExprProcedure :: CompileType -> String -> Expr a -> Expr b ->  
                         Expr c -> Expr d  -> Expr e -> State CompileState Int
compile5ExprProcedure t p e1 e2 e3 e4 e5 = do
    b <- compileSimpleProcedure t (p ++ "(" ++ compileExpr e1 ++ "," ++
                                               compileExpr e2 ++ "," ++ 
                                               compileExpr e3 ++ "," ++ 
                                               compileExpr e4 ++ "," ++ 
                                               compileExpr e5 ++ ")")
    return b

compileNewRef :: CompileType -> Expr a -> State CompileState Int
compileNewRef t e = do
    s <- get
    let x = ix s
    put s {ix = x + 1}
    compileAllocRef $ compileTypeToString t ++ " ref" ++ show x ++ ";"
    s <- get
    let b = ib s
    put s {ib = b + 1}
    compileLine $ refName ++ show b ++ " = " ++ compileExpr e ++ ";"
    return  x

compileReadRef :: CompileType -> Int -> State CompileState Int
compileReadRef t ix = do
    s <- get
    let b = ib s
    put s {ib = b + 1}
    compileLine $ bindName ++ show b ++ " = ref" ++ show ix ++ ";"
    compileAllocBind $ compileTypeToString t ++ " bind" ++ show b ++ ";"
    return b

compileProcedure :: ArduinoProcedure a -> State CompileState a
compileProcedure QueryFirmwareE = do
    b <- compileNoExprProcedure Word16Type "queryFirmware" -- ToDo: runtime
    return $ remBind b
compileProcedure QueryProcessorE = do
    b <- compileNoExprProcedure Word8Type "queryProcessor" -- ToDo: runtime
    return $ remBind b
compileProcedure MillisE = do
    b <- compileNoExprProcedure Word32Type "millis"
    return $ remBind b
compileProcedure MicrosE = do
    b <- compileNoExprProcedure Word32Type "micros"
    return $ remBind b
compileProcedure (DelayMillisE ms) = do
    compile1ExprCommand "delayMilliseconds" ms -- ToDo: runtime
    return ()
compileProcedure (DelayMicrosE ms) = do
    compile1ExprCommand "delayMicroseconds" ms
    return ()
compileProcedure (DigitalReadE p) = do
    b <- compile1ExprProcedure BoolType "digitalRead" p
    return $ remBind b
compileProcedure (DigitalPortReadE p m) = do
    b <- compile2ExprProcedure Word8Type "digitalPortRead" p m -- ToDo: runtime
    return $ remBind b
compileProcedure (AnalogReadE p) = do
    b <- compile1ExprProcedure Word16Type "analogRead" p
    return $ remBind b
compileProcedure (I2CReadE p n) = do
    b <- compile2ExprProcedure List8Type "i2cRead" p n -- ToDo: runtime
    return $ remBind b
compileProcedure (Stepper2PinE s p1 p2) = do
    b <- compile3ExprProcedure Word8Type "stepper2Pin" s p1 p2 -- ToDo: runtime
    return $ remBind b
compileProcedure (Stepper4PinE s p1 p2 p3 p4) = do
    b <- compile5ExprProcedure Word8Type "stepper4Pin" s p1 p2 p3 p4 -- ToDo: runtime
    return $ remBind b
compileProcedure (StepperStepE st s) = do
    compile2ExprCommand "stepperStep" st s
    return ()
compileProcedure (ServoAttachE p) = do
    b <- compile1ExprProcedure Word8Type "servoAttach" p -- ToDo: runtime
    return $ remBind b
compileProcedure (ServoAttachMinMaxE p min max) = do
    b <- compile3ExprProcedure Word8Type "servoAttachMixMax" p min max -- ToDo: runtime
    return $ remBind b
compileProcedure (ServoReadE sv) = do
    b <- compile1ExprProcedure Word16Type "servoRead" sv -- ToDo: runtime
    return $ remBind b
compileProcedure (ServoReadMicrosE sv) = do
    b <- compile1ExprProcedure Word16Type "servoReadMicros" sv -- ToDo: runtime
    return $ remBind b
compileProcedure QueryAllTasksE = do
    compileLine "/* queryAllTasksE not suppported by compiler */"
    return (litString "")
compileProcedure (QueryTaskE t) = do
    compileLine "/* queryTaskE not suppported by compiler */"
    return Nothing
compileProcedure (BootTaskE tids) = do
    compileLine "/* bootTaskE not suppported by compiler */"
    return true
compileProcedure (DebugE s) = do
    compile1ExprCommand "debug" s -- ToDo: Runtime
    return ()
compileProcedure (NewRemoteRefB e) = do
    x <- compileNewRef BoolType e
    return $ RemoteRefB x
compileProcedure (NewRemoteRefW8 e) = do
    x <- compileNewRef Word8Type e
    return $ RemoteRefW8 x
compileProcedure (NewRemoteRefW16 e) = do
    x <- compileNewRef Word16Type e
    return $ RemoteRefW16 x
compileProcedure (NewRemoteRefW32 e) = do
    x <- compileNewRef Word32Type e
    return $ RemoteRefW32 x
compileProcedure (NewRemoteRefI8 e) = do
    x <- compileNewRef Int8Type e
    return $ RemoteRefI8 x
compileProcedure (NewRemoteRefI16 e) = do
    x <- compileNewRef Int16Type e
    return $ RemoteRefI16 x
compileProcedure (NewRemoteRefI32 e) = do
    x <- compileNewRef Int32Type e
    return $ RemoteRefI32 x
compileProcedure (NewRemoteRefL8 e) = do
    x <- compileNewRef List8Type e
    return $ RemoteRefL8 x
compileProcedure (NewRemoteRefFloat e) = do
    x <- compileNewRef FloatType e
    return $ RemoteRefFloat x
compileProcedure (ReadRemoteRefB (RemoteRefB i)) = do
    b <- compileReadRef BoolType i
    return $ remBind b
compileProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) = do
    b <- compileReadRef Word8Type i
    return $ remBind b
compileProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) = do
    b <- compileReadRef Word16Type i
    return $ remBind b
compileProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) = do
    b <- compileReadRef Word32Type i
    return $ remBind b
compileProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) = do
    b <- compileReadRef Int8Type i
    return $ remBind b
compileProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) = do
    b <- compileReadRef Int16Type i
    return $ remBind b
compileProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) = do
    b <- compileReadRef Int32Type i
    return $ remBind b
compileProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) = do
    b <- compileReadRef List8Type i
    return $ remBind b
compileProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) = do
    b <- compileReadRef FloatType i
    return $ remBind b

compileCodeBlock :: Arduino a -> State CompileState a
compileCodeBlock (Arduino commands) = do
    s <- get
    put s {bindList = (binds s) : (bindList s),
           cmdList = (cmds s) : (cmdList s),
           binds = "",
           cmds = "",
           level = level s + 1 }
    r <- compileMonad commands
    s <- get
    put s {bindList = tail $ bindList s,
           cmdList = tail $ cmdList s,
           binds = head $ bindList s,
           cmds = (head $ cmdList s) ++ (indent $ level s) ++ "{\n" ++ 
                  body (binds s) (cmds s) ++ (indent $ level s) ++ "}\n",
           level = level s - 1 }
    return r
  where
      body :: String -> String -> String
      body [] cs = cs
      body bs cs = bs ++ "\n" ++ cs

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
compileBind b = bindName ++ show b
 
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
 
compileRef :: Int -> String
compileRef n = refName ++ show n

compileExpr :: Expr a -> String
compileExpr (LitB b) = if b then "1" else "0"
compileExpr (ShowB e) = compileSubExpr "showBool" e
compileExpr (RefB n) = compileRef n
compileExpr (RemBindB b) = bindName ++ show b
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
compileExpr (RefW8 n) = compileRef n
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
compileExpr (RefW16 n) = compileRef n
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
compileExpr (RefW32 n) = compileRef n
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
compileExpr (RefI8 n) = compileRef n
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
compileExpr (RefI16 n) = compileRef n
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
compileExpr (RefI32 n) = compileRef n
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
compileExpr (RefList8 n) = compileRef n
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
compileExpr (RefFloat n) = compileRef n
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

