-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Protocol
--                Based on System.Hardware.Arduino.Protocol
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs      #-}

module System.Hardware.Haskino.Protocol(framePackage, packageCommand, 
                                            packageProcedure, packageRemoteBinding,
                                            unpackageResponse, parseQueryResult) where

import Data.Bits            (xor)
import Data.Word (Word8)

import Control.Concurrent   (modifyMVar_, readMVar)
import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

-- | Maximum size of a firmata message
maxFirmwareSize :: Int
maxFirmwareSize = 128

framePackage :: B.ByteString -> B.ByteString
framePackage bs = B.append (B.concatMap escape bs) (B.pack [check bs, 0x7E])
  where
    escape :: Word8 -> B.ByteString
    escape c = if c == 0x7E || c == 0x7D
               then B.pack $ [0x7D, xor c 0x20]
               else B.singleton c
    check b = B.foldl (+) 0 b

buildCommand :: FirmwareCmd -> [Word8] -> B.ByteString
buildCommand cmd bs = B.pack $ firmwareCmdVal cmd : bs

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
packageCommand :: ArduinoConnection -> Command -> IO B.ByteString
packageCommand c SystemReset = 
    return $ buildCommand BC_CMD_SYSTEM_RESET []
packageCommand c (SetPinMode p m) = do
    return $ buildCommand BC_CMD_SET_PIN_MODE [p, fromIntegral $ fromEnum m]
packageCommand c (SetPinModeE p m) = do
    pe <- packageExpr c p
    return $ buildCommand BC_CMD_SET_PIN_MODE_E (pe ++ [fromIntegral $ fromEnum m])
packageCommand c (DigitalWrite p b)  = do
    return $ buildCommand DIG_CMD_WRITE_PIN [p, if b then 1 else 0]
packageCommand c (DigitalWriteE p b)  = do
    pe <- packageExpr c p
    be <- packageExpr c b
    return $ buildCommand DIG_CMD_WRITE_PIN_E (pe ++ be)
packageCommand c (AnalogWrite p w) = do
    return $ buildCommand ALG_CMD_WRITE_PIN (p : (word16ToBytes w))
packageCommand c (AnalogWriteE p w) = do
    pe <- packageExpr c p
    we <- packageExpr c w
    return $ buildCommand ALG_CMD_WRITE_PIN_E (pe ++ we)
packageCommand c (Tone p f (Just d)) = do
    return $ buildCommand ALG_CMD_TONE_PIN (p : (word16ToBytes f) ++ (word32ToBytes d))
packageCommand c (Tone p f Nothing) = do
    packageCommand c (Tone p f (Just 0))
packageCommand c (ToneE p f (Just d)) = do
    pe <- packageExpr c p
    fe <- packageExpr c f
    de <- packageExpr c d
    return $ buildCommand ALG_CMD_TONE_PIN_E (pe ++ fe ++ de)
packageCommand c (ToneE p f Nothing) = do
    packageCommand c (ToneE p f (Just 0))
packageCommand c (NoTone p) = do
    return $ buildCommand ALG_CMD_NOTONE_PIN [p]
packageCommand c (NoToneE p) = do
    pe <- packageExpr c p
    return $ buildCommand ALG_CMD_NOTONE_PIN_E pe
packageCommand c (I2CWrite sa w8s) = 
    return $ buildCommand I2C_CMD_WRITE (sa : w8s)
-- ToDo: packageCommand c (I2CWriteE sa w8s) = 
--    return $ buildCommand I2C_CMD_WRITE_E (sa : w8s)
packageCommand c I2CConfig = 
    return $ buildCommand I2C_CMD_CONFIG []
packageCommand c (DeleteTask tid) = 
    return $ buildCommand SCHED_CMD_DELETE_TASK [tid]
packageCommand c (DeleteTaskE tid) = do
    tide <- packageExpr c tid
    return $ buildCommand SCHED_CMD_DELETE_TASK_E tide
packageCommand c (DelayMillis ms) = 
    return $ buildCommand BC_CMD_DELAY_MILLIS (word32ToBytes ms)
packageCommand c (DelayMillisE ms) = do
    mse <- packageExpr c ms
    return $ buildCommand BC_CMD_DELAY_MILLIS_E mse
packageCommand c (DelayMicros ms) = 
    return $ buildCommand BC_CMD_DELAY_MICROS (word32ToBytes ms)
packageCommand c (DelayMicrosE ms) = do
    mse <- packageExpr c ms
    return $ buildCommand BC_CMD_DELAY_MICROS_E mse
packageCommand c (ScheduleTask tid tt) = 
    return $ buildCommand SCHED_CMD_SCHED_TASK (tid : word32ToBytes tt)
packageCommand c (ScheduleTaskE tid tt) = do
    tide <- packageExpr c tid
    tte <- packageExpr c tt
    return $ buildCommand SCHED_CMD_SCHED_TASK_E (tide ++ tte)
packageCommand c (CreateTask tid m) = do
    td <- packageTaskData c m
    let taskSize = fromIntegral (B.length td)
    let cmd = buildCommand SCHED_CMD_CREATE_TASK (tid : (word16ToBytes taskSize))                                   
    return $ (framePackage cmd) `B.append` (genAddToTaskCmds td)
  where
    -- Max command data size is max frame size - 3 (command,checksum,frame flag) 
    maxCmdSize = maxFirmwareSize - 3
    genAddToTaskCmds tds | fromIntegral (B.length tds) > maxCmdSize = 
        addToTask (B.take maxCmdSize tds) 
            `B.append` (genAddToTaskCmds (B.drop maxCmdSize tds))
    genAddToTaskCmds tds = addToTask tds
    addToTask tds' = framePackage $ buildCommand SCHED_CMD_ADD_TO_TASK ([tid, fromIntegral $ B.length tds'] ++ (B.unpack tds'))
packageCommand c (AssignExprB v rh) = packageAssignExpr c VAR_CMD_ASGN_EXPRB v rh
packageCommand c (AssignExpr8 v rh) = packageAssignExpr c VAR_CMD_ASGN_EXPR8 v rh
packageCommand c (AssignExpr16 v rh) = packageAssignExpr c VAR_CMD_ASGN_EXPR16 v rh
packageCommand c (AssignExpr32 v rh) = packageAssignExpr c VAR_CMD_ASGN_EXPR32 v rh
packageCommand c (AssignProcB v rh) = packageAssignProc c VAR_CMD_ASGN_PROCB v rh
packageCommand c (AssignProc8 v rh) = packageAssignProc c VAR_CMD_ASGN_PROC8 v rh
packageCommand c (AssignProc16 v rh) = packageAssignProc c VAR_CMD_ASGN_PROC16 v rh
packageCommand c (AssignProc32 v rh) = packageAssignProc c VAR_CMD_ASGN_PROC32 v rh
-- ToDo: Do we need to check maximum frame size on conditionals?
packageCommand c (While e ps) = do
    pe <- packageExpr c e
    td <- packageTaskData c ps  
    return $ buildCommand BC_CMD_WHILE (pe ++ B.unpack td)
packageCommand c (IfThenElse e ps1 ps2) = do
    pe <- packageExpr c e
    td1 <- packageTaskData c ps1  
    td2 <- packageTaskData c ps2  
    let thenSize = word16ToBytes $ fromIntegral (B.length td1)
    return $ buildCommand BC_CMD_IF_THEN_ELSE (thenSize ++ pe ++ B.unpack td1 ++ B.unpack td2)

packageAssign :: ArduinoConnection -> Expr a -> IO Word8
packageAssign c lh = do
    v <- case lh of
              VarB s -> return s
              Var8 s -> return s
              Var16 s -> return s
              Var32 s -> return s
              otherwise -> runDie c "" []
    vn <- lookupVar c v
    return vn

packageAssignExpr :: ArduinoConnection -> FirmwareCmd -> Expr a -> Expr a -> IO B.ByteString
packageAssignExpr c fc lh rh = do
    vn <- packageAssign c lh
    rhe <- packageExpr c rh
    return $ buildCommand fc (vn : rhe)

packageAssignProc :: ArduinoConnection -> FirmwareCmd -> Expr a -> Arduino a -> IO B.ByteString
packageAssignProc c fc lh rh = do
    vn <- packageAssign c lh
    d <- packageTaskData c rh
    return $ buildCommand fc (vn : (B.unpack d))

packageTaskData :: ArduinoConnection -> Arduino a -> IO B.ByteString
packageTaskData conn commands =
      packageTaskData' conn commands B.empty
  where
      packBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packBind c (Return a)      k cmds = do
          cs <- cmds
          packageTaskData' c (k a) cs
      packBind c (Bind m k1)    k2 cmds = packBind c m (\ r -> Bind (k1 r) k2) cmds
      packBind c (Command cmd) k cmds = do
          packCmd <- packageCommand c cmd
          cs <- cmds
          -- Instead of framing each command as is done with sending them
          -- seperately, here a byte which contains the command length
          -- is prepended.
          packageTaskData' c (k ()) (B.append cs (lenPackage packCmd))
      packBind c (Local local) k cmds = packLocal c local k cmds
      packBind c (Procedure procedure) k cmds = packProcedure c procedure k cmds

      -- For sending as part of a Scheduler task, locals make no sense.  
      -- Instead of signalling an error, at this point they are just ignored.
      packLocal :: ArduinoConnection -> Local a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packLocal c (Debug _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k ()) cs
      packLocal c (Die _ _) k cmds = do
          cs <- cmds 
          packageTaskData' c (k ()) cs

      -- ToDo:  Add expression procedures, and actually add procedures
      -- to task stream, since they are now used in the AssignXxx commands
      packProcedure :: ArduinoConnection -> Procedure a -> (a -> Arduino b) -> IO B.ByteString -> IO B.ByteString
      packProcedure c QueryFirmware k cmds = do 
          cs <- cmds           
          packageTaskData' c (k (0,0)) cs
      packProcedure c QueryProcessor k cmds = do 
          cs <- cmds           
          packageTaskData' c (k ATMEGA8) cs
      packProcedure c (DigitalRead _) k cmds = do 
          cs <- cmds           
          packageTaskData' c (k False) cs
      packProcedure c (AnalogRead _) k cmds = do 
          cs <- cmds           
          packageTaskData' c (k 0) cs
      packProcedure c QueryAllTasks k cmds = do
          cs <- cmds
          packageTaskData' c (k ([])) cs
      packProcedure c (QueryTask _) k cmds = do
          cs <- cmds
          packageTaskData' c (k Nothing) cs

      packageTaskData' :: ArduinoConnection -> Arduino a -> B.ByteString -> IO B.ByteString
      packageTaskData' c (Bind m k) cmds = packBind c m k (return cmds)
      packageTaskData' c (Return a) cmds = return cmds
      packageTaskData' c cmd        cmds = packBind c cmd Return (return cmds)

      lenPackage :: B.ByteString -> B.ByteString
      lenPackage package = B.cons (fromIntegral $ B.length package) package      

packageProcedure :: Procedure a -> B.ByteString
packageProcedure QueryFirmware       = buildCommand BS_CMD_REQUEST_VERSION []
packageProcedure QueryProcessor      = buildCommand BS_CMD_REQUEST_TYPE []
packageProcedure Micros              = buildCommand BS_CMD_REQUEST_MICROS []
packageProcedure Millis              = buildCommand BS_CMD_REQUEST_MILLIS []
packageProcedure (DigitalRead p)     = buildCommand DIG_CMD_READ_PIN [p]
packageProcedure (AnalogRead p)      = buildCommand ALG_CMD_READ_PIN [p]
packageProcedure (I2CRead sa cnt)    = buildCommand I2C_CMD_READ [sa,cnt]
packageProcedure QueryAllTasks       = buildCommand SCHED_CMD_QUERY_ALL []
packageProcedure (QueryTask tid)     = buildCommand SCHED_CMD_QUERY [tid]

packageRemoteBinding :: RemoteBinding a -> B.ByteString
packageRemoteBinding (NewVarB _)   = buildCommand VAR_CMD_NEW [2] -- ToDo:  Are arduino booleans 2 bytes?
packageRemoteBinding (NewVar8 _)   = buildCommand VAR_CMD_NEW [1]
packageRemoteBinding (NewVar16 _)   = buildCommand VAR_CMD_NEW [2]
packageRemoteBinding (NewVar32 _)   = buildCommand VAR_CMD_NEW [4]

packageSubExpr :: ArduinoConnection -> ExprCmd -> Expr a -> IO [Word8]
packageSubExpr c ec e = do
    pe <- packageExpr c e
    return $ (exprCmdVal ec) : pe

packageTwoSubExpr :: ArduinoConnection -> ExprCmd -> Expr a -> Expr b -> IO [Word8]
packageTwoSubExpr c ec e1 e2 = do
    pe1 <- packageExpr c e1
    pe2 <- packageExpr c e2
    return $ (exprCmdVal ec) : (pe1 ++ pe2)

packageThreeSubExpr :: ArduinoConnection -> ExprCmd -> Expr a -> Expr b -> Expr c -> IO [Word8]
packageThreeSubExpr c ec e1 e2 e3 = do
    pe1 <- packageExpr c e1
    pe2 <- packageExpr c e2
    pe3 <- packageExpr c e3
    return $ (exprCmdVal ec) : (pe1 ++ pe2 ++ pe3)

-- ToDo: Add variable type checking
lookupVar :: ArduinoConnection -> String -> IO Word8
lookupVar c s = do
    vmap <- readMVar $ variables c
    case M.lookup s vmap of
        Just vn -> return vn 
        Nothing -> runDie c "Haskino:ERROR: Unallocated variable" 
                             [ "Variable name - " ++ s, 
                             "Make sure Variables are allocated before use"]

packageVar :: ArduinoConnection -> String -> ExprCmd -> IO [Word8]
packageVar c s ec = do
    vn <- lookupVar c s
    return [exprCmdVal ec, vn]

packageExpr :: ArduinoConnection -> Expr a -> IO [Word8]
packageExpr c (LitB b) = return $ [exprCmdVal EXPR_LITB, if b then 1 else 0]
packageExpr c (VarB s) = packageVar c s EXPR_VARB
packageExpr c (NotB e) = packageSubExpr c EXPR_NOTB e 
packageExpr c (AndB e1 e2) = packageTwoSubExpr c EXPR_ANDB e1 e2 
packageExpr c (OrB e1 e2) = packageTwoSubExpr c EXPR_ORB e1 e2 
packageExpr c (Eq8 e1 e2) = packageTwoSubExpr c EXPR_EQ8 e1 e2 
packageExpr c (Less8 e1 e2) = packageTwoSubExpr c EXPR_LESS8 e1 e2 
packageExpr c (Eq16 e1 e2) = packageTwoSubExpr c EXPR_EQ16 e1 e2 
packageExpr c (Less16 e1 e2) = packageTwoSubExpr c EXPR_LESS16 e1 e2 
packageExpr c (Eq32 e1 e2) = packageTwoSubExpr c EXPR_EQ32 e1 e2 
packageExpr c (Less32 e1 e2) = packageTwoSubExpr c EXPR_LESS32 e1 e2 
packageExpr c (Lit8 w) = return $ [exprCmdVal EXPR_LIT8, w]
packageExpr c (Var8 s) = packageVar c s EXPR_VAR8
packageExpr c (Neg8 e) = packageSubExpr c EXPR_NEG8 e
packageExpr c (Sign8 e) = packageSubExpr c EXPR_SIGN8 e
packageExpr c (Add8 e1 e2) = packageTwoSubExpr c EXPR_ADD8 e1 e2 
packageExpr c (Sub8 e1 e2) = packageTwoSubExpr c EXPR_SUB8 e1 e2 
packageExpr c (Mult8 e1 e2) = packageTwoSubExpr c EXPR_MULT8 e1 e2 
packageExpr c (Div8 e1 e2) = packageTwoSubExpr c EXPR_DIV8 e1 e2 
packageExpr c (Rem8 e1 e2) = packageTwoSubExpr c EXPR_REM8 e1 e2 
packageExpr c (And8 e1 e2) = packageTwoSubExpr c EXPR_AND8 e1 e2 
packageExpr c (Or8 e1 e2) = packageTwoSubExpr c EXPR_OR8 e1 e2 
packageExpr c (Xor8 e1 e2) = packageTwoSubExpr c EXPR_XOR8 e1 e2 
packageExpr c (If8 e1 e2 e3) = packageThreeSubExpr c EXPR_IF8 e1 e2 e3
packageExpr c (Lit16 w) = return $ (exprCmdVal EXPR_LIT16) : word16ToBytes w
packageExpr c (Var16 s) = packageVar c s EXPR_VAR16
packageExpr c (Neg16 e) = packageSubExpr c EXPR_NEG16 e
packageExpr c (Sign16 e) = packageSubExpr c EXPR_SIGN16 e
packageExpr c (Add16 e1 e2) = packageTwoSubExpr c EXPR_ADD16 e1 e2 
packageExpr c (Sub16 e1 e2) = packageTwoSubExpr c EXPR_SUB16 e1 e2 
packageExpr c (Mult16 e1 e2) = packageTwoSubExpr c EXPR_MULT16 e1 e2 
packageExpr c (Div16 e1 e2) = packageTwoSubExpr c EXPR_DIV16 e1 e2 
packageExpr c (Rem16 e1 e2) = packageTwoSubExpr c EXPR_REM16 e1 e2 
packageExpr c (And16 e1 e2) = packageTwoSubExpr c EXPR_AND16 e1 e2 
packageExpr c (Or16 e1 e2) = packageTwoSubExpr c EXPR_OR16 e1 e2 
packageExpr c (Xor16 e1 e2) = packageTwoSubExpr c EXPR_XOR16 e1 e2 
packageExpr c (If16 e1 e2 e3) = packageThreeSubExpr c EXPR_IF16 e1 e2 e3
packageExpr c (Lit32 w) = return $ (exprCmdVal EXPR_LIT32) : word32ToBytes w
packageExpr c (Var32 s) = packageVar c s EXPR_VAR32
packageExpr c (Neg32 e) = packageSubExpr c EXPR_NEG32 e
packageExpr c (Sign32 e) = packageSubExpr c EXPR_SIGN32 e
packageExpr c (Add32 e1 e2) = packageTwoSubExpr c EXPR_ADD32 e1 e2 
packageExpr c (Sub32 e1 e2) = packageTwoSubExpr c EXPR_SUB32 e1 e2 
packageExpr c (Mult32 e1 e2) = packageTwoSubExpr c EXPR_MULT32 e1 e2 
packageExpr c (Div32 e1 e2) = packageTwoSubExpr c EXPR_DIV32 e1 e2 
packageExpr c (Rem32 e1 e2) = packageTwoSubExpr c EXPR_REM32 e1 e2 
packageExpr c (And32 e1 e2) = packageTwoSubExpr c EXPR_AND32 e1 e2 
packageExpr c (Or32 e1 e2) = packageTwoSubExpr c EXPR_OR32 e1 e2 
packageExpr c (Xor32 e1 e2) = packageTwoSubExpr c EXPR_XOR32 e1 e2 
packageExpr c (If32 e1 e2 e3) = packageThreeSubExpr c EXPR_IF32 e1 e2 e3

-- | Unpackage a Amber Firmware response
unpackageResponse :: [Word8] -> Response
unpackageResponse [] = Unimplemented (Just "<EMPTY-REPLY>") []
unpackageResponse (cmdWord:args)
  | Right cmd <- getFirmwareReply cmdWord
  = case (cmd, args) of
      (BS_RESP_VERSION, [majV, minV]) -> Firmware majV minV
      (BS_RESP_TYPE, [p])             -> ProcessorType p
      (BS_RESP_MICROS, [m0,m1,m2,m3]) -> MicrosReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_MILLIS, [m0,m1,m2,m3]) -> MillisReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_STRING, rest)          -> StringMessage (getString rest)
      (DIG_RESP_READ_PIN, [b])        -> DigitalReply b
      (ALG_RESP_READ_PIN, [bl,bh])    -> AnalogReply (bytesToWord16 (bl,bh))
      (I2C_RESP_READ, xs)             -> I2CReply xs
      (SCHED_RESP_QUERY_ALL, ts)      -> QueryAllTasksReply ts
      (SCHED_RESP_QUERY, ts) | length ts == 0 -> 
          QueryTaskReply Nothing
      (SCHED_RESP_QUERY, ts) | length ts >= 9 -> 
          let ts0:ts1:tl0:tl1:tp0:tp1:tt0:tt1:tt2:tt3:rest = ts
          in QueryTaskReply (Just (bytesToWord16 (ts0,ts1), 
                                   bytesToWord16 (tl0,tl1),
                                   bytesToWord16 (tp0,tp1), 
                                   bytesToWord32 (tt0,tt1,tt2,tt3)))  
      (VAR_RESP_NEW , [w])            -> NewReply w
      _                               -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
parseQueryResult :: ArduinoConnection -> Arduino a -> Response -> IO (Maybe a)
parseQueryResult c (Procedure QueryFirmware) (Firmware wa wb) = return $ Just (wa,wb)
parseQueryResult c (Procedure QueryProcessor) (ProcessorType pt) = return $ Just $ getProcessor pt
parseQueryResult c (Procedure Micros) (MicrosReply m) = return $ Just m
parseQueryResult c (Procedure Millis) (MillisReply m) = return $ Just m
parseQueryResult c (Procedure (DigitalRead p)) (DigitalReply d) = return $ Just (if d == 0 then False else True)
parseQueryResult c (Procedure (DigitalReadE p)) (DigitalReply d) = return $ Just (if d == 0 then False else True)
parseQueryResult c (Procedure (AnalogRead p)) (AnalogReply a) = return $ Just a
parseQueryResult c (Procedure (AnalogReadE p)) (AnalogReply a) = return $ Just a
parseQueryResult c (Procedure (I2CRead saq cnt)) (I2CReply ds) = return $ Just ds
parseQueryResult c (Procedure (I2CReadE saq cnt)) (I2CReply ds) = return $ Just ds
parseQueryResult c (Procedure QueryAllTasks) (QueryAllTasksReply ts) = return $ Just ts
parseQueryResult c (Procedure (QueryTask tid)) (QueryTaskReply tr) = return $ Just tr
parseQueryResult c (Procedure (QueryTaskE tid)) (QueryTaskReply tr) = return $ Just tr
parseQueryResult c (RemoteBinding (NewVarB s)) (NewReply vn) = do
    updateVariables c s vn
    return $ Just $ VarB s
parseQueryResult c (RemoteBinding (NewVar8 s)) (NewReply vn) = do
    updateVariables c s vn
    return $ Just $ Var8 s
parseQueryResult c (RemoteBinding (NewVar16 s)) (NewReply vn) = do
    updateVariables c s vn
    return $ Just $ Var16 s
parseQueryResult c (RemoteBinding (NewVar32 s)) (NewReply vn) = do
    updateVariables c s vn
    return $ Just $ Var32 s
parseQueryResult c q r = return Nothing

updateVariables :: ArduinoConnection -> String -> Word8 -> IO ()
updateVariables c s w = 
    modifyMVar_ (variables c) $ \vs -> return $ M.insert s w vs
