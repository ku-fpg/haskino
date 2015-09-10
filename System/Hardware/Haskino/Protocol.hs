-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Protocol
--                Based on System.Hardware.Arduino.Protocol
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the Haskino Fimrware protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module System.Hardware.Haskino.Protocol(framePackage, packageCommand, 
                                            packageProcedure, packageRemoteBinding,
                                            unpackageResponse, parseQueryResult,
                                            maxFirmwareSize) where

import Data.Bits            (xor)
import Data.Word (Word8, Word16, Word32)

import Control.Concurrent   (modifyMVar_, readMVar)
import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

-- | Maximum size of a Haskino Firmware message
maxFirmwareSize :: Int
maxFirmwareSize = 256

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
-- using the Haskino Firmware protocol.
packageCommand :: Command -> B.ByteString
packageCommand SystemReset = buildCommand BC_CMD_SYSTEM_RESET []
packageCommand (SetPinMode p m) = 
    buildCommand BC_CMD_SET_PIN_MODE [p, fromIntegral $ fromEnum m]
packageCommand (SetPinModeE p m) =
    buildCommand BC_CMD_SET_PIN_MODE_E (packageExpr p ++ [fromIntegral $ fromEnum m])
packageCommand (DigitalWrite p b) = 
    buildCommand DIG_CMD_WRITE_PIN [p, if b then 1 else 0]
packageCommand (DigitalWriteE p b) =
    buildCommand DIG_CMD_WRITE_PIN_E (packageExpr p ++ packageExpr b)
packageCommand (AnalogWrite p w) = 
    buildCommand ALG_CMD_WRITE_PIN (p : (word16ToBytes w))
packageCommand (AnalogWriteE p w) =
    buildCommand ALG_CMD_WRITE_PIN_E (packageExpr p ++ packageExpr w)
packageCommand (Tone p f (Just d)) =
    buildCommand ALG_CMD_TONE_PIN (p : (word16ToBytes f) ++ (word32ToBytes d))
packageCommand (Tone p f Nothing) =
    packageCommand (Tone p f (Just 0))
packageCommand (ToneE p f (Just d)) =
    buildCommand ALG_CMD_TONE_PIN_E (packageExpr p ++ packageExpr f ++ packageExpr d)
packageCommand (ToneE p f Nothing) =
    packageCommand (ToneE p f (Just 0))
packageCommand (NoTone p) =
    buildCommand ALG_CMD_NOTONE_PIN [p]
packageCommand (NoToneE p) =
    buildCommand ALG_CMD_NOTONE_PIN_E (packageExpr  p)
packageCommand (I2CWrite sa w8s) = 
    buildCommand I2C_CMD_WRITE (sa : w8s)
-- ToDo: packageCommand c (I2CWriteE sa w8s) = 
--    return $ buildCommand I2C_CMD_WRITE_E (sa : w8s)
packageCommand I2CConfig = 
    buildCommand I2C_CMD_CONFIG []
packageCommand (DeleteTask tid) = 
    buildCommand SCHED_CMD_DELETE_TASK [tid]
packageCommand (DeleteTaskE tid) =
    buildCommand SCHED_CMD_DELETE_TASK_E (packageExpr tid)
packageCommand (ScheduleTask tid tt) = 
    buildCommand SCHED_CMD_SCHED_TASK (tid : word32ToBytes tt)
packageCommand (ScheduleTaskE tid tt) =
    buildCommand SCHED_CMD_SCHED_TASK_E (packageExpr tid ++ packageExpr tt)
packageCommand (CreateTask tid m) =
    (framePackage cmd) `B.append` (genAddToTaskCmds td)
  where
    td = packageCodeBlock m
    taskSize = fromIntegral (B.length td)
    cmd = buildCommand SCHED_CMD_CREATE_TASK (tid : (word16ToBytes taskSize))                                   
    -- Max command data size is max frame size - 3 (command,checksum,frame flag) 
    maxCmdSize = maxFirmwareSize - 3
    genAddToTaskCmds tds | fromIntegral (B.length tds) > maxCmdSize = 
        addToTask (B.take maxCmdSize tds) 
            `B.append` (genAddToTaskCmds (B.drop maxCmdSize tds))
    genAddToTaskCmds tds = addToTask tds
    addToTask tds' = framePackage $ buildCommand SCHED_CMD_ADD_TO_TASK ([tid, fromIntegral $ B.length tds'] ++ (B.unpack tds'))
packageCommand (CreateTaskE tid m) =
    (framePackage cmd) `B.append` (genAddToTaskCmds td)
  where
    td = packageCodeBlock m
    taskSize = fromIntegral (B.length td)
    cmd = buildCommand SCHED_CMD_CREATE_TASK_E ((packageExpr tid) ++ (packageExpr (Lit16 taskSize)))                                   
    -- Max command data size is max frame size - 3 (command,checksum,frame flag) 
    maxCmdSize = maxFirmwareSize - 3
    genAddToTaskCmds tds | fromIntegral (B.length tds) > maxCmdSize = 
        addToTask (B.take maxCmdSize tds) 
            `B.append` (genAddToTaskCmds (B.drop maxCmdSize tds))
    genAddToTaskCmds tds = addToTask tds
    addToTask tds' = framePackage $ buildCommand SCHED_CMD_ADD_TO_TASK_E ((packageExpr tid) ++ 
                                                                          (packageExpr (Lit8 (fromIntegral (B.length tds')))) ++ 
                                                                          (B.unpack tds'))
packageCommand (WriteRemoteRefB (RemoteRefB i) e) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_BOOL, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRef8 (RemoteRefW8 i) e) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD8, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRef16 (RemoteRefW16 i) e) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD16, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRef32 (RemoteRefW32 i) e) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD32, fromIntegral i] ++ packageExpr e)
packageCommand (WriteEffectRemoteRefB (RemoteRefB i) cb) =
    buildCommand REF_CMD_WRITE_EFFECT ([refTypeCmdVal REF_BOOL, fromIntegral i] ++ B.unpack (packageCodeBlock cb))
packageCommand (WriteEffectRemoteRef8 (RemoteRefW8 i) cb) =
    buildCommand REF_CMD_WRITE_EFFECT ([refTypeCmdVal REF_WORD8, fromIntegral i] ++ B.unpack (packageCodeBlock cb))
packageCommand (WriteEffectRemoteRef16 (RemoteRefW16 i) cb) =
    buildCommand REF_CMD_WRITE_EFFECT ([refTypeCmdVal REF_WORD16, fromIntegral i] ++ B.unpack (packageCodeBlock cb))
packageCommand (WriteEffectRemoteRef32 (RemoteRefW32 i) cb) =
    buildCommand REF_CMD_WRITE_EFFECT ([refTypeCmdVal REF_WORD32, fromIntegral i] ++ B.unpack (packageCodeBlock cb))
packageCommand (ModifyRemoteRefB (RemoteRefB i) f) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_BOOL, fromIntegral i] ++ packageExpr (f (RefB i)))
packageCommand (ModifyRemoteRef8 (RemoteRefW8 i) f) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD8, fromIntegral i] ++ packageExpr (f (Ref8 i)))
packageCommand (ModifyRemoteRef16 (RemoteRefW16 i) f) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD16, fromIntegral i] ++ packageExpr (f (Ref16 i)))
packageCommand (ModifyRemoteRef32 (RemoteRefW32 i) f) =
    buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD32, fromIntegral i] ++ packageExpr (f (Ref32 i)))
-- ToDo: Do we need to check maximum frame size on conditionals?
packageCommand (While e cb) =
    buildCommand BC_CMD_WHILE ((packageExpr e) ++ (B.unpack pc))
  where
    pc = packageCodeBlock cb
packageCommand (IfThenElse e cb1 cb2) =
    buildCommand BC_CMD_IF_THEN_ELSE (thenSize ++ pe ++ (B.unpack pc1) ++ (B.unpack pc2))
  where
    pe = packageExpr e
    pc1 = packageCodeBlock cb1  
    pc2 = packageCodeBlock cb2  
    thenSize = word16ToBytes $ fromIntegral (B.length pc1)

-- ToDo:  Create locally scoped remote refs by passing ID offsets 
-- through pack calls and incrementing them whan packing newRemoteRef calls.
packageCodeBlock :: Arduino a -> B.ByteString
packageCodeBlock commands =
      packageCodeBlock' commands B.empty
  where
      packBind :: Arduino a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packBind (Return a) k cmds = packageCodeBlock' (k a) cmds
      packBind (Bind m k1) k2 cmds = packBind m (\ r -> Bind (k1 r) k2) cmds
      packBind (Command cmd) k cmds =
          -- Instead of framing each command as is done with sending them
          -- seperately, here a byte which contains the command length
          -- is prepended.
          packageCodeBlock' (k ()) (B.append cmds (lenPackage (packageCommand cmd)))
      packBind (Local local) k cmds = packLocal local k cmds
      packBind (Procedure procedure) k cmds = packProcedure procedure k cmds

      -- For sending as part of a Scheduler task, locals make no sense.  
      -- Instead of signalling an error, at this point they are just ignored.
      packLocal :: Local a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packLocal (Debug _) k cmds = packageCodeBlock' (k ()) cmds
      packLocal (Die _ _) k cmds = packageCodeBlock' (k ()) cmds

      packProcedure :: Procedure a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packProcedure QueryFirmware k cmds = packageCodeBlock' (k 0) (B.append cmds (lenPackage (packageProcedure QueryFirmware)))
      packProcedure QueryFirmwareE k cmds = packageCodeBlock' (k (lit 0)) (B.append cmds (lenPackage (packageProcedure QueryFirmwareE)))
      packProcedure QueryProcessor k cmds = packageCodeBlock' (k ATMEGA8) (B.append cmds (lenPackage (packageProcedure QueryProcessor)))
      packProcedure Micros k cmds = packageCodeBlock' (k 0) (B.append cmds (lenPackage (packageProcedure Micros)))
      packProcedure MicrosE k cmds = packageCodeBlock' (k (lit 0)) (B.append cmds (lenPackage (packageProcedure MicrosE)))
      packProcedure Millis k cmds = packageCodeBlock' (k 0) (B.append cmds (lenPackage (packageProcedure Millis)))
      packProcedure MillisE k cmds = packageCodeBlock' (k (lit 0)) (B.append cmds (lenPackage (packageProcedure MillisE)))
      packProcedure (DelayMillis ms) k cmds = packageCodeBlock' (k ()) (B.append cmds (lenPackage (packageProcedure (DelayMillis ms))))
      packProcedure (DelayMillisE ms) k cmds = packageCodeBlock' (k ()) (B.append cmds (lenPackage (packageProcedure (DelayMillisE ms))))
      packProcedure (DelayMicros ms) k cmds = packageCodeBlock' (k ()) (B.append cmds (lenPackage (packageProcedure (DelayMicros ms))))
      packProcedure (DelayMicrosE ms) k cmds = packageCodeBlock' (k ()) (B.append cmds (lenPackage (packageProcedure (DelayMicrosE ms))))
      packProcedure (DigitalRead p) k cmds = packageCodeBlock' (k False) (B.append cmds (lenPackage (packageProcedure (DigitalRead p))))
      packProcedure (DigitalReadE p) k cmds = packageCodeBlock' (k (lit False)) (B.append cmds (lenPackage (packageProcedure (DigitalReadE p))))
      packProcedure (AnalogRead p) k cmds = packageCodeBlock' (k 0) (B.append cmds (lenPackage (packageProcedure (AnalogRead p))))
      packProcedure (AnalogReadE p) k cmds = packageCodeBlock' (k (lit 0)) (B.append cmds (lenPackage (packageProcedure (AnalogReadE p))))
      packProcedure (I2CRead p n) k cmds = packageCodeBlock' (k []) (B.append cmds (lenPackage (packageProcedure (I2CRead p n))))
      packProcedure (I2CReadE p n) k cmds = packageCodeBlock' (k []) (B.append cmds (lenPackage (packageProcedure (I2CReadE p n))))
      packProcedure QueryAllTasks k cmds = packageCodeBlock' (k ([])) (B.append cmds (lenPackage (packageProcedure QueryAllTasks)))
      packProcedure (QueryTask t) k cmds = packageCodeBlock' (k Nothing) (B.append cmds (lenPackage (packageProcedure (QueryTask t))))
      packProcedure (QueryTaskE t) k cmds = packageCodeBlock' (k Nothing) (B.append cmds (lenPackage (packageProcedure (QueryTaskE t))))
      packProcedure (ReadRemoteRefB (RemoteRefB i)) k cmds = packageCodeBlock' (k (RefB i)) cmds
      packProcedure (ReadRemoteRef8 (RemoteRefW8 i)) k cmds = packageCodeBlock' (k (Ref8 i)) cmds
      packProcedure (ReadRemoteRef16 (RemoteRefW16 i)) k cmds = packageCodeBlock' (k (Ref16 i)) cmds
      packProcedure (ReadRemoteRef32 (RemoteRefW32 i)) k cmds = packageCodeBlock' (k (Ref32 i)) cmds

      packageCodeBlock' :: Arduino a -> B.ByteString -> B.ByteString
      packageCodeBlock' (Bind m k) cmds = packBind m k cmds
      packageCodeBlock' (Return a) cmds = cmds
      packageCodeBlock' cmd        cmds = packBind cmd Return cmds

      lenPackage :: B.ByteString -> B.ByteString
      lenPackage package = B.cons (fromIntegral $ B.length package) package      

packageProcedure :: Procedure a -> B.ByteString
packageProcedure QueryFirmware       = buildCommand BS_CMD_REQUEST_VERSION []
packageProcedure QueryFirmwareE      = buildCommand BS_CMD_REQUEST_VERSION []
packageProcedure QueryProcessor      = buildCommand BS_CMD_REQUEST_TYPE []
packageProcedure Micros              = buildCommand BS_CMD_REQUEST_MICROS []
packageProcedure MicrosE             = buildCommand BS_CMD_REQUEST_MICROS[]
packageProcedure Millis              = buildCommand BS_CMD_REQUEST_MILLIS []
packageProcedure MillisE             = buildCommand BS_CMD_REQUEST_MILLIS []
packageProcedure (DigitalRead p)     = buildCommand DIG_CMD_READ_PIN [p]
packageProcedure (DigitalReadE pe)   = buildCommand DIG_CMD_READ_PIN_E (packageExpr pe)
packageProcedure (AnalogRead p)      = buildCommand ALG_CMD_READ_PIN [p]
packageProcedure (AnalogReadE pe)    = buildCommand ALG_CMD_READ_PIN_E (packageExpr pe)
packageProcedure (I2CRead sa cnt)    = buildCommand I2C_CMD_READ [sa,cnt]
packageProcedure (I2CReadE sae cnte) = buildCommand I2C_CMD_READ_E ((packageExpr sae) ++ (packageExpr cnte))
packageProcedure QueryAllTasks       = buildCommand SCHED_CMD_QUERY_ALL []
packageProcedure (QueryTask tid)     = buildCommand SCHED_CMD_QUERY [tid]
packageProcedure (QueryTaskE tide)   = buildCommand SCHED_CMD_QUERY_E (packageExpr tide)
packageProcedure (DelayMillis ms)    = buildCommand BC_CMD_DELAY_MILLIS (word32ToBytes ms)
packageProcedure (DelayMillisE ms)   = buildCommand BC_CMD_DELAY_MILLIS_E (packageExpr ms)
packageProcedure (DelayMicros ms)    = buildCommand BC_CMD_DELAY_MICROS (word32ToBytes ms)
packageProcedure (DelayMicrosE ms)   = buildCommand BC_CMD_DELAY_MICROS_E (packageExpr ms)

packageRemoteBinding :: RemoteBinding a -> B.ByteString
packageRemoteBinding (NewRemoteRefB e)   = buildCommand REF_CMD_NEW ((refTypeCmdVal REF_BOOL) : (packageExpr e))
packageRemoteBinding (NewRemoteRef8 e)   = buildCommand REF_CMD_NEW ((refTypeCmdVal REF_WORD8) : (packageExpr e))
packageRemoteBinding (NewRemoteRef16 e)  = buildCommand REF_CMD_NEW ((refTypeCmdVal REF_WORD16) : (packageExpr e))
packageRemoteBinding (NewRemoteRef32 e)  = buildCommand REF_CMD_NEW ((refTypeCmdVal REF_WORD32) : (packageExpr e))

packageSubExpr :: Word8 -> Expr a -> [Word8]
packageSubExpr ec e = ec : packageExpr e

packageTwoSubExpr :: Word8 -> Expr a -> Expr b -> [Word8]
packageTwoSubExpr ec e1 e2 = ec : (packageExpr e1) ++ (packageExpr e2)

packageIfBSubExpr :: Word8 -> Expr a -> Expr b -> Expr b -> [Word8]
packageIfBSubExpr ec e1 e2 e3 = ec : thenSize ++ elseSize ++ pcond ++ pthen ++ pelse
  where
    pcond = packageExpr e1
    pthen = packageExpr e2
    pelse = packageExpr e3
    thenSize = word16ToBytes $ fromIntegral $ length pthen
    elseSize = word16ToBytes $ fromIntegral $ length pelse

packageRef :: Int -> Word8 -> [Word8]
packageRef n ec = [ec, fromIntegral n]

packageExpr :: Expr a -> [Word8]
packageExpr (LitB b) = [exprCmdVal EXPR_BOOL EXPR_LIT, if b then 1 else 0]
packageExpr (RefB n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (NotB e) = packageSubExpr (exprCmdVal EXPR_BOOL EXPR_NOT) e 
packageExpr (AndB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_AND) e1 e2 
packageExpr (OrB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_OR) e1 e2 
packageExpr (Eq8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_EQ) e1 e2 
packageExpr (Less8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_LESS) e1 e2 
packageExpr (Eq16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_EQ) e1 e2 
packageExpr (Less16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_LESS) e1 e2 
packageExpr (Eq32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_EQ) e1 e2 
packageExpr (Less32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_LESS) e1 e2 
packageExpr (Lit8 w) = [exprCmdVal EXPR_WORD8 EXPR_LIT, w]
packageExpr (Ref8 n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (Neg8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_NEG) e
packageExpr (Sign8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_SIGN) e
packageExpr (Add8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_ADD) e1 e2 
packageExpr (Sub8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SUB) e1 e2 
packageExpr (Mult8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_MULT) e1 e2 
packageExpr (Div8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_DIV) e1 e2 
packageExpr (Rem8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_REM) e1 e2 
packageExpr (And8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_AND) e1 e2 
packageExpr (Or8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_OR) e1 e2 
packageExpr (Xor8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_XOR) e1 e2 
packageExpr (Comp8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_COMP) e 
packageExpr (ShfL8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHFL) e1 e2 
packageExpr (ShfR8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHFR) e1 e2 
packageExpr (If8 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD8 EXPR_IF) e1 e2 e3
packageExpr (Bit8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_BIT) e
packageExpr (SetB8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SETB) e1 e2 
packageExpr (ClrB8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_CLRB) e1 e2 
packageExpr (Lit16 w) = (exprCmdVal EXPR_WORD16 EXPR_LIT) : word16ToBytes w
packageExpr (Ref16 n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (Neg16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_NEG) e
packageExpr (Sign16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_SIGN) e
packageExpr (Add16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_ADD) e1 e2 
packageExpr (Sub16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SUB) e1 e2 
packageExpr (Mult16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_MULT) e1 e2 
packageExpr (Div16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_DIV) e1 e2 
packageExpr (Rem16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_REM) e1 e2 
packageExpr (And16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_AND) e1 e2 
packageExpr (Or16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_OR) e1 e2 
packageExpr (Xor16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_XOR) e1 e2 
packageExpr (Comp16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_COMP) e 
packageExpr (ShfL16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHFL) e1 e2 
packageExpr (ShfR16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHFR) e1 e2 
packageExpr (If16 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD16 EXPR_IF) e1 e2 e3
packageExpr (Bit16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_BIT) e
packageExpr (SetB16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SETB) e1 e2 
packageExpr (ClrB16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_CLRB) e1 e2 
packageExpr (Lit32 w) = (exprCmdVal EXPR_WORD32 EXPR_LIT) : word32ToBytes w
packageExpr (Ref32 n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (Neg32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_NEG) e
packageExpr (Sign32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_SIGN) e
packageExpr (Add32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_ADD) e1 e2 
packageExpr (Sub32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SUB) e1 e2 
packageExpr (Mult32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_MULT) e1 e2 
packageExpr (Div32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_DIV) e1 e2 
packageExpr (Rem32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_REM) e1 e2 
packageExpr (And32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_AND) e1 e2 
packageExpr (Or32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_OR) e1 e2 
packageExpr (Xor32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_XOR) e1 e2 
packageExpr (Comp32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_COMP) e
packageExpr (ShfL32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHFL) e1 e2 
packageExpr (ShfR32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHFR) e1 e2 
packageExpr (If32 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD32 EXPR_IF) e1 e2 e3
packageExpr (Bit32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_BIT) e
packageExpr (SetB32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SETB) e1 e2 
packageExpr (ClrB32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_CLRB) e1 e2 

-- | Unpackage a Haskino Firmware response
unpackageResponse :: [Word8] -> Response
unpackageResponse [] = Unimplemented (Just "<EMPTY-REPLY>") []
unpackageResponse (cmdWord:args)
  | Right cmd <- getFirmwareReply cmdWord
  = case (cmd, args) of
      (BC_RESP_DELAY, [])             -> DelayResp
      (BS_RESP_VERSION, [majV, minV]) -> Firmware (bytesToWord16 (majV,minV))
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
      (REF_RESP_NEW , [w])            -> NewReply w
      (REF_RESP_NEW , [])             -> FailedNewRef
      _                               -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
-- ToDo: Fix I2CReadE and query task expr versions.
parseQueryResult :: Arduino a -> Response -> Maybe a
parseQueryResult (Procedure QueryFirmware) (Firmware v) = Just v
parseQueryResult (Procedure QueryFirmwareE) (Firmware v) = Just (lit v)
parseQueryResult (Procedure QueryProcessor) (ProcessorType pt) = Just $ getProcessor pt
parseQueryResult (Procedure Micros) (MicrosReply m) = Just m
parseQueryResult (Procedure MicrosE) (MicrosReply m) = Just (lit m)
parseQueryResult (Procedure Millis) (MillisReply m) = Just m
parseQueryResult (Procedure MillisE) (MillisReply m) = Just (lit m)
parseQueryResult (Procedure (DelayMicros m)) DelayResp = Just ()
parseQueryResult (Procedure (DelayMicrosE m)) DelayResp = Just ()
parseQueryResult (Procedure (DelayMillis m)) DelayResp = Just ()
parseQueryResult (Procedure (DelayMillisE m)) DelayResp = Just ()
parseQueryResult (Procedure (DigitalRead p)) (DigitalReply d) = Just (if d == 0 then False else True)
parseQueryResult (Procedure (DigitalReadE p)) (DigitalReply d) = Just (if d == 0 then lit False else lit True)
parseQueryResult (Procedure (AnalogRead p)) (AnalogReply a) = Just a
parseQueryResult (Procedure (AnalogReadE p)) (AnalogReply a) = Just (lit a)
parseQueryResult (Procedure (I2CRead saq cnt)) (I2CReply ds) = Just ds
parseQueryResult (Procedure (I2CReadE saq cnt)) (I2CReply ds) = Just ds
parseQueryResult (Procedure QueryAllTasks) (QueryAllTasksReply ts) = Just ts
parseQueryResult (Procedure (QueryTask tid)) (QueryTaskReply tr) = Just tr
parseQueryResult (Procedure (QueryTaskE tid)) (QueryTaskReply tr) = Just tr
parseQueryResult (RemoteBinding (NewRemoteRefB _)) (NewReply r) = Just $ RemoteRefB $ fromIntegral r
parseQueryResult (RemoteBinding (NewRemoteRef8 _)) (NewReply r) = Just $ RemoteRefW8 $ fromIntegral r
parseQueryResult (RemoteBinding (NewRemoteRef16 _)) (NewReply r) = Just $ RemoteRefW16 $ fromIntegral r
parseQueryResult (RemoteBinding (NewRemoteRef32 _)) (NewReply r) = Just $ RemoteRefW32 $ fromIntegral r
parseQueryResult q r = Nothing
