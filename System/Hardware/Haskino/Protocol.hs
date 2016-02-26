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

module System.Hardware.Haskino.Protocol(framePackage, packageCommand, 
                                            packageProcedure, packageRemoteBinding,
                                            unpackageResponse, parseQueryResult,
                                            maxFirmwareSize) where

import Data.Bits (xor,shiftR)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)

import Control.Concurrent (modifyMVar_, readMVar)

import           Control.Remote.Monad
import           Control.Remote.Monad.Types as T

import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils

-- | Maximum size of a Haskino Firmware message
maxFirmwareSize :: Int
maxFirmwareSize = 256

-- | Minimum and maximum servo pulse widths
minServo :: Int16 
minServo = 544

maxServo :: Int16 
maxServo = 2400

framePackage :: B.ByteString -> B.ByteString
framePackage bs = B.append (B.concatMap escape bs) (B.append (escape $ check bs) (B.singleton 0x7E))
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
packageCommand :: ArduinoCommand -> Int -> Int -> (B.ByteString, Int)
packageCommand SystemReset ix ib = (buildCommand BC_CMD_SYSTEM_RESET [], ix)
packageCommand (SetPinModeE p m) ix _ =
    (buildCommand BC_CMD_SET_PIN_MODE (packageExpr p ++ [fromIntegral $ fromEnum m]), ix)
packageCommand (DigitalWriteE p b) ix _ =
    (buildCommand DIG_CMD_WRITE_PIN (packageExpr p ++ packageExpr b), ix)
packageCommand (DigitalPortWriteE p b m) ix _ =
    (buildCommand DIG_CMD_WRITE_PORT (packageExpr p ++ packageExpr b ++ packageExpr m), ix)
packageCommand (AnalogWriteE p w) ix _ =
    (buildCommand ALG_CMD_WRITE_PIN (packageExpr p ++ packageExpr w), ix)
packageCommand (ToneE p f (Just d)) ix _ =
    (buildCommand ALG_CMD_TONE_PIN (packageExpr p ++ packageExpr f ++ packageExpr d), ix)
packageCommand (ToneE p f Nothing) ix ib =
    packageCommand (ToneE p f (Just 0)) ix ib
packageCommand (NoToneE p) ix _ =
    (buildCommand ALG_CMD_NOTONE_PIN (packageExpr  p), ix)
packageCommand (I2CWrite sa w8s) ix _ = 
    (buildCommand I2C_CMD_WRITE (packageExpr sa ++ packageExpr w8s), ix)
packageCommand I2CConfig ix _ = 
    (buildCommand I2C_CMD_CONFIG [], ix)
packageCommand (StepperSetSpeedE st sp) ix _ = 
    (buildCommand STEP_CMD_SET_SPEED (packageExpr st ++ packageExpr sp), ix)
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
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e), ix)
packageCommand (ModifyRemoteRefB (RemoteRefB i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefB i))), ix)
packageCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefW8 i))), ix)
packageCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW16 i))), ix)
packageCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW32 i))), ix)
packageCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefI8 i))), ix)
packageCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI16 i))), ix)
packageCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI32 i))), ix)
packageCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefList8 i))), ix)
packageCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) ix _ =
    (buildCommand REF_CMD_WRITE ([refTypeCmdVal REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefFloat i))), ix)
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

-- The package code block takes the monad code block to package, an
-- an integer with the current remote reference index, an integer with the
-- current remote bind index, and returns a tuple of the packaged block,
-- final remote reference index, and final remote bind index.
packageCodeBlock :: Arduino a -> Int -> Int -> (B.ByteString, Int, Int)
packageCodeBlock (Arduino commands) ix ib = (cmds', ix', ib')
  where
      (_, cmds', ix', ib') = packMonad commands ix ib B.empty

      packProcedure :: ArduinoProcedure a -> Int -> Int -> B.ByteString -> (a, B.ByteString, Int, Int)
      packProcedure QueryFirmware ix ib cmds = (0, B.append cmds (lenPackage (packageProcedure QueryFirmware ib)), ix, ib)
      packProcedure QueryFirmwareE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure QueryFirmwareE ib)), ix, ib+1)
      packProcedure QueryProcessor ix ib cmds = (UNKNOWN_PROCESSOR, B.append cmds (lenPackage (packageProcedure QueryProcessor ib)), ix, ib)
      packProcedure QueryProcessorE ix ib cmds = (remBind ib, B.append cmds (lenPackage (packageProcedure QueryProcessorE ib)), ix, ib+1)
{-
      packProcedure Micros ix ib k cmds = (0, ix, ib, B.append cmds (lenPackage (packageProcedure Micros ib)))
      packProcedure MicrosE ix ib k cmds = (remBind ib, ix, ib+1, B.append cmds (lenPackage (packageProcedure MicrosE ib)))
      packProcedure Millis ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure Millis ib)))
      packProcedure MillisE ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure MillisE ib)))
      packProcedure (DelayMillis ms) ix ib k cmds = packageCodeBlock' (k ()) ix ib (B.append cmds (lenPackage (packageProcedure (DelayMillis ms) ib)))
      packProcedure (DelayMillisE ms) ix ib k cmds = packageCodeBlock' (k ()) ix ib (B.append cmds (lenPackage (packageProcedure (DelayMillisE ms) ib)))
      packProcedure (DelayMicros ms) ix ib k cmds = packageCodeBlock' (k ()) ix ib (B.append cmds (lenPackage (packageProcedure (DelayMicros ms) ib)))
      packProcedure (DelayMicrosE ms) ix ib k cmds = packageCodeBlock' (k ()) ix ib (B.append cmds (lenPackage (packageProcedure (DelayMicrosE ms) ib)))
      packProcedure (DigitalRead p) ix ib k cmds = packageCodeBlock' (k False) ix ib (B.append cmds (lenPackage (packageProcedure (DigitalRead p) ib)))
      packProcedure (DigitalReadE p) ib ix k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (DigitalReadE p) ib)))
      packProcedure (DigitalPortRead p m) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (DigitalPortRead p m) ib)))
      packProcedure (DigitalPortReadE p m) ib ix k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (DigitalPortReadE p m) ib)))
      packProcedure (AnalogRead p) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (AnalogRead p) ib)))
      packProcedure (AnalogReadE p) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (AnalogReadE p) ib)))
      packProcedure (I2CRead p n) ix ib k cmds = packageCodeBlock' (k []) ix ib (B.append cmds (lenPackage (packageProcedure (I2CRead p n) ib)))
      packProcedure (I2CReadE p n) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (I2CReadE p n) ib)))
      packProcedure (Stepper2Pin s p1 p2) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (Stepper2Pin s p1 p2) ib)))
      packProcedure (Stepper2PinE s p1 p2) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (Stepper2PinE s p1 p2) ib)))
      packProcedure (Stepper4Pin s p1 p2 p3 p4) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (Stepper4Pin s p1 p2 p3 p4) ib)))
      packProcedure (Stepper4PinE s p1 p2 p3 p4) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (Stepper4PinE s p1 p2 p3 p4) ib)))
      packProcedure (StepperStepE st s) ix ib k cmds = packageCodeBlock' (k ()) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (StepperStepE st s) ib)))
      packProcedure (ServoAttach p) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (ServoAttach p) ib)))
      packProcedure (ServoAttachE p) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ServoAttachE p) ib)))
      packProcedure (ServoAttachMinMax p min max) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (ServoAttachMinMax p min max) ib)))
      packProcedure (ServoAttachMinMaxE p min max) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ServoAttachMinMaxE p min max) ib)))
      packProcedure (ServoRead sv) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (ServoRead sv) ib)))
      packProcedure (ServoReadE sv) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ServoReadE sv) ib)))
      packProcedure (ServoReadMicros sv) ix ib k cmds = packageCodeBlock' (k 0) ix ib (B.append cmds (lenPackage (packageProcedure (ServoReadMicros sv) ib)))
      packProcedure (ServoReadMicrosE sv) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ServoReadMicrosE sv) ib)))
      packProcedure QueryAllTasks ix ib k cmds = packageCodeBlock' (k ([])) ix ib (B.append cmds (lenPackage (packageProcedure QueryAllTasks ib)))
      packProcedure QueryAllTasksE ix ib k cmds = packageCodeBlock' (k (lit [])) ix (ib+1) (B.append cmds (lenPackage (packageProcedure QueryAllTasksE ib)))
      packProcedure (QueryTask t) ix ib k cmds = packageCodeBlock' (k Nothing) ix ib (B.append cmds (lenPackage (packageProcedure (QueryTask t) ib)))
      packProcedure (QueryTaskE t) ix ib k cmds = packageCodeBlock' (k Nothing) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (QueryTaskE t) ib)))
      packProcedure (BootTaskE tid) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (BootTaskE tid) ib)))
      packProcedure (ReadRemoteRefB (RemoteRefB i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefB (RemoteRefB i)) ib)))
      packProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ib)))
      packProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ib)))
      packProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ib)))
      packProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ib)))
      packProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ib)))
      packProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ib)))
      packProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ib)))
      packProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ix ib k cmds = packageCodeBlock' (k (remBind ib)) ix (ib+1) (B.append cmds (lenPackage (packageProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ib)))
      packProcedure (NewRemoteRefB e) ix ib k cmds = packageCodeBlock' (k (RemoteRefB ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefB e) ix ib)))
      packProcedure (NewRemoteRefW8 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefW8 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW8 e) ix ib)))
      packProcedure (NewRemoteRefW16 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefW16 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW16 e) ix ib)))
      packProcedure (NewRemoteRefW32 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefW32 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefW32 e) ix ib)))
      packProcedure (NewRemoteRefI8 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefI8 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI8 e) ix ib)))
      packProcedure (NewRemoteRefI16 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefI16 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI16 e) ix ib)))
      packProcedure (NewRemoteRefI32 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefI32 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefI32 e) ix ib)))
      packProcedure (NewRemoteRefL8 e) ix ib k cmds = packageCodeBlock' (k (RemoteRefL8 ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefL8 e) ix ib)))
      packProcedure (NewRemoteRefFloat e) ix ib k cmds = packageCodeBlock' (k (RemoteRefFloat ix)) (ix+1) (ib+1) (B.append cmds (lenPackage (packageRemoteBinding (NewRemoteRefFloat e) ix ib)))
-}
      -- For sending as part of a Scheduler task, debug and die make no sense.  
      -- Instead of signalling an error, at this point they are just ignored.
      packProcedure (Debug _) ix ib cmds = ((), cmds, ix, ib)
      packProcedure (Die _ _) ix ib cmds = ((), cmds, ix, ib)

      packAppl :: RemoteApplicative ArduinoCommand ArduinoProcedure a -> Int -> Int -> B.ByteString -> (a, B.ByteString, Int, Int)
      packAppl (T.Command cmd) ix ib cmds = ((), B.append cmds pc, ix', ib)
        where 
          (pc, ix') = packageCommand cmd ix ib
      packAppl (T.Procedure p) ix ib cmds = packProcedure p ix ib cmds
      packAppl (T.Ap a1 a2) ix ib cmds = (f g, cmds'', ix'', ib'')
        where
          (f, cmds', ix', ib') = packAppl a1 ix ib cmds
          (g, cmds'', ix'', ib'') = packAppl a2 ix' ib' cmds'
      packAppl (T.Pure a) ix ib cmds = (a, cmds, ix, ib)

      packMonad :: RemoteMonad ArduinoCommand ArduinoProcedure a -> Int -> Int -> B.ByteString -> (a, B.ByteString, Int, Int)
      packMonad (T.Appl app) ix ib cmds = packAppl app ix ib cmds
      packMonad (T.Bind m k) ix ib cmds = packMonad (k r) ix' ib' cmds'
        where
          (r, cmds', ix', ib') = packMonad m ix ib cmds
      packMonad (T.Ap' m1 m2) ix ib cmds = (f g, cmds'', ix'', ib'')
        where
          (f, cmds', ix', ib') = packMonad m1 ix ib cmds
          (g, cmds'', ix'', ib'') = packMonad m2 ix' ib' cmds'

      lenPackage :: B.ByteString -> B.ByteString
      lenPackage package = B.append (lenEncode $ B.length package) package      

      -- Length of the code block is encoded with a 1 or 3 byte sequence.
      -- If the length is 0-254, the length is sent as a one byte value.
      -- If the length is greater than 255, it is sent as a zero byte,
      -- following by a 16 bit little endian length.
      -- (Zero is not a valid length, as it would be an empty command)
      lenEncode :: Int -> B.ByteString
      lenEncode l = if l < 255
                    then B.singleton $ fromIntegral l 
                    else B.pack $ 0xFF : (word16ToBytes $ fromIntegral l)

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
packageProcedure (BootTaskE tid) ib = buildCommand SCHED_CMD_BOOT_TASK ((fromIntegral ib) : (packageExpr tid))
packageProcedure (ReadRemoteRefB (RemoteRefB i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_BOOL, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_WORD8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_WORD16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_WORD32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_INT8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_INT16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_INT32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_LIST8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
packageProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) ib = buildCommand REF_CMD_READ [refTypeCmdVal REF_FLOAT, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]

packageRemoteBinding :: ArduinoProcedure a -> Int -> Int -> B.ByteString
packageRemoteBinding (NewRemoteRefB e)  ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_BOOL, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefW8 e)  ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_WORD8, fromIntegral ib, fromIntegral ix] ++(packageExpr e))
packageRemoteBinding (NewRemoteRefW16 e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_WORD16, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefW32 e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_WORD32, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefI8 e)  ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_INT8, fromIntegral ib, fromIntegral ix] ++(packageExpr e))
packageRemoteBinding (NewRemoteRefI16 e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_INT16, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefI32 e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_INT32, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefL8 e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_LIST8, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))
packageRemoteBinding (NewRemoteRefFloat e) ix ib = buildCommand REF_CMD_NEW ([refTypeCmdVal REF_FLOAT, fromIntegral ib, fromIntegral ix] ++ (packageExpr e))

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

packageMathExpr :: ExprFloatMathOp -> Expr a -> [Word8]
packageMathExpr o e = (exprFMathCmdVals o) ++ (packageExpr e)

packageTwoMathExpr :: ExprFloatMathOp -> Expr a -> Expr b -> [Word8]
packageTwoMathExpr o e1 e2 = (exprFMathCmdVals o) ++ (packageExpr e1) ++ (packageExpr e2)

packageRef :: Int -> Word8 -> [Word8]
packageRef n ec = [ec, fromIntegral n]

packageExpr :: Expr a -> [Word8]
packageExpr (LitB b) = [exprCmdVal EXPR_BOOL EXPR_LIT, if b then 1 else 0]
packageExpr (ShowB e) = packageSubExpr (exprCmdVal EXPR_BOOL EXPR_SHOW) e 
packageExpr (RefB n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (RemBindB b) = [exprCmdVal EXPR_BOOL EXPR_BIND, fromIntegral b]
packageExpr (NotB e) = packageSubExpr (exprCmdVal EXPR_BOOL EXPR_NOT) e 
packageExpr (AndB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_AND) e1 e2 
packageExpr (OrB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_OR) e1 e2 
packageExpr (EqB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_EQ) e1 e2 
packageExpr (LessB e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_BOOL EXPR_LESS) e1 e2 
packageExpr (IfB e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_BOOL EXPR_IF) e1 e2 e3
packageExpr (EqW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_EQ) e1 e2 
packageExpr (LessW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_LESS) e1 e2 
packageExpr (EqW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_EQ) e1 e2 
packageExpr (LessW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_LESS) e1 e2 
packageExpr (EqW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_EQ) e1 e2 
packageExpr (LessW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_LESS) e1 e2 
packageExpr (EqI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_EQ) e1 e2 
packageExpr (LessI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_LESS) e1 e2 
packageExpr (EqI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_EQ) e1 e2 
packageExpr (LessI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_LESS) e1 e2 
packageExpr (EqI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_EQ) e1 e2 
packageExpr (LessI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_LESS) e1 e2 
packageExpr (EqL8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_EQ) e1 e2 
packageExpr (LessL8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_LESS) e1 e2 
packageExpr (EqFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_EQ) e1 e2 
packageExpr (LessFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_LESS) e1 e2 
packageExpr (LitW8 w) = [exprCmdVal EXPR_WORD8 EXPR_LIT, w]
packageExpr (ShowW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHOW) e
packageExpr (RefW8 n) = packageRef n (exprCmdVal EXPR_WORD8 EXPR_REF)
packageExpr (RemBindW8 b) = [exprCmdVal EXPR_WORD8 EXPR_BIND, fromIntegral b]
packageExpr (FromIntW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_FINT) e
packageExpr (ToIntW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_TINT) e
packageExpr (NegW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_NEG) e
packageExpr (SignW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_SIGN) e
packageExpr (AddW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_ADD) e1 e2 
packageExpr (SubW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SUB) e1 e2 
packageExpr (MultW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_MULT) e1 e2 
packageExpr (DivW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_DIV) e1 e2 
packageExpr (RemW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_REM) e1 e2 
packageExpr (QuotW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_QUOT) e1 e2 
packageExpr (ModW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_MOD) e1 e2 
packageExpr (AndW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_AND) e1 e2 
packageExpr (OrW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_OR) e1 e2 
packageExpr (XorW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_XOR) e1 e2 
packageExpr (CompW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_COMP) e 
packageExpr (ShfLW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHFL) e1 e2 
packageExpr (ShfRW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHFR) e1 e2 
packageExpr (IfW8 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD8 EXPR_IF) e1 e2 e3
packageExpr (TestBW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_TSTB) e1 e2 
packageExpr (SetBW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_SETB) e1 e2 
packageExpr (ClrBW8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_CLRB) e1 e2 
packageExpr (LitW16 w) = (exprCmdVal EXPR_WORD16 EXPR_LIT) : word16ToBytes w
packageExpr (ShowW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHOW) e
packageExpr (RefW16 n) = packageRef n (exprCmdVal EXPR_WORD16 EXPR_REF)
packageExpr (RemBindW16 b) = [exprCmdVal EXPR_WORD16 EXPR_BIND, fromIntegral b]
packageExpr (FromIntW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_FINT) e
packageExpr (ToIntW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_TINT) e
packageExpr (NegW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_NEG) e
packageExpr (SignW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_SIGN) e
packageExpr (AddW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_ADD) e1 e2 
packageExpr (SubW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SUB) e1 e2 
packageExpr (MultW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_MULT) e1 e2 
packageExpr (DivW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_DIV) e1 e2 
packageExpr (RemW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_REM) e1 e2 
packageExpr (QuotW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_QUOT) e1 e2 
packageExpr (ModW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_MOD) e1 e2 
packageExpr (AndW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_AND) e1 e2 
packageExpr (OrW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_OR) e1 e2 
packageExpr (XorW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_XOR) e1 e2 
packageExpr (CompW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_COMP) e 
packageExpr (ShfLW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHFL) e1 e2 
packageExpr (ShfRW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHFR) e1 e2 
packageExpr (IfW16 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD16 EXPR_IF) e1 e2 e3
packageExpr (TestBW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_TSTB) e1 e2 
packageExpr (SetBW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_SETB) e1 e2 
packageExpr (ClrBW16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD16 EXPR_CLRB) e1 e2 
packageExpr (LitW32 w) = (exprCmdVal EXPR_WORD32 EXPR_LIT) : word32ToBytes w
packageExpr (ShowW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHOW) e
packageExpr (RefW32 n) = packageRef n (exprCmdVal EXPR_WORD32 EXPR_REF)
packageExpr (RemBindW32 b) = [exprCmdVal EXPR_WORD32 EXPR_BIND, fromIntegral b]
packageExpr (FromIntW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_FINT) e
packageExpr (ToIntW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_TINT) e
packageExpr (NegW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_NEG) e
packageExpr (SignW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_SIGN) e
packageExpr (AddW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_ADD) e1 e2 
packageExpr (SubW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SUB) e1 e2 
packageExpr (MultW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_MULT) e1 e2 
packageExpr (DivW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_DIV) e1 e2 
packageExpr (RemW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_REM) e1 e2 
packageExpr (QuotW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_QUOT) e1 e2 
packageExpr (ModW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_MOD) e1 e2 
packageExpr (AndW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_AND) e1 e2 
packageExpr (OrW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_OR) e1 e2 
packageExpr (XorW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_XOR) e1 e2 
packageExpr (CompW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_COMP) e
packageExpr (ShfLW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHFL) e1 e2 
packageExpr (ShfRW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHFR) e1 e2 
packageExpr (IfW32 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD32 EXPR_IF) e1 e2 e3
packageExpr (TestBW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_TSTB) e1 e2 
packageExpr (SetBW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_SETB) e1 e2 
packageExpr (ClrBW32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD32 EXPR_CLRB) e1 e2 
packageExpr (LitI8 w) = [exprCmdVal EXPR_INT8 EXPR_LIT, fromIntegral w]
packageExpr (ShowI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_SHOW) e
packageExpr (RefI8 n) = packageRef n (exprCmdVal EXPR_INT8 EXPR_REF)
packageExpr (RemBindI8 b) = [exprCmdVal EXPR_INT8 EXPR_BIND, fromIntegral b]
packageExpr (FromIntI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_FINT) e
packageExpr (ToIntI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_TINT) e
packageExpr (NegI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_NEG) e
packageExpr (SignI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_SIGN) e
packageExpr (AddI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_ADD) e1 e2 
packageExpr (SubI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_SUB) e1 e2 
packageExpr (MultI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_MULT) e1 e2 
packageExpr (DivI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_DIV) e1 e2 
packageExpr (RemI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_REM) e1 e2 
packageExpr (QuotI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_QUOT) e1 e2 
packageExpr (ModI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_MOD) e1 e2 
packageExpr (AndI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_AND) e1 e2 
packageExpr (OrI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_OR) e1 e2 
packageExpr (XorI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_XOR) e1 e2 
packageExpr (CompI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_COMP) e 
packageExpr (ShfLI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_SHFL) e1 e2 
packageExpr (ShfRI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_SHFR) e1 e2 
packageExpr (IfI8 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_INT8 EXPR_IF) e1 e2 e3
packageExpr (TestBI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_TSTB) e1 e2 
packageExpr (SetBI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_SETB) e1 e2 
packageExpr (ClrBI8 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT8 EXPR_CLRB) e1 e2 
packageExpr (LitI16 w) = (exprCmdVal EXPR_INT16 EXPR_LIT) : word16ToBytes (fromIntegral w)
packageExpr (ShowI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_SHOW) e
packageExpr (RefI16 n) = packageRef n (exprCmdVal EXPR_INT16 EXPR_REF)
packageExpr (RemBindI16 b) = [exprCmdVal EXPR_INT16 EXPR_BIND, fromIntegral b]
packageExpr (FromIntI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_FINT) e
packageExpr (ToIntI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_TINT) e
packageExpr (NegI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_NEG) e
packageExpr (SignI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_SIGN) e
packageExpr (AddI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_ADD) e1 e2 
packageExpr (SubI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_SUB) e1 e2 
packageExpr (MultI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_MULT) e1 e2 
packageExpr (DivI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_DIV) e1 e2 
packageExpr (RemI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_REM) e1 e2 
packageExpr (QuotI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_QUOT) e1 e2 
packageExpr (ModI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_MOD) e1 e2 
packageExpr (AndI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_AND) e1 e2 
packageExpr (OrI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_OR) e1 e2 
packageExpr (XorI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_XOR) e1 e2 
packageExpr (CompI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_COMP) e 
packageExpr (ShfLI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_SHFL) e1 e2 
packageExpr (ShfRI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_SHFR) e1 e2 
packageExpr (IfI16 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_INT16 EXPR_IF) e1 e2 e3
packageExpr (TestBI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_TSTB) e1 e2 
packageExpr (SetBI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_SETB) e1 e2 
packageExpr (ClrBI16 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT16 EXPR_CLRB) e1 e2 
packageExpr (LitI32 w) = (exprCmdVal EXPR_INT32 EXPR_LIT) : word32ToBytes (fromIntegral w)
packageExpr (ShowI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_SHOW) e
packageExpr (RefI32 n) = packageRef n (exprCmdVal EXPR_INT32 EXPR_REF)
packageExpr (RemBindI32 b) = [exprCmdVal EXPR_INT32 EXPR_BIND, fromIntegral b]
packageExpr (NegI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_NEG) e
packageExpr (SignI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_SIGN) e
packageExpr (AddI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_ADD) e1 e2 
packageExpr (SubI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SUB) e1 e2 
packageExpr (MultI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_MULT) e1 e2 
packageExpr (DivI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_DIV) e1 e2 
packageExpr (RemI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_REM) e1 e2 
packageExpr (QuotI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_QUOT) e1 e2 
packageExpr (ModI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_MOD) e1 e2 
packageExpr (AndI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_AND) e1 e2 
packageExpr (OrI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_OR) e1 e2 
packageExpr (XorI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_XOR) e1 e2 
packageExpr (CompI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_COMP) e
packageExpr (ShfLI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SHFL) e1 e2 
packageExpr (ShfRI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SHFR) e1 e2 
packageExpr (IfI32 e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_INT32 EXPR_IF) e1 e2 e3
packageExpr (TestBI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_TSTB) e1 e2 
packageExpr (SetBI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SETB) e1 e2 
packageExpr (ClrBI32 e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_CLRB) e1 e2 
packageExpr (LitList8 ws) = [exprLCmdVal EXPRL_LIT, fromIntegral $ length ws] ++ ws
packageExpr (RefList8 n) = packageRef n (exprLCmdVal EXPRL_REF)
packageExpr (RemBindList8 b) = [exprLCmdVal EXPRL_BIND, fromIntegral b]
packageExpr (IfL8 e1 e2 e3) = packageIfBSubExpr (exprLCmdVal EXPRL_IF) e1 e2 e3
packageExpr (ElemList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_ELEM) e1 e2 
packageExpr (LenList8 e) = packageSubExpr (exprLCmdVal EXPRL_LEN) e
packageExpr (ConsList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_CONS) e1 e2 
packageExpr (ApndList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_APND) e1 e2
packageExpr (PackList8 es) = [exprLCmdVal EXPRL_PACK, fromIntegral $ length es] ++ (foldl (++) [] (map packageExpr es))
packageExpr (LitFloat f) = (exprFCmdVal EXPRF_LIT) : floatToBytes f
packageExpr (ShowFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_SHOW) e1 e2
packageExpr (RefFloat n) = packageRef n (exprFCmdVal EXPRF_REF)
packageExpr (RemBindFloat b) = [exprFCmdVal EXPRF_BIND, fromIntegral b]
packageExpr (FromIntFloat e) = packageSubExpr (exprFCmdVal EXPRF_FINT) e
packageExpr (NegFloat e) = packageSubExpr (exprFCmdVal EXPRF_NEG) e
packageExpr (SignFloat e) = packageSubExpr (exprFCmdVal EXPRF_SIGN) e
packageExpr (AddFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_ADD) e1 e2 
packageExpr (SubFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_SUB) e1 e2 
packageExpr (MultFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_MULT) e1 e2 
packageExpr (DivFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_DIV) e1 e2 
packageExpr (IfFloat e1 e2 e3) = packageIfBSubExpr (exprFCmdVal EXPRF_IF) e1 e2 e3
packageExpr (TruncFloat e) = packageMathExpr EXPRF_TRUNC e 
packageExpr (FracFloat e) = packageMathExpr EXPRF_FRAC e 
packageExpr (RoundFloat e) = packageMathExpr EXPRF_ROUND e 
packageExpr (CeilFloat e) = packageMathExpr EXPRF_CEIL e 
packageExpr (FloorFloat e) = packageMathExpr EXPRF_FLOOR e 
packageExpr PiFloat = exprFMathCmdVals EXPRF_PI
packageExpr (ExpFloat e) = packageMathExpr EXPRF_EXP e 
packageExpr (LogFloat e) = packageMathExpr EXPRF_LOG e 
packageExpr (SqrtFloat e) = packageMathExpr EXPRF_SQRT e 
packageExpr (SinFloat e) = packageMathExpr EXPRF_SIN e 
packageExpr (CosFloat e) = packageMathExpr EXPRF_COS e 
packageExpr (TanFloat e) = packageMathExpr EXPRF_TAN e 
packageExpr (AsinFloat e) = packageMathExpr EXPRF_ASIN e 
packageExpr (AcosFloat e) = packageMathExpr EXPRF_ACOS e 
packageExpr (AtanFloat e) = packageMathExpr EXPRF_ATAN e 
packageExpr (Atan2Float e1 e2) = packageTwoMathExpr EXPRF_ATAN2 e1 e2 
packageExpr (SinhFloat e) = packageMathExpr EXPRF_SINH e 
packageExpr (CoshFloat e) = packageMathExpr EXPRF_COSH e 
packageExpr (TanhFloat e) = packageMathExpr EXPRF_TANH e 
packageExpr (PowerFloat e1 e2) = packageTwoMathExpr EXPRF_POWER e1 e2 
packageExpr (IsNaNFloat e) = packageMathExpr EXPRF_ISNAN e 
packageExpr (IsInfFloat e) = packageMathExpr EXPRF_ISINF e 

-- | Unpackage a Haskino Firmware response
unpackageResponse :: [Word8] -> Response
unpackageResponse [] = Unimplemented (Just "<EMPTY-REPLY>") []
unpackageResponse (cmdWord:args)
  | Right cmd <- getFirmwareReply cmdWord
  = case (cmd, args) of
      (BC_RESP_DELAY, [])               -> DelayResp
      (BS_RESP_VERSION, [majV, minV])   -> Firmware (bytesToWord16 (majV,minV))
      (BS_RESP_TYPE, [p])               -> ProcessorType p
      (BS_RESP_MICROS, [l,m0,m1,m2,m3]) -> MicrosReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_MILLIS, [l,m0,m1,m2,m3]) -> MillisReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_STRING, rest)            -> StringMessage (getString rest)
      (DIG_RESP_READ_PIN, [l,b])        -> DigitalReply b
      (DIG_RESP_READ_PORT, [l,b])       -> DigitalPortReply b
      (ALG_RESP_READ_PIN, [l,bl,bh])    -> AnalogReply (bytesToWord16 (bl,bh))
      (I2C_RESP_READ, _:_:xs)           -> I2CReply xs
      (STEP_RESP_2PIN, [l,st])          -> Stepper2PinReply st
      (STEP_RESP_4PIN, [l,st])          -> Stepper4PinReply st
      (STEP_RESP_STEP, [])              -> StepperStepReply
      (SRVO_RESP_ATTACH, [l,sv])        -> ServoAttachReply sv
      (SRVO_RESP_READ, [l,il,ih])       -> ServoReadReply (fromIntegral (bytesToWord16 (il,ih)))
      (SRVO_RESP_READ_MICROS, [l,il,ih]) -> ServoReadMicrosReply (fromIntegral (bytesToWord16 (il,ih)))
      (SCHED_RESP_BOOT, [l,b])          -> BootTaskResp b
      (SCHED_RESP_QUERY_ALL, _:_:ts)    -> QueryAllTasksReply ts
      (SCHED_RESP_QUERY, ts) | length ts == 0 -> 
          QueryTaskReply Nothing
      (SCHED_RESP_QUERY, ts) | length ts >= 9 -> 
          let ts0:ts1:tl0:tl1:tp0:tp1:tt0:tt1:tt2:tt3:rest = ts
          in QueryTaskReply (Just (bytesToWord16 (ts0,ts1), 
                                   bytesToWord16 (tl0,tl1),
                                   bytesToWord16 (tp0,tp1), 
                                   bytesToWord32 (tt0,tt1,tt2,tt3)))  
      (REF_RESP_READ , [l,b]) | l == exprCmdVal EXPR_BOOL EXPR_LIT
                                      -> ReadRefBReply (if b == 0 then False else True)
      (REF_RESP_READ , [l,b]) | l == exprCmdVal EXPR_WORD8 EXPR_LIT
                                      -> ReadRefW8Reply b
      (REF_RESP_READ , [l,b1,b2]) | l == exprCmdVal EXPR_WORD16 EXPR_LIT
                                      -> ReadRefW16Reply (bytesToWord16 (b1, b2))
      (REF_RESP_READ , [l,b1,b2,b3,b4]) | l == exprCmdVal EXPR_WORD32 EXPR_LIT
                                      -> ReadRefW32Reply (bytesToWord32 (b1, b2, b3, b4))
      (REF_RESP_READ , [l,b]) | l == exprCmdVal EXPR_INT8 EXPR_LIT
                                      -> ReadRefI8Reply $ fromIntegral b
      (REF_RESP_READ , [l,b1,b2]) | l == exprCmdVal EXPR_INT16 EXPR_LIT
                                      -> ReadRefI16Reply $ fromIntegral (bytesToWord16 (b1, b2))
      (REF_RESP_READ , [l,b1,b2,b3,b4]) | l == exprCmdVal EXPR_INT32 EXPR_LIT
                                      -> ReadRefI32Reply $ fromIntegral (bytesToWord32 (b1, b2, b3, b4))
      (REF_RESP_READ , l:_:bs) | l == exprLCmdVal EXPRL_LIT
                                      -> ReadRefL8Reply bs
      (REF_RESP_READ , [l,b1,b2,b3,b4]) | l == exprFCmdVal EXPRF_LIT
                                      -> ReadRefFloatReply $ bytesToFloat (b1, b2, b3, b4)
      (REF_RESP_NEW , [l,w])          -> NewReply w
      (REF_RESP_NEW , [])             -> FailedNewRef
      _                               -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
parseQueryResult :: ArduinoProcedure a -> Response -> Maybe a
parseQueryResult QueryFirmware (Firmware v) = Just v
parseQueryResult QueryFirmwareE (Firmware v) = Just (lit v)
parseQueryResult QueryProcessor (ProcessorType pt) = Just $ toEnum $ fromIntegral pt
parseQueryResult QueryProcessorE (ProcessorType pt) = Just $ (lit pt)
parseQueryResult Micros (MicrosReply m) = Just m
parseQueryResult MicrosE (MicrosReply m) = Just (lit m)
parseQueryResult Millis (MillisReply m) = Just m
parseQueryResult MillisE (MillisReply m) = Just (lit m)
parseQueryResult (DelayMicros _) DelayResp = Just ()
parseQueryResult (DelayMicrosE _) DelayResp = Just ()
parseQueryResult (DelayMillis _) DelayResp = Just ()
parseQueryResult (DelayMillisE _) DelayResp = Just ()
parseQueryResult (DigitalRead _) (DigitalReply d) = Just (if d == 0 then False else True)
parseQueryResult (DigitalReadE _) (DigitalReply d) = Just (if d == 0 then lit False else lit True)
parseQueryResult (DigitalPortRead _ _) (DigitalPortReply d) = Just d
parseQueryResult (DigitalPortReadE _ _) (DigitalPortReply d) = Just (lit d)
parseQueryResult (AnalogRead _) (AnalogReply a) = Just a
parseQueryResult (AnalogReadE _) (AnalogReply a) = Just (lit a)
parseQueryResult (I2CRead _ _) (I2CReply ds) = Just ds
parseQueryResult (I2CReadE _ _) (I2CReply ds) = Just (lit ds)
parseQueryResult (Stepper2Pin _ _ _) (Stepper2PinReply st) = Just st
parseQueryResult (Stepper2PinE _ _ _) (Stepper2PinReply st) = Just (lit st)
parseQueryResult (Stepper4Pin _ _ _ _ _) (Stepper4PinReply st) = Just st
parseQueryResult (Stepper4PinE _ _ _ _ _) (Stepper4PinReply st) = Just (lit st)
parseQueryResult (StepperStepE _ _) StepperStepReply = Just ()
parseQueryResult QueryAllTasks (QueryAllTasksReply ts) = Just ts
parseQueryResult QueryAllTasksE (QueryAllTasksReply ts) = Just (lit ts)
parseQueryResult (QueryTask _) (QueryTaskReply tr) = Just tr
parseQueryResult (QueryTaskE _) (QueryTaskReply tr) = Just tr
parseQueryResult (BootTaskE _) (BootTaskResp b) = Just (if b == 0 then lit False else lit True)
parseQueryResult (NewRemoteRefB _) (NewReply r) = Just $ RemoteRefB $ fromIntegral r
parseQueryResult (NewRemoteRefW8 _) (NewReply r) = Just $ RemoteRefW8 $ fromIntegral r
parseQueryResult (NewRemoteRefW16 _) (NewReply r) = Just $ RemoteRefW16 $ fromIntegral r
parseQueryResult (NewRemoteRefW32 _) (NewReply r) = Just $ RemoteRefW32 $ fromIntegral r
parseQueryResult (NewRemoteRefI8 _) (NewReply r) = Just $ RemoteRefI8 $ fromIntegral r
parseQueryResult (NewRemoteRefI16 _) (NewReply r) = Just $ RemoteRefI16 $ fromIntegral r
parseQueryResult (NewRemoteRefI32 _) (NewReply r) = Just $ RemoteRefI32 $ fromIntegral r
parseQueryResult (NewRemoteRefL8 _) (NewReply r) = Just $ RemoteRefL8 $ fromIntegral r
parseQueryResult (NewRemoteRefFloat _) (NewReply r) = Just $ RemoteRefFloat$ fromIntegral r
parseQueryResult (ReadRemoteRefB _) (ReadRefBReply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW8 _) (ReadRefW8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW16 _) (ReadRefW16Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW32 _) (ReadRefW32Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI8 _) (ReadRefI8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI16 _) (ReadRefI16Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI32 _) (ReadRefI32Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefL8 _) (ReadRefL8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefFloat _) (ReadRefFloatReply r) = Just $ lit r
parseQueryResult q r = Nothing
