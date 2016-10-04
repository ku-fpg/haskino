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
                                            maxFirmwareSize, packageExpr,
                                            CommandState(..) ) where

import Data.Bits (xor,shiftR)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)

import Control.Concurrent (modifyMVar_, readMVar)

import Control.Monad.State

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

data CommandState = CommandState {ix :: Int  
                                , ib :: Int
                                , block :: B.ByteString    
                                , blocks :: [B.ByteString]}    

framePackage :: B.ByteString -> B.ByteString
framePackage bs = B.append (B.concatMap escape bs) (B.append (escape $ check bs) (B.singleton 0x7E))
  where
    escape :: Word8 -> B.ByteString
    escape c = if c == 0x7E || c == 0x7D
               then B.pack $ [0x7D, xor c 0x20]
               else B.singleton c
    check b = B.foldl (+) 0 b

addCommand :: FirmwareCmd -> [Word8] -> State CommandState B.ByteString
addCommand cmd bs = return $ buildCommand cmd bs

buildCommand :: FirmwareCmd -> [Word8] -> B.ByteString
buildCommand cmd bs = B.pack $ firmwareCmdVal cmd : bs

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Haskino Firmware protocol.
packageCommand :: ArduinoCommand -> State CommandState B.ByteString
packageCommand SystemReset =
    addCommand BC_CMD_SYSTEM_RESET []
packageCommand (SetPinModeE p m) = 
    addCommand BC_CMD_SET_PIN_MODE (packageExpr p ++ packageExpr m)
packageCommand (DigitalWriteE p b) =
    addCommand DIG_CMD_WRITE_PIN (packageExpr p ++ packageExpr b)
packageCommand (DigitalPortWriteE p b m) =
    addCommand DIG_CMD_WRITE_PORT (packageExpr p ++ packageExpr b ++ packageExpr m)
packageCommand (AnalogWriteE p w) =
    addCommand ALG_CMD_WRITE_PIN (packageExpr p ++ packageExpr w)
packageCommand (ToneE p f (Just d)) =
    addCommand ALG_CMD_TONE_PIN (packageExpr p ++ packageExpr f ++ packageExpr d)
packageCommand (ToneE p f Nothing) =
    packageCommand (ToneE p f (Just 0))
packageCommand (NoToneE p) =
    addCommand ALG_CMD_NOTONE_PIN (packageExpr  p)
packageCommand (I2CWrite sa w8s) = 
    addCommand I2C_CMD_WRITE (packageExpr sa ++ packageExpr w8s)
packageCommand I2CConfig = 
    addCommand I2C_CMD_CONFIG []
packageCommand (StepperSetSpeedE st sp) = 
    addCommand STEP_CMD_SET_SPEED (packageExpr st ++ packageExpr sp)
packageCommand (ServoDetachE sv) = 
    addCommand SRVO_CMD_DETACH (packageExpr sv)
packageCommand (ServoWriteE sv w) = 
    addCommand SRVO_CMD_WRITE (packageExpr sv ++ packageExpr w)
packageCommand (ServoWriteMicrosE sv w) = 
    addCommand SRVO_CMD_WRITE_MICROS (packageExpr sv ++ packageExpr w)
packageCommand (DeleteTaskE tid) =
    addCommand SCHED_CMD_DELETE_TASK (packageExpr tid)
packageCommand (ScheduleTaskE tid tt) =
    addCommand SCHED_CMD_SCHED_TASK (packageExpr tid ++ packageExpr tt)
packageCommand ScheduleReset =
    addCommand SCHED_CMD_RESET []
packageCommand (AttachIntE p t m) =
    addCommand SCHED_CMD_ATTACH_INT (packageExpr p ++ packageExpr t ++ packageExpr m)
packageCommand (DetachIntE p) =
    addCommand SCHED_CMD_DETACH_INT (packageExpr p)
packageCommand (Interrupts) =
    addCommand SCHED_CMD_INTERRUPTS []
packageCommand (NoInterrupts) =
    addCommand SCHED_CMD_NOINTERRUPTS []
packageCommand (GiveSemE id) =
    addCommand SCHED_CMD_GIVE_SEM (packageExpr id)
packageCommand (TakeSemE id) =
    addCommand SCHED_CMD_TAKE_SEM (packageExpr id)
packageCommand (CreateTaskE tid m) = do
    td <- packageCodeBlock m
    s <- get
    let taskSize = fromIntegral (B.length td)
    cmd <- addCommand SCHED_CMD_CREATE_TASK ((packageExpr tid) ++ (packageExpr (LitW16 taskSize)) ++ (packageExpr (LitW16 (fromIntegral (ib s)))))                                   
    return $ (framePackage cmd) `B.append` (genAddToTaskCmds td)
  where
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
packageCommand (WriteRemoteRefB (RemoteRefB i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefW8 (RemoteRefW8 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefW16 (RemoteRefW16 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefW32 (RemoteRefW32 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefI8 (RemoteRefI8 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefI16 (RemoteRefI16 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefI32 (RemoteRefI32 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefL8 (RemoteRefL8 i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (WriteRemoteRefFloat (RemoteRefFloat i) e) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr e)
packageCommand (ModifyRemoteRefB (RemoteRefB i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_BOOL, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefB i)))
packageCommand (ModifyRemoteRefW8 (RemoteRefW8 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefW8 i)))
packageCommand (ModifyRemoteRefW16 (RemoteRefW16 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW16 i)))
packageCommand (ModifyRemoteRefW32 (RemoteRefW32 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_WORD32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefW32 i)))
packageCommand (ModifyRemoteRefI8 (RemoteRefI8 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT8, exprCmdVal EXPR_WORD8 EXPR_LIT,fromIntegral i] ++ packageExpr (f (RefI8 i)))
packageCommand (ModifyRemoteRefI16 (RemoteRefI16 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT16, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI16 i)))
packageCommand (ModifyRemoteRefI32 (RemoteRefI32 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_INT32, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefI32 i)))
packageCommand (ModifyRemoteRefL8 (RemoteRefL8 i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_LIST8, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefList8 i)))
packageCommand (ModifyRemoteRefFloat (RemoteRefFloat i) f) =
    addCommand REF_CMD_WRITE ([fromIntegral $ fromEnum REF_FLOAT, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (f (RefFloat i)))
packageCommand (WhileRemoteRefB (RemoteRefB i) bf uf cb) =
    packageWhileCommand (RefB i) i bf uf cb
packageCommand (WhileRemoteRefW8 (RemoteRefW8 i) bf uf cb) =
    packageWhileCommand (RefW8 i) i bf uf cb
packageCommand (WhileRemoteRefW16 (RemoteRefW16 i) bf uf cb) =
    packageWhileCommand (RefW16 i) i bf uf cb
packageCommand (WhileRemoteRefW32 (RemoteRefW32 i) bf uf cb) =
    packageWhileCommand (RefW32 i) i bf uf cb
packageCommand (WhileRemoteRefI8 (RemoteRefI8 i) bf uf cb) =
    packageWhileCommand (RefI8 i) i bf uf cb
packageCommand (WhileRemoteRefI16 (RemoteRefI16 i) bf uf cb) =
    packageWhileCommand (RefI16 i) i bf uf cb
packageCommand (WhileRemoteRefI32 (RemoteRefI32 i) bf uf cb) =
    packageWhileCommand (RefI32 i) i bf uf cb
packageCommand (WhileRemoteRefFloat (RemoteRefFloat i) bf uf cb) =
    packageWhileCommand (RefFloat i) i bf uf cb
packageCommand (WhileRemoteRefL8 (RemoteRefL8 i) bf uf cb) = 
    packageWhileCommand (RefList8 i) i bf uf cb
packageCommand (LoopE cb) = do
    p <- packageCodeBlock cb
    l <- addCommand BC_CMD_LOOP (B.unpack p)
    return l
packageCommand (ForInE ws f) = do
    s <- get
    p <- packageCodeBlock $ f $ RemBindW8 $ ib s
    fc <- addCommand BC_CMD_FORIN ((packageExpr ws) ++ (packageExpr (RemBindW8 (ib s))))
    put s {ib = (ib s) + 1}
    return $ B.append fc p
packageCommand (IfThenElse e cb1 cb2) = do
    pc1 <- packageCodeBlock cb1
    pc2 <- packageCodeBlock cb2
    let thenSize = word16ToBytes $ fromIntegral (B.length pc1)
    i <- addCommand BC_CMD_IF_THEN_ELSE (thenSize ++ (packageExpr e))
    return $ B.append i (B.append pc1 pc2)

packageWhileCommand :: Expr a -> Int -> (Expr a -> Expr Bool) -> (Expr a -> Expr a) -> Arduino () -> State CommandState B.ByteString
packageWhileCommand rr i bf uf cb = do
    w <- addCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf rr) ++ [fromIntegral $ length ufe] ++ ufe)
    p <- packageCodeBlock cb
    return $ B.append w p
  where
    ufe = packageExpr $ uf rr

{-
packageCommand (WhileRemoteRefB (RemoteRefB i) bf uf cb) = do
    w <- addCommand BC_CMD_WHILE ([exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i] ++ packageExpr (bf (RefB i)) ++ [fromIntegral $ length ufe] ++ ufe)
    p <- packageCodeBlock cb
    return $ B.append w p
  where
    ufe = packageExpr (uf (RefB i))
-}

-- The package code block takes the monad code block to package, an
-- an integer with the current remote reference index, an integer with the
-- current remote bind index, and returns a tuple of the packaged block,
-- final remote reference index, and final remote bind index.
packageCodeBlock :: Arduino a -> State CommandState B.ByteString
packageCodeBlock (Arduino commands) = do
    startNewBlock 
    packMonad commands
    endCurrentBlock
  where
      startNewBlock :: State CommandState ()
      startNewBlock = do
          s <- get
          put s {block = B.empty, blocks = (block s) : (blocks s)}

      endCurrentBlock :: State CommandState B.ByteString
      endCurrentBlock = do
          s <- get
          put s {block = head $ blocks s, blocks = tail $ blocks s}
          return $ block s

      addToBlock :: B.ByteString -> State CommandState ()
      addToBlock bs = do
          s <- get
          put s {block = B.append (block s) bs}

      packShallowProcedure :: ArduinoProcedure a -> a -> State CommandState a
      packShallowProcedure p r = do
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          return r

      packDeepProcedure :: ArduinoProcedure a -> State CommandState Int
      packDeepProcedure p = do
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          s <- get
          put s {ib = (ib s) + 1}
          return $ ib s

      packNewRef :: ArduinoProcedure a -> a -> State CommandState a
      packNewRef p r = do
          prb <- packageRemoteBinding p
          addToBlock $ lenPackage prb
          s <- get
          put s {ib = (ib s) + 1, ix = (ix s) + 1}
          return r

      packProcedure :: ArduinoProcedure a -> State CommandState a
      packProcedure QueryFirmware = packShallowProcedure QueryFirmware 0
      packProcedure QueryFirmwareE = do
          i <- packDeepProcedure QueryFirmwareE 
          return $ RemBindW16 i
      packProcedure QueryProcessor = packShallowProcedure QueryProcessor UNKNOWN_PROCESSOR
      packProcedure QueryProcessorE = do
          i <- packDeepProcedure QueryProcessorE
          return $ RemBindW8 i
      packProcedure Micros = packShallowProcedure Micros 0
      packProcedure MicrosE = do
          i <- packDeepProcedure MicrosE
          return $ RemBindW32 i
      packProcedure Millis = packShallowProcedure Millis 0
      packProcedure MillisE = do
          i <- packDeepProcedure MillisE
          return $ RemBindW32 i
      packProcedure (DelayMillis ms) = packShallowProcedure (DelayMillis ms) ()
      packProcedure (DelayMillisE ms) = packShallowProcedure (DelayMillisE ms) ()
      packProcedure (DelayMicros ms) = packShallowProcedure (DelayMicros ms) ()
      packProcedure (DelayMicrosE ms) = packShallowProcedure (DelayMicrosE ms) ()
      packProcedure (DigitalRead p) = packShallowProcedure (DigitalRead p) False
      packProcedure (DigitalReadE p) = do
          i <- packDeepProcedure (DigitalReadE p)
          return $ RemBindB i
      packProcedure (DigitalPortRead p m) = packShallowProcedure (DigitalPortRead p m) 0
      packProcedure (DigitalPortReadE p m) = do
          i <- packDeepProcedure (DigitalPortReadE p m)
          return $ RemBindW8 i
      packProcedure (AnalogRead p) = packShallowProcedure (AnalogRead p) 0
      packProcedure (AnalogReadE p) = do
          i <- packDeepProcedure (AnalogReadE p)
          return $ RemBindW16 i
      packProcedure (I2CRead p n) = packShallowProcedure (I2CRead p n) []
      packProcedure (I2CReadE p n) = do
          i <- packDeepProcedure (I2CReadE p n)
          return $ RemBindList8 i
      packProcedure (Stepper2Pin s p1 p2) = packShallowProcedure (Stepper2Pin s p1 p2) 0
      packProcedure (Stepper2PinE s p1 p2) = do
          i <- packDeepProcedure (Stepper2PinE s p1 p2)
          return $ RemBindW8 i
      packProcedure (Stepper4Pin s p1 p2 p3 p4) = packShallowProcedure (Stepper4Pin s p1 p2 p3 p4) 0
      packProcedure (Stepper4PinE s p1 p2 p3 p4) = do
          i <- packDeepProcedure (Stepper4PinE s p1 p2 p3 p4)
          return $ RemBindW8 i
      packProcedure (StepperStepE st s) = packShallowProcedure (StepperStepE st s) ()
      packProcedure (ServoAttach p) = packShallowProcedure (ServoAttach p) 0
      packProcedure (ServoAttachE p) = do
          i <- packDeepProcedure (ServoAttachE p)
          return $ RemBindW8 i
      packProcedure (ServoAttachMinMax p min max) = packShallowProcedure (ServoAttachMinMax p min max) 0
      packProcedure (ServoAttachMinMaxE p min max) = do
          i <- packDeepProcedure (ServoAttachMinMaxE p min max)
          return $ RemBindW8 i
      packProcedure (ServoRead sv) = packShallowProcedure (ServoRead sv) 0
      packProcedure (ServoReadE sv) = do
          i <- packDeepProcedure (ServoReadE sv)
          return $ RemBindI16 i
      packProcedure (ServoReadMicros sv) = packShallowProcedure (ServoReadMicros sv) 0
      packProcedure (ServoReadMicrosE sv) = do
          i <- packDeepProcedure (ServoReadMicrosE sv)
          return $ RemBindI16 i
      packProcedure QueryAllTasks = packShallowProcedure QueryAllTasks []
      packProcedure QueryAllTasksE = do
          i <- packDeepProcedure QueryAllTasksE
          return $ RemBindList8 i
      packProcedure (QueryTask t) = packShallowProcedure (QueryTask t) Nothing
      packProcedure (QueryTaskE t) = packShallowProcedure (QueryTaskE t) Nothing
      packProcedure (BootTaskE tids) = do
          i <- packDeepProcedure (BootTaskE tids)
          return $ RemBindB i
      packProcedure (ReadRemoteRefB (RemoteRefB i)) = do
          i <- packDeepProcedure (ReadRemoteRefB (RemoteRefB i))
          return $ RemBindB i
      packProcedure (ReadRemoteRefW8 (RemoteRefW8 i)) = do
          i <- packDeepProcedure (ReadRemoteRefW8 (RemoteRefW8 i))
          return $ RemBindW8 i
      packProcedure (ReadRemoteRefW16 (RemoteRefW16 i)) = do
          i <- packDeepProcedure (ReadRemoteRefW16 (RemoteRefW16 i))
          return $ RemBindW16 i
      packProcedure (ReadRemoteRefW32 (RemoteRefW32 i)) = do
          i <- packDeepProcedure (ReadRemoteRefW32 (RemoteRefW32 i))
          return $ RemBindW32 i
      packProcedure (ReadRemoteRefI8 (RemoteRefI8 i)) = do
          i <- packDeepProcedure (ReadRemoteRefI8 (RemoteRefI8 i))
          return $ RemBindI8 i
      packProcedure (ReadRemoteRefI16 (RemoteRefI16 i)) = do
          i <- packDeepProcedure (ReadRemoteRefI16 (RemoteRefI16 i))
          return $ RemBindI16 i
      packProcedure (ReadRemoteRefI32 (RemoteRefI32 i)) = do
          i <- packDeepProcedure (ReadRemoteRefI32 (RemoteRefI32 i))
          return $ RemBindI32 i
      packProcedure (ReadRemoteRefL8 (RemoteRefL8 i)) = do
          i <- packDeepProcedure (ReadRemoteRefL8 (RemoteRefL8 i))
          return $ RemBindList8 i
      packProcedure (ReadRemoteRefFloat (RemoteRefFloat i)) = do
          i <- packDeepProcedure (ReadRemoteRefFloat (RemoteRefFloat i))
          return $ RemBindFloat i
      packProcedure (NewRemoteRefB e) = do
          s <- get
          packNewRef (NewRemoteRefB e) (RemoteRefB (ix s))
      packProcedure (NewRemoteRefW8 e) = do
          s <- get
          packNewRef (NewRemoteRefW8 e) (RemoteRefW8 (ix s))
      packProcedure (NewRemoteRefW16 e) = do
          s <- get
          packNewRef (NewRemoteRefW16 e) (RemoteRefW16 (ix s))
      packProcedure (NewRemoteRefW32 e) = do
          s <- get
          packNewRef (NewRemoteRefW32 e) (RemoteRefW32 (ix s))
      packProcedure (NewRemoteRefI8 e) = do
          s <- get
          packNewRef (NewRemoteRefI8 e) (RemoteRefI8 (ix s))
      packProcedure (NewRemoteRefI16 e) = do
          s <- get
          packNewRef (NewRemoteRefI16 e) (RemoteRefI16 (ix s))
      packProcedure (NewRemoteRefI32 e) = do
          s <- get
          packNewRef (NewRemoteRefI32 e) (RemoteRefI32 (ix s))
      packProcedure (NewRemoteRefL8 e) = do
          s <- get
          packNewRef (NewRemoteRefL8 e) (RemoteRefL8 (ix s))
      packProcedure (NewRemoteRefFloat e) = do
          s <- get
          packNewRef (NewRemoteRefFloat e) (RemoteRefFloat (ix s))
      packProcedure (DebugE tids) = packShallowProcedure (DebugE tids) ()
      -- For sending as part of a Scheduler task, debug and die make no sense.  
      -- Instead of signalling an error, at this point they are just ignored.
      packProcedure (Debug _) = return ()
      packProcedure DebugListen = return ()
      packProcedure (Die _ _) = return ()

      packAppl :: RemoteApplicative ArduinoCommand ArduinoProcedure a -> State CommandState a
      packAppl (T.Command cmd) = do
          pc <- packageCommand cmd
          addToBlock $ lenPackage pc
          return ()
      packAppl (T.Procedure p) = packProcedure p
      packAppl (T.Ap a1 a2) = do
          f <- packAppl a1
          g <- packAppl a2
          return $ f g
      packAppl (T.Pure a) = return a

      packMonad :: RemoteMonad ArduinoCommand ArduinoProcedure a -> State CommandState a
      packMonad (T.Appl app) = packAppl app
      packMonad (T.Bind m k) = do
          r <- packMonad m
          packMonad (k r)
      packMonad (T.Ap' m1 m2) = do
          f <- packMonad m1
          g <- packMonad m2
          return $ f g

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

packageProcedure :: ArduinoProcedure a -> State CommandState B.ByteString
packageProcedure p = do
    s <- get
    packageProcedure' p (ib s)
  where
    packageProcedure' :: ArduinoProcedure a -> Int -> State CommandState B.ByteString
    packageProcedure' QueryFirmware ib    = addCommand BS_CMD_REQUEST_VERSION [fromIntegral ib]
    packageProcedure' QueryFirmwareE ib   = addCommand BS_CMD_REQUEST_VERSION [fromIntegral ib]
    packageProcedure' QueryProcessor ib   = addCommand BS_CMD_REQUEST_TYPE [fromIntegral ib]
    packageProcedure' QueryProcessorE ib  = addCommand BS_CMD_REQUEST_TYPE [fromIntegral ib]
    packageProcedure' Micros ib           = addCommand BS_CMD_REQUEST_MICROS [fromIntegral ib]
    packageProcedure' MicrosE ib          = addCommand BS_CMD_REQUEST_MICROS [fromIntegral ib]
    packageProcedure' Millis ib           = addCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib]
    packageProcedure' MillisE ib          = addCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib]
    packageProcedure' (DigitalRead p) ib  = addCommand DIG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr $ lit p))
    packageProcedure' (DigitalReadE pe) ib = addCommand DIG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr pe))
    packageProcedure' (DigitalPortRead p m) ib  = addCommand DIG_CMD_READ_PORT ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit m)))
    packageProcedure' (DigitalPortReadE pe me) ib = addCommand DIG_CMD_READ_PORT ((fromIntegral ib) : ((packageExpr pe) ++ (packageExpr me)))
    packageProcedure' (AnalogRead p) ib   = addCommand ALG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr $ lit p))
    packageProcedure' (AnalogReadE pe) ib = addCommand ALG_CMD_READ_PIN ((fromIntegral ib) : (packageExpr pe))
    packageProcedure' (I2CRead sa cnt) ib = addCommand I2C_CMD_READ ((fromIntegral ib) : ((packageExpr $ lit sa) ++ (packageExpr $ lit cnt)))
    packageProcedure' (I2CReadE sae cnte) ib = addCommand I2C_CMD_READ ((fromIntegral ib) : ((packageExpr sae) ++ (packageExpr cnte)))
    packageProcedure' (Stepper2Pin s p1 p2) ib = addCommand STEP_CMD_2PIN ((fromIntegral ib) : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2)))
    packageProcedure' (Stepper2PinE s p1 p2) ib = addCommand STEP_CMD_2PIN ((fromIntegral ib) : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)))
    packageProcedure' (Stepper4Pin s p1 p2 p3 p4) ib = addCommand STEP_CMD_4PIN ((fromIntegral ib) : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2) ++ (packageExpr $ lit p3) ++ (packageExpr $ lit p4)))
    packageProcedure' (Stepper4PinE s p1 p2 p3 p4) ib = addCommand STEP_CMD_4PIN ((fromIntegral ib) : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)++ (packageExpr p3) ++ (packageExpr p4)))
    packageProcedure' (StepperStepE st s) ib = addCommand STEP_CMD_STEP ((fromIntegral ib) : ((packageExpr st) ++ (packageExpr s)))
    packageProcedure' (ServoAttach p) ib = addCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
    packageProcedure' (ServoAttachE p) ib = addCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr p) ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
    packageProcedure' (ServoAttachMinMax p min max) ib = addCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr $ lit p) ++ (packageExpr $ lit min) ++ (packageExpr $ lit max)))
    packageProcedure' (ServoAttachMinMaxE p min max) ib = addCommand SRVO_CMD_ATTACH ((fromIntegral ib) : ((packageExpr p)++ (packageExpr min) ++ (packageExpr max)))
    packageProcedure' (ServoRead sv) ib = addCommand SRVO_CMD_READ ((fromIntegral ib) : ((packageExpr $ lit sv)))
    packageProcedure' (ServoReadE sv) ib = addCommand SRVO_CMD_READ ((fromIntegral ib) : ((packageExpr sv)))
    packageProcedure' (ServoReadMicros sv) ib = addCommand SRVO_CMD_READ_MICROS ((fromIntegral ib) : ((packageExpr $ lit sv)))
    packageProcedure' (ServoReadMicrosE sv) ib = addCommand SRVO_CMD_READ_MICROS ((fromIntegral ib) : ((packageExpr sv)))
    packageProcedure' QueryAllTasks ib    = addCommand SCHED_CMD_QUERY_ALL [fromIntegral ib]
    packageProcedure' QueryAllTasksE ib   = addCommand SCHED_CMD_QUERY_ALL [fromIntegral ib]
    packageProcedure' (QueryTask tid) ib  = addCommand SCHED_CMD_QUERY ((fromIntegral ib) : (packageExpr $ lit tid))
    packageProcedure' (QueryTaskE tide) ib = addCommand SCHED_CMD_QUERY ((fromIntegral ib) : (packageExpr tide))
    packageProcedure' (DelayMillis ms) ib  = addCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib) : (packageExpr $ lit ms))
    packageProcedure' (DelayMillisE ms) ib = addCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib) : (packageExpr ms))
    packageProcedure' (DelayMicros ms) ib  = addCommand BC_CMD_DELAY_MICROS ((fromIntegral ib) : (packageExpr $ lit ms))
    packageProcedure' (DelayMicrosE ms) ib = addCommand BC_CMD_DELAY_MICROS ((fromIntegral ib) : (packageExpr ms))
    packageProcedure' (BootTaskE tids) ib = addCommand SCHED_CMD_BOOT_TASK ((fromIntegral ib) : (packageExpr tids))
    packageProcedure' (ReadRemoteRefB (RemoteRefB i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_BOOL, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefW8 (RemoteRefW8 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefW16 (RemoteRefW16 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefW32 (RemoteRefW32 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_WORD32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefI8 (RemoteRefI8 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefI16 (RemoteRefI16 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT16, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefI32 (RemoteRefI32 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_INT32, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefL8 (RemoteRefL8 i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_LIST8, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (ReadRemoteRefFloat (RemoteRefFloat i)) ib = addCommand REF_CMD_READ [fromIntegral $ fromEnum REF_FLOAT, fromIntegral ib, exprCmdVal EXPR_WORD8 EXPR_LIT, fromIntegral i]
    packageProcedure' (DebugE s) ib = addCommand BS_CMD_DEBUG ((fromIntegral ib) : (packageExpr s))
    packageProcedure' DebugListen ib = return B.empty


packageRemoteBinding' :: RefType -> Expr a -> State CommandState B.ByteString
packageRemoteBinding' rt e = do
    s <- get
    addCommand REF_CMD_NEW ([fromIntegral $ fromEnum rt, fromIntegral (ib s), fromIntegral (ix s)] ++ (packageExpr e))

packageRemoteBinding :: ArduinoProcedure a -> State CommandState B.ByteString
packageRemoteBinding (NewRemoteRefB e) =  packageRemoteBinding' REF_BOOL e
packageRemoteBinding (NewRemoteRefW8 e) =  packageRemoteBinding' REF_WORD8 e
packageRemoteBinding (NewRemoteRefW16 e) =  packageRemoteBinding' REF_WORD16 e
packageRemoteBinding (NewRemoteRefW32 e) =  packageRemoteBinding' REF_WORD32 e
packageRemoteBinding (NewRemoteRefI8 e) =  packageRemoteBinding' REF_INT8 e
packageRemoteBinding (NewRemoteRefI16 e) =  packageRemoteBinding' REF_INT16 e
packageRemoteBinding (NewRemoteRefI32 e) =  packageRemoteBinding' REF_INT32 e
packageRemoteBinding (NewRemoteRefL8 e) =  packageRemoteBinding' REF_LIST8 e
packageRemoteBinding (NewRemoteRefFloat e) =  packageRemoteBinding' REF_FLOAT e

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
      (BS_RESP_DEBUG, [])               -> DebugResp
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
parseQueryResult (DebugE _) DebugResp = Just ()
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
