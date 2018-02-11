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
{-# OPTIONS_GHC -fmax-pmcheck-iterations=9000000 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hardware.Haskino.Protocol(framePackage, packageCommand,
                                            packageProcedure, packageRemoteBinding,
                                            unpackageResponse, parseQueryResult,
                                            maxFirmwareSize, packageExpr,
                                            CommandState(..) ) where

import           Control.Monad.State
import           Control.Remote.Applicative.Types as T
import           Control.Remote.Monad
import           Control.Remote.Monad.Types       as T
import           Data.Bits                        (xor)
import qualified Data.ByteString                  as B
import           Data.Int                         (Int16)
import           Data.Word                        (Word8)
import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Expr
import           System.Hardware.Haskino.Utils

-- | Maximum size of a Haskino Firmware message
maxFirmwareSize :: Int
maxFirmwareSize = 256

-- | Minimum and maximum servo pulse widths
minServo :: Int16
minServo = 544

maxServo :: Int16
maxServo = 2400

data CommandState = CommandState {ix        :: Int
                                , ib        :: Int
                                , block     :: B.ByteString
                                , blocks    :: [B.ByteString]
                                , pureLast  :: Bool
                                , pureLasts :: [Bool]
                                , iterBinds :: [Int]}

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

packageUnsupported :: String -> State CommandState B.ByteString
packageUnsupported s = do
  _ <- error $ "Error: Cannot package Shallow Task command, use deep version:" ++ s
  return B.empty

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Haskino Firmware protocol.
packageCommand :: forall a . ArduinoPrimitive a -> State CommandState B.ByteString
packageCommand SystemResetE =
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
packageCommand (I2CWriteE sa w8s) =
    addCommand I2C_CMD_WRITE (packageExpr sa ++ packageExpr w8s)
packageCommand I2CConfigE =
    addCommand I2C_CMD_CONFIG []
packageCommand (SerialBeginE p r) =
    addCommand SER_CMD_BEGIN (packageExpr p ++ packageExpr r)
packageCommand (SerialEndE p) =
    addCommand SER_CMD_END (packageExpr p)
packageCommand (SerialWriteE p w) =
    addCommand SER_CMD_WRITE (packageExpr p ++ packageExpr w)
packageCommand (SerialWriteListE p ws) =
    addCommand SER_CMD_WRITE_LIST (packageExpr p ++ packageExpr ws)
packageCommand (StepperSetSpeedE st sp) =
    addCommand STEP_CMD_SET_SPEED (packageExpr st ++ packageExpr sp)
packageCommand (ServoDetachE sv) =
    addCommand SRVO_CMD_DETACH (packageExpr sv)
packageCommand (ServoWriteE sv w) =
    addCommand SRVO_CMD_WRITE (packageExpr sv ++ packageExpr w)
packageCommand (ServoWriteMicrosE sv w) =
    addCommand SRVO_CMD_WRITE_MICROS (packageExpr sv ++ packageExpr w)
packageCommand (DeleteTask tid) = packageUnsupported $ "createTask " ++ show tid
packageCommand (DeleteTaskE tid) =
    addCommand SCHED_CMD_DELETE_TASK (packageExpr tid)
packageCommand (ScheduleTask tid tt) = packageUnsupported $ "scheduleTask " ++ show tid ++ " " ++ show tt
packageCommand (ScheduleTaskE tid tt) =
    addCommand SCHED_CMD_SCHED_TASK (packageExpr tid ++ packageExpr tt)
packageCommand ScheduleReset = packageUnsupported $ "scheduleReset"
packageCommand ScheduleResetE =
    addCommand SCHED_CMD_RESET []
packageCommand (AttachInt p t _) = packageUnsupported $ "attachInt " ++ show p ++ " " ++ show t
packageCommand (AttachIntE p t m) =
    addCommand SCHED_CMD_ATTACH_INT (packageExpr p ++ packageExpr t ++ packageExpr m)
packageCommand (DetachInt p) = packageUnsupported $ "detachInt " ++ show p
packageCommand (DetachIntE p) =
    addCommand SCHED_CMD_DETACH_INT (packageExpr p)
packageCommand Interrupts = packageUnsupported $ "interrupts"
packageCommand (InterruptsE) =
    addCommand SCHED_CMD_INTERRUPTS []
packageCommand NoInterrupts = packageUnsupported $ "noInterrupts"
packageCommand (NoInterruptsE) =
    addCommand SCHED_CMD_NOINTERRUPTS []
packageCommand (GiveSem i) = packageUnsupported $ "giveSem " ++ show i
packageCommand (GiveSemE i) =
    addCommand SCHED_CMD_GIVE_SEM (packageExpr i)
packageCommand (TakeSem i) = packageUnsupported $ "takeSem " ++ show i
packageCommand (TakeSemE i) =
    addCommand SCHED_CMD_TAKE_SEM (packageExpr i)
packageCommand (CreateTask tid _) = packageUnsupported $ "createTask " ++ show tid
packageCommand (CreateTaskE tid m) = do
    (_, td, _) <- packageCodeBlock m
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
packageCommand (WriteRemoteRefBE (RemoteRefB i) e) = addWriteRefCommand EXPR_BOOL i e
packageCommand (WriteRemoteRefW8E (RemoteRefW8 i) e) = addWriteRefCommand EXPR_WORD8 i e
packageCommand (WriteRemoteRefW16E (RemoteRefW16 i) e) = addWriteRefCommand EXPR_WORD16 i e
packageCommand (WriteRemoteRefW32E (RemoteRefW32 i) e) = addWriteRefCommand EXPR_WORD32 i e
packageCommand (WriteRemoteRefI8E (RemoteRefI8 i) e) = addWriteRefCommand EXPR_INT8 i e
packageCommand (WriteRemoteRefI16E (RemoteRefI16 i) e) = addWriteRefCommand EXPR_INT16 i e
packageCommand (WriteRemoteRefI32E (RemoteRefI32 i) e) = addWriteRefCommand EXPR_INT32 i e
packageCommand (WriteRemoteRefIE (RemoteRefI i) e) = addWriteRefCommand EXPR_INT32 i e
packageCommand (WriteRemoteRefL8E (RemoteRefL8 i) e) = addWriteRefCommand EXPR_LIST8 i e
packageCommand (WriteRemoteRefFloatE (RemoteRefFloat i) e) = addWriteRefCommand EXPR_FLOAT i e
packageCommand (ModifyRemoteRefBE (RemoteRefB i) f) = addWriteRefCommand EXPR_BOOL i f
packageCommand (ModifyRemoteRefW8E (RemoteRefW8 i) f) = addWriteRefCommand EXPR_WORD8 i f
packageCommand (ModifyRemoteRefW16E (RemoteRefW16 i) f) = addWriteRefCommand EXPR_WORD16 i f
packageCommand (ModifyRemoteRefW32E (RemoteRefW32 i) f) = addWriteRefCommand EXPR_WORD32 i f
packageCommand (ModifyRemoteRefI8E (RemoteRefI8 i) f) = addWriteRefCommand EXPR_INT8 i f
packageCommand (ModifyRemoteRefI16E (RemoteRefI16 i) f) = addWriteRefCommand EXPR_INT16 i f
packageCommand (ModifyRemoteRefI32E (RemoteRefI32 i) f) = addWriteRefCommand EXPR_INT32 i f
packageCommand (ModifyRemoteRefIE (RemoteRefI i) f) = addWriteRefCommand EXPR_INT32 i f
packageCommand (ModifyRemoteRefL8E (RemoteRefL8 i) f) = addWriteRefCommand EXPR_LIST8 i f
packageCommand (ModifyRemoteRefFloatE (RemoteRefFloat i) f) = addWriteRefCommand EXPR_FLOAT i f
packageCommand _ = error $ "packageCommand: Error Command not supported (It may have been a procedure)"

addWriteRefCommand :: ExprType -> Int -> Expr a -> State CommandState B.ByteString
addWriteRefCommand t i e =
  addCommand REF_CMD_WRITE ([toW8 t, toW8 EXPR_WORD8, toW8 EXPR_LIT, fromIntegral i] ++ packageExpr e)

packageCodeBlock :: Arduino a -> State CommandState (a, B.ByteString, Bool)
packageCodeBlock (Arduino commands) = do
    startNewBlock
    ret <- packMonad commands
    s' <- get
    str <- endCurrentBlock
    return (ret, str, pureLast s')
  where
      startNewBlock :: State CommandState ()
      startNewBlock = do
          s <- get
          put s {pureLast = False, pureLasts = (pureLast s) : (pureLasts s),
                 block = B.empty, blocks = (block s) : (blocks s)}

      endCurrentBlock :: State CommandState B.ByteString
      endCurrentBlock = do
          s <- get
          put s {pureLast = head $ pureLasts s, pureLasts = tail $ pureLasts s,
                 block = head $ blocks s, blocks = tail $ blocks s}
          return $ block s

      addToBlock :: B.ByteString -> State CommandState ()
      addToBlock bs = do
          s <- get
          put s {block = B.append (block s) bs}

      packShallowProcedure :: ArduinoPrimitive a -> a -> State CommandState a
      packShallowProcedure p r = do
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          return r

      packDeepProcedure :: ArduinoPrimitive a -> State CommandState Int
      packDeepProcedure p = do
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          s <- get
          put s {ib = (ib s) + 1}
          return $ ib s

      packIfEitherProcedure :: ArduinoPrimitive a -> State CommandState Int
      packIfEitherProcedure p = do
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          s <- get
          return $ ib s

      packIterateProcedure :: ArduinoPrimitive a -> State CommandState Int
      packIterateProcedure p = do
          s <- get
          pp <- packageProcedure p
          addToBlock $ lenPackage pp
          return $ ib s

      packNewRef :: ArduinoPrimitive a -> a -> State CommandState a
      packNewRef p r = do
          prb <- packageRemoteBinding p
          addToBlock $ lenPackage prb
          s <- get
          put s {ib = (ib s) + 1, ix = (ix s) + 1}
          return r

      packProcedure :: ArduinoPrimitive a -> State CommandState a
      packProcedure QueryFirmware = packShallowProcedure QueryFirmware 0
      packProcedure QueryFirmwareE = do
          i <- packDeepProcedure QueryFirmwareE
          return $ RemBindW16 i
      packProcedure QueryProcessor = packShallowProcedure QueryProcessor 0
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
      packProcedure (DelayMillisE ms) = packShallowProcedure (DelayMillisE ms) LitUnit
      packProcedure (DelayMicros ms) = packShallowProcedure (DelayMicros ms) ()
      packProcedure (DelayMicrosE ms) = packShallowProcedure (DelayMicrosE ms) LitUnit
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
      packProcedure (SerialAvailable p) = packShallowProcedure (SerialAvailable p) 0
      packProcedure (SerialAvailableE p) = do
          i <- packDeepProcedure (SerialAvailableE p)
          return $ RemBindW8 i
      packProcedure (SerialRead p) = packShallowProcedure (SerialRead p) 0
      packProcedure (SerialReadE p) = do
          i <- packDeepProcedure (SerialReadE p)
          return $ RemBindI32 i
      packProcedure (SerialReadList p) = packShallowProcedure (SerialReadList p) []
      packProcedure (SerialReadListE p) = do
          i <- packDeepProcedure (SerialReadListE p)
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
      packProcedure (ServoAttachMinMax p mi ma) = packShallowProcedure (ServoAttachMinMax p mi ma) 0
      packProcedure (ServoAttachMinMaxE p mi ma) = do
          i <- packDeepProcedure (ServoAttachMinMaxE p mi ma)
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
      packProcedure (ReadRemoteRefBE (RemoteRefB i')) = do
          i <- packDeepProcedure (ReadRemoteRefBE (RemoteRefB i'))
          return $ RemBindB i
      packProcedure (ReadRemoteRefW8E (RemoteRefW8 i')) = do
          i <- packDeepProcedure (ReadRemoteRefW8E (RemoteRefW8 i'))
          return $ RemBindW8 i
      packProcedure (ReadRemoteRefW16E (RemoteRefW16 i')) = do
          i <- packDeepProcedure (ReadRemoteRefW16E (RemoteRefW16 i'))
          return $ RemBindW16 i
      packProcedure (ReadRemoteRefW32E (RemoteRefW32 i')) = do
          i <- packDeepProcedure (ReadRemoteRefW32E (RemoteRefW32 i'))
          return $ RemBindW32 i
      packProcedure (ReadRemoteRefI8E (RemoteRefI8 i')) = do
          i <- packDeepProcedure (ReadRemoteRefI8E (RemoteRefI8 i'))
          return $ RemBindI8 i
      packProcedure (ReadRemoteRefI16E (RemoteRefI16 i')) = do
          i <- packDeepProcedure (ReadRemoteRefI16E (RemoteRefI16 i'))
          return $ RemBindI16 i
      packProcedure (ReadRemoteRefI32E (RemoteRefI32 i')) = do
          i <- packDeepProcedure (ReadRemoteRefI32E (RemoteRefI32 i'))
          return $ RemBindI32 i
      packProcedure (ReadRemoteRefIE (RemoteRefI i')) = do
          i <- packDeepProcedure (ReadRemoteRefIE (RemoteRefI i'))
          return $ RemBindI i
      packProcedure (ReadRemoteRefL8E (RemoteRefL8 i')) = do
          i <- packDeepProcedure (ReadRemoteRefL8E (RemoteRefL8 i'))
          return $ RemBindList8 i
      packProcedure (ReadRemoteRefFloatE (RemoteRefFloat i')) = do
          i <- packDeepProcedure (ReadRemoteRefFloatE (RemoteRefFloat i'))
          return $ RemBindFloat i
      packProcedure (NewRemoteRefBE e) = do
          s <- get
          packNewRef (NewRemoteRefBE e) (RemoteRefB (ix s))
      packProcedure (NewRemoteRefW8E e) = do
          s <- get
          packNewRef (NewRemoteRefW8E e) (RemoteRefW8 (ix s))
      packProcedure (NewRemoteRefW16E e) = do
          s <- get
          packNewRef (NewRemoteRefW16E e) (RemoteRefW16 (ix s))
      packProcedure (NewRemoteRefW32E e) = do
          s <- get
          packNewRef (NewRemoteRefW32E e) (RemoteRefW32 (ix s))
      packProcedure (NewRemoteRefI8E e) = do
          s <- get
          packNewRef (NewRemoteRefI8E e) (RemoteRefI8 (ix s))
      packProcedure (NewRemoteRefI16E e) = do
          s <- get
          packNewRef (NewRemoteRefI16E e) (RemoteRefI16 (ix s))
      packProcedure (NewRemoteRefI32E e) = do
          s <- get
          packNewRef (NewRemoteRefI32E e) (RemoteRefI32 (ix s))
      packProcedure (NewRemoteRefIE e) = do
          s <- get
          packNewRef (NewRemoteRefIE e) (RemoteRefI (ix s))
      packProcedure (NewRemoteRefL8E e) = do
          s <- get
          packNewRef (NewRemoteRefL8E e) (RemoteRefL8 (ix s))
      packProcedure (NewRemoteRefFloatE e) = do
          s <- get
          packNewRef (NewRemoteRefFloatE e) (RemoteRefFloat (ix s))
      packProcedure (IfThenElseUnitE e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseUnitE e cb1 cb2)
          return $ RemBindUnit i
      packProcedure (IfThenElseBoolE e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseBoolE e cb1 cb2)
          return $ RemBindB i
      packProcedure (IfThenElseWord8E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseWord8E e cb1 cb2)
          return $ RemBindW8 i
      packProcedure (IfThenElseWord16E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseWord16E e cb1 cb2)
          return $ RemBindW16 i
      packProcedure (IfThenElseWord32E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseWord32E e cb1 cb2)
          return $ RemBindW32 i
      packProcedure (IfThenElseInt8E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseInt8E e cb1 cb2)
          return $ RemBindI8 i
      packProcedure (IfThenElseInt16E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseInt16E e cb1 cb2)
          return $ RemBindI16 i
      packProcedure (IfThenElseInt32E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseInt32E e cb1 cb2)
          return $ RemBindI32 i
      packProcedure (IfThenElseIntE e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseIntE e cb1 cb2)
          return $ RemBindI i
      packProcedure (IfThenElseL8E e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseL8E e cb1 cb2)
          return $ RemBindList8 i
      packProcedure (IfThenElseFloatE e cb1 cb2) = do
          i <- packDeepProcedure (IfThenElseFloatE e cb1 cb2)
          return $ RemBindFloat i
      -- The following IfThenElse* functions generated by toold/GenEitherTypes.hs
      packProcedure (IfThenElseUnitUnit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitUnit e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitBool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitBool e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitW8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitW8 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitW16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitW16 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitW32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitW32 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitI8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitI8 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitI16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitI16 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitI32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitI32 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitI e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitI e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitL8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitL8 e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseUnitFloat e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseUnitFloat e cb1 cb2)
          return $ ExprLeft $ RemBindUnit i
      packProcedure (IfThenElseBoolUnit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolUnit e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolBool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolBool e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolW8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolW8 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolW16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolW16 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolW32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolW32 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolI8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolI8 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolI16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolI16 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolI32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolI32 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolI e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolI e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolL8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolL8 e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseBoolFloat e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseBoolFloat e cb1 cb2)
          return $ ExprLeft $ RemBindB i
      packProcedure (IfThenElseW8Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8Unit e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8Bool e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8W8 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8W16 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8W32 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8I8 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8I16 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8I32 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8I e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8L8 e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW8Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW8Float e cb1 cb2)
          return $ ExprLeft $ RemBindW8 i
      packProcedure (IfThenElseW16Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16Unit e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16Bool e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16W8 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16W16 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16W32 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16I8 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16I16 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16I32 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16I e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16L8 e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW16Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW16Float e cb1 cb2)
          return $ ExprLeft $ RemBindW16 i
      packProcedure (IfThenElseW32Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32Unit e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32Bool e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32W8 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32W16 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32W32 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32I8 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32I16 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32I32 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32I e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32L8 e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseW32Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseW32Float e cb1 cb2)
          return $ ExprLeft $ RemBindW32 i
      packProcedure (IfThenElseI8Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8Unit e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8Bool e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8W8 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8W16 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8W32 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8I8 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8I16 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8I32 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8I e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8L8 e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI8Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI8Float e cb1 cb2)
          return $ ExprLeft $ RemBindI8 i
      packProcedure (IfThenElseI16Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16Unit e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16Bool e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16W8 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16W16 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16W32 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16I8 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16I16 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16I32 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16I e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16L8 e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI16Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI16Float e cb1 cb2)
          return $ ExprLeft $ RemBindI16 i
      packProcedure (IfThenElseI32Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32Unit e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32Bool e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32W8 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32W16 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32W32 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32I8 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32I16 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32I32 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32I e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32L8 e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseI32Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseI32Float e cb1 cb2)
          return $ ExprLeft $ RemBindI32 i
      packProcedure (IfThenElseIUnit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIUnit e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIBool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIBool e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIW8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIW8 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIW16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIW16 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIW32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIW32 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseII8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseII8 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseII16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseII16 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseII32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseII32 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseII e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseII e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIL8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIL8 e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseIFloat e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseIFloat e cb1 cb2)
          return $ ExprLeft $ RemBindI i
      packProcedure (IfThenElseL8Unit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8Unit e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8Bool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8Bool e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8W8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8W8 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8W16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8W16 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8W32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8W32 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8I8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8I8 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8I16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8I16 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8I32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8I32 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8I e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8I e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8L8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8L8 e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseL8Float e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseL8Float e cb1 cb2)
          return $ ExprLeft $ RemBindList8 i
      packProcedure (IfThenElseFloatUnit e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatUnit e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatBool e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatBool e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatW8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatW8 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatW16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatW16 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatW32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatW32 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatI8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatI8 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatI16 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatI16 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatI32 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatI32 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatI e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatI e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatL8 e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatL8 e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      packProcedure (IfThenElseFloatFloat e cb1 cb2) = do
          i <- packIfEitherProcedure (IfThenElseFloatFloat e cb1 cb2)
          return $ ExprLeft $ RemBindFloat i
      -- The following Iterate*E functions generated by toold/GenEitherTypes.hs
      packProcedure (IterateUnitUnitE iv bf) = do
          i <- packIterateProcedure (IterateUnitUnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateUnitBoolE iv bf) = do
          i <- packIterateProcedure (IterateUnitBoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateUnitW8E iv bf) = do
          i <- packIterateProcedure (IterateUnitW8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateUnitW16E iv bf) = do
          i <- packIterateProcedure (IterateUnitW16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateUnitW32E iv bf) = do
          i <- packIterateProcedure (IterateUnitW32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateUnitI8E iv bf) = do
          i <- packIterateProcedure (IterateUnitI8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateUnitI16E iv bf) = do
          i <- packIterateProcedure (IterateUnitI16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateUnitI32E iv bf) = do
          i <- packIterateProcedure (IterateUnitI32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateUnitIE iv bf) = do
          i <- packIterateProcedure (IterateUnitIE iv bf)
          return $ RemBindI i
      packProcedure (IterateUnitL8E iv bf) = do
          i <- packIterateProcedure (IterateUnitL8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateUnitFloatE iv bf) = do
          i <- packIterateProcedure (IterateUnitFloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateBoolUnitE iv bf) = do
          i <- packIterateProcedure (IterateBoolUnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateBoolBoolE iv bf) = do
          i <- packIterateProcedure (IterateBoolBoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateBoolW8E iv bf) = do
          i <- packIterateProcedure (IterateBoolW8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateBoolW16E iv bf) = do
          i <- packIterateProcedure (IterateBoolW16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateBoolW32E iv bf) = do
          i <- packIterateProcedure (IterateBoolW32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateBoolI8E iv bf) = do
          i <- packIterateProcedure (IterateBoolI8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateBoolI16E iv bf) = do
          i <- packIterateProcedure (IterateBoolI16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateBoolI32E iv bf) = do
          i <- packIterateProcedure (IterateBoolI32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateBoolIE iv bf) = do
          i <- packIterateProcedure (IterateBoolIE iv bf)
          return $ RemBindI i
      packProcedure (IterateBoolL8E iv bf) = do
          i <- packIterateProcedure (IterateBoolL8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateBoolFloatE iv bf) = do
          i <- packIterateProcedure (IterateBoolFloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateW8UnitE iv bf) = do
          i <- packIterateProcedure (IterateW8UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateW8BoolE iv bf) = do
          i <- packIterateProcedure (IterateW8BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateW8W8E iv bf) = do
          i <- packIterateProcedure (IterateW8W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateW8W16E iv bf) = do
          i <- packIterateProcedure (IterateW8W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateW8W32E iv bf) = do
          i <- packIterateProcedure (IterateW8W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateW8I8E iv bf) = do
          i <- packIterateProcedure (IterateW8I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateW8I16E iv bf) = do
          i <- packIterateProcedure (IterateW8I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateW8I32E iv bf) = do
          i <- packIterateProcedure (IterateW8I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateW8IE iv bf) = do
          i <- packIterateProcedure (IterateW8IE iv bf)
          return $ RemBindI i
      packProcedure (IterateW8L8E iv bf) = do
          i <- packIterateProcedure (IterateW8L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateW8FloatE iv bf) = do
          i <- packIterateProcedure (IterateW8FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateW16UnitE iv bf) = do
          i <- packIterateProcedure (IterateW16UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateW16BoolE iv bf) = do
          i <- packIterateProcedure (IterateW16BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateW16W8E iv bf) = do
          i <- packIterateProcedure (IterateW16W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateW16W16E iv bf) = do
          i <- packIterateProcedure (IterateW16W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateW16W32E iv bf) = do
          i <- packIterateProcedure (IterateW16W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateW16I8E iv bf) = do
          i <- packIterateProcedure (IterateW16I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateW16I16E iv bf) = do
          i <- packIterateProcedure (IterateW16I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateW16I32E iv bf) = do
          i <- packIterateProcedure (IterateW16I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateW16IE iv bf) = do
          i <- packIterateProcedure (IterateW16IE iv bf)
          return $ RemBindI i
      packProcedure (IterateW16L8E iv bf) = do
          i <- packIterateProcedure (IterateW16L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateW16FloatE iv bf) = do
          i <- packIterateProcedure (IterateW16FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateW32UnitE iv bf) = do
          i <- packIterateProcedure (IterateW32UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateW32BoolE iv bf) = do
          i <- packIterateProcedure (IterateW32BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateW32W8E iv bf) = do
          i <- packIterateProcedure (IterateW32W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateW32W16E iv bf) = do
          i <- packIterateProcedure (IterateW32W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateW32W32E iv bf) = do
          i <- packIterateProcedure (IterateW32W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateW32I8E iv bf) = do
          i <- packIterateProcedure (IterateW32I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateW32I16E iv bf) = do
          i <- packIterateProcedure (IterateW32I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateW32I32E iv bf) = do
          i <- packIterateProcedure (IterateW32I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateW32IE iv bf) = do
          i <- packIterateProcedure (IterateW32IE iv bf)
          return $ RemBindI i
      packProcedure (IterateW32L8E iv bf) = do
          i <- packIterateProcedure (IterateW32L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateW32FloatE iv bf) = do
          i <- packIterateProcedure (IterateW32FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateI8UnitE iv bf) = do
          i <- packIterateProcedure (IterateI8UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateI8BoolE iv bf) = do
          i <- packIterateProcedure (IterateI8BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateI8W8E iv bf) = do
          i <- packIterateProcedure (IterateI8W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateI8W16E iv bf) = do
          i <- packIterateProcedure (IterateI8W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateI8W32E iv bf) = do
          i <- packIterateProcedure (IterateI8W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateI8I8E iv bf) = do
          i <- packIterateProcedure (IterateI8I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateI8I16E iv bf) = do
          i <- packIterateProcedure (IterateI8I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateI8I32E iv bf) = do
          i <- packIterateProcedure (IterateI8I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateI8IE iv bf) = do
          i <- packIterateProcedure (IterateI8IE iv bf)
          return $ RemBindI i
      packProcedure (IterateI8L8E iv bf) = do
          i <- packIterateProcedure (IterateI8L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateI8FloatE iv bf) = do
          i <- packIterateProcedure (IterateI8FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateI16UnitE iv bf) = do
          i <- packIterateProcedure (IterateI16UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateI16BoolE iv bf) = do
          i <- packIterateProcedure (IterateI16BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateI16W8E iv bf) = do
          i <- packIterateProcedure (IterateI16W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateI16W16E iv bf) = do
          i <- packIterateProcedure (IterateI16W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateI16W32E iv bf) = do
          i <- packIterateProcedure (IterateI16W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateI16I8E iv bf) = do
          i <- packIterateProcedure (IterateI16I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateI16I16E iv bf) = do
          i <- packIterateProcedure (IterateI16I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateI16I32E iv bf) = do
          i <- packIterateProcedure (IterateI16I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateI16IE iv bf) = do
          i <- packIterateProcedure (IterateI16IE iv bf)
          return $ RemBindI i
      packProcedure (IterateI16L8E iv bf) = do
          i <- packIterateProcedure (IterateI16L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateI16FloatE iv bf) = do
          i <- packIterateProcedure (IterateI16FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateI32UnitE iv bf) = do
          i <- packIterateProcedure (IterateI32UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateI32BoolE iv bf) = do
          i <- packIterateProcedure (IterateI32BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateI32W8E iv bf) = do
          i <- packIterateProcedure (IterateI32W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateI32W16E iv bf) = do
          i <- packIterateProcedure (IterateI32W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateI32W32E iv bf) = do
          i <- packIterateProcedure (IterateI32W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateI32I8E iv bf) = do
          i <- packIterateProcedure (IterateI32I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateI32I16E iv bf) = do
          i <- packIterateProcedure (IterateI32I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateI32I32E iv bf) = do
          i <- packIterateProcedure (IterateI32I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateI32IE iv bf) = do
          i <- packIterateProcedure (IterateI32IE iv bf)
          return $ RemBindI i
      packProcedure (IterateI32L8E iv bf) = do
          i <- packIterateProcedure (IterateI32L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateI32FloatE iv bf) = do
          i <- packIterateProcedure (IterateI32FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateIUnitE iv bf) = do
          i <- packIterateProcedure (IterateIUnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateIBoolE iv bf) = do
          i <- packIterateProcedure (IterateIBoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateIW8E iv bf) = do
          i <- packIterateProcedure (IterateIW8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateIW16E iv bf) = do
          i <- packIterateProcedure (IterateIW16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateIW32E iv bf) = do
          i <- packIterateProcedure (IterateIW32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateII8E iv bf) = do
          i <- packIterateProcedure (IterateII8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateII16E iv bf) = do
          i <- packIterateProcedure (IterateII16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateII32E iv bf) = do
          i <- packIterateProcedure (IterateII32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateIIE iv bf) = do
          i <- packIterateProcedure (IterateIIE iv bf)
          return $ RemBindI i
      packProcedure (IterateIL8E iv bf) = do
          i <- packIterateProcedure (IterateIL8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateIFloatE iv bf) = do
          i <- packIterateProcedure (IterateIFloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateL8UnitE iv bf) = do
          i <- packIterateProcedure (IterateL8UnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateL8BoolE iv bf) = do
          i <- packIterateProcedure (IterateL8BoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateL8W8E iv bf) = do
          i <- packIterateProcedure (IterateL8W8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateL8W16E iv bf) = do
          i <- packIterateProcedure (IterateL8W16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateL8W32E iv bf) = do
          i <- packIterateProcedure (IterateL8W32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateL8I8E iv bf) = do
          i <- packIterateProcedure (IterateL8I8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateL8I16E iv bf) = do
          i <- packIterateProcedure (IterateL8I16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateL8I32E iv bf) = do
          i <- packIterateProcedure (IterateL8I32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateL8IE iv bf) = do
          i <- packIterateProcedure (IterateL8IE iv bf)
          return $ RemBindI i
      packProcedure (IterateL8L8E iv bf) = do
          i <- packIterateProcedure (IterateL8L8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateL8FloatE iv bf) = do
          i <- packIterateProcedure (IterateL8FloatE iv bf)
          return $ RemBindFloat i
      packProcedure (IterateFloatUnitE iv bf) = do
          i <- packIterateProcedure (IterateFloatUnitE iv bf)
          return $ RemBindUnit i
      packProcedure (IterateFloatBoolE iv bf) = do
          i <- packIterateProcedure (IterateFloatBoolE iv bf)
          return $ RemBindB i
      packProcedure (IterateFloatW8E iv bf) = do
          i <- packIterateProcedure (IterateFloatW8E iv bf)
          return $ RemBindW8 i
      packProcedure (IterateFloatW16E iv bf) = do
          i <- packIterateProcedure (IterateFloatW16E iv bf)
          return $ RemBindW16 i
      packProcedure (IterateFloatW32E iv bf) = do
          i <- packIterateProcedure (IterateFloatW32E iv bf)
          return $ RemBindW32 i
      packProcedure (IterateFloatI8E iv bf) = do
          i <- packIterateProcedure (IterateFloatI8E iv bf)
          return $ RemBindI8 i
      packProcedure (IterateFloatI16E iv bf) = do
          i <- packIterateProcedure (IterateFloatI16E iv bf)
          return $ RemBindI16 i
      packProcedure (IterateFloatI32E iv bf) = do
          i <- packIterateProcedure (IterateFloatI32E iv bf)
          return $ RemBindI32 i
      packProcedure (IterateFloatIE iv bf) = do
          i <- packIterateProcedure (IterateFloatIE iv bf)
          return $ RemBindI i
      packProcedure (IterateFloatL8E iv bf) = do
          i <- packIterateProcedure (IterateFloatL8E iv bf)
          return $ RemBindList8 i
      packProcedure (IterateFloatFloatE iv bf) = do
          i <- packIterateProcedure (IterateFloatFloatE iv bf)
          return $ RemBindFloat i
      packProcedure (DebugE tids) = packShallowProcedure (DebugE tids) ()
      -- For sending as part of a Scheduler task, debug and die make no sense.
      -- Instead of signalling an error, at this point they are just ignored.
      packProcedure (Debug _) = return ()
      packProcedure DebugListen = return ()
      packProcedure (Die _ _) = return ()
      packProcedure _ = error "packProcedure: unsupported Procedure (it may have been a command)"

      packAppl :: RemoteApplicative ArduinoPrimitive a -> State CommandState a
      packAppl (T.Primitive p) = do
          s <- get
          put s{pureLast = False}
          case knownResult p of
            Just a -> do
                        pc <- packageCommand p
                        addToBlock $ lenPackage pc
                        return a
            Nothing -> do
                        packProcedure p
      packAppl (T.Ap a1 a2) = do
          f <- packAppl a1
          g <- packAppl a2
          return $ f g
      packAppl (T.Pure a)  = do
          s <- get
          put s{pureLast = True}
          return a
      packAppl (T.Alt _ _) = error "packAppl: \"Alt\" is not supported"
      packAppl  T.Empty    = error "packAppl: \"Empty\" is not supported"

      packMonad :: RemoteMonad  ArduinoPrimitive a -> State CommandState a
      packMonad (T.Appl app) = packAppl app
      packMonad (T.Bind m k) = do
          r <- packMonad m
          packMonad (k r)
      packMonad (T.Ap' m1 m2) = do
          f <- packMonad m1
          g <- packMonad m2
          return $ f g
      packMonad (T.Alt' _ _)  = error "packMonad: \"Alt\" is not supported"
      packMonad T.Empty'      = error "packMonad: \"Alt\" is not supported"
      packMonad (T.Catch _ _) = error "packMonad: \"Catch\" is not supported"
      packMonad (T.Throw  _)  = error "packMonad: \"Throw\" is not supported"

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

packageProcedure :: ArduinoPrimitive a -> State CommandState B.ByteString
packageProcedure p = do
    s <- get
    packageProcedure' p (fromIntegral (ib s))
  where
    packageProcedure' :: ArduinoPrimitive a -> Int -> State CommandState B.ByteString
    packageProcedure' QueryFirmware ib'    = addCommand BS_CMD_REQUEST_VERSION [fromIntegral ib']
    packageProcedure' QueryFirmwareE ib'   = addCommand BS_CMD_REQUEST_VERSION [fromIntegral ib']
    packageProcedure' QueryProcessor ib'   = addCommand BS_CMD_REQUEST_TYPE [fromIntegral ib']
    packageProcedure' QueryProcessorE ib'  = addCommand BS_CMD_REQUEST_TYPE [fromIntegral ib']
    packageProcedure' Micros ib'           = addCommand BS_CMD_REQUEST_MICROS [fromIntegral ib']
    packageProcedure' MicrosE ib'          = addCommand BS_CMD_REQUEST_MICROS [fromIntegral ib']
    packageProcedure' Millis ib'           = addCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib']
    packageProcedure' MillisE ib'          = addCommand BS_CMD_REQUEST_MILLIS [fromIntegral ib']
    packageProcedure' (DigitalRead p') ib'  = addCommand DIG_CMD_READ_PIN ((fromIntegral ib') : (packageExpr $ lit p'))
    packageProcedure' (DigitalReadE pe) ib' = addCommand DIG_CMD_READ_PIN ((fromIntegral ib') : (packageExpr pe))
    packageProcedure' (DigitalPortRead p' m) ib'  = addCommand DIG_CMD_READ_PORT ((fromIntegral ib') : ((packageExpr $ lit p') ++ (packageExpr $ lit m)))
    packageProcedure' (DigitalPortReadE pe me) ib' = addCommand DIG_CMD_READ_PORT ((fromIntegral ib') : ((packageExpr pe) ++ (packageExpr me)))
    packageProcedure' (AnalogRead p') ib'   = addCommand ALG_CMD_READ_PIN ((fromIntegral ib') : (packageExpr $ lit p'))
    packageProcedure' (AnalogReadE pe) ib' = addCommand ALG_CMD_READ_PIN ((fromIntegral ib') : (packageExpr pe))
    packageProcedure' (I2CRead sa cnt) ib' = addCommand I2C_CMD_READ ((fromIntegral ib') : ((packageExpr $ lit sa) ++ (packageExpr $ lit cnt)))
    packageProcedure' (I2CReadE sae cnte) ib' = addCommand I2C_CMD_READ ((fromIntegral ib') : ((packageExpr sae) ++ (packageExpr cnte)))
    packageProcedure' (SerialAvailable p') ib' = addCommand SER_CMD_AVAIL ((fromIntegral ib') : (packageExpr $ lit p'))
    packageProcedure' (SerialAvailableE pe) ib' = addCommand SER_CMD_AVAIL ((fromIntegral ib') : (packageExpr pe))
    packageProcedure' (SerialRead p') ib' = addCommand SER_CMD_READ ((fromIntegral ib') : (packageExpr $ lit p'))
    packageProcedure' (SerialReadE pe) ib' = addCommand SER_CMD_READ ((fromIntegral ib') : (packageExpr pe))
    packageProcedure' (SerialReadList p') ib' = addCommand SER_CMD_READ_LIST ((fromIntegral ib') : (packageExpr $ lit p'))
    packageProcedure' (SerialReadListE pe) ib' = addCommand SER_CMD_READ_LIST ((fromIntegral ib') : (packageExpr pe))
    packageProcedure' (Stepper2Pin s p1 p2) ib' = addCommand STEP_CMD_2PIN ((fromIntegral ib') : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2)))
    packageProcedure' (Stepper2PinE s p1 p2) ib' = addCommand STEP_CMD_2PIN ((fromIntegral ib') : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)))
    packageProcedure' (Stepper4Pin s p1 p2 p3 p4) ib' = addCommand STEP_CMD_4PIN ((fromIntegral ib') : ((packageExpr $ lit s) ++ (packageExpr $ lit p1) ++ (packageExpr $ lit p2) ++ (packageExpr $ lit p3) ++ (packageExpr $ lit p4)))
    packageProcedure' (Stepper4PinE s p1 p2 p3 p4) ib' = addCommand STEP_CMD_4PIN ((fromIntegral ib') : ((packageExpr s) ++ (packageExpr p1) ++ (packageExpr p2)++ (packageExpr p3) ++ (packageExpr p4)))
    packageProcedure' (StepperStepE st s) ib' = addCommand STEP_CMD_STEP ((fromIntegral ib') : ((packageExpr st) ++ (packageExpr s)))
    packageProcedure' (ServoAttach p') ib' = addCommand SRVO_CMD_ATTACH ((fromIntegral ib') : ((packageExpr $ lit p') ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
    packageProcedure' (ServoAttachE p') ib' = addCommand SRVO_CMD_ATTACH ((fromIntegral ib') : ((packageExpr p') ++ (packageExpr $ lit minServo) ++ (packageExpr $ lit maxServo)))
    packageProcedure' (ServoAttachMinMax p' mi ma) ib' = addCommand SRVO_CMD_ATTACH ((fromIntegral ib') : ((packageExpr $ lit p') ++ (packageExpr $ lit mi) ++ (packageExpr $ lit ma)))
    packageProcedure' (ServoAttachMinMaxE p' mi ma) ib' = addCommand SRVO_CMD_ATTACH ((fromIntegral ib') : ((packageExpr p')++ (packageExpr mi) ++ (packageExpr ma)))
    packageProcedure' (ServoRead sv) ib' = addCommand SRVO_CMD_READ ((fromIntegral ib') : ((packageExpr $ lit sv)))
    packageProcedure' (ServoReadE sv) ib' = addCommand SRVO_CMD_READ ((fromIntegral ib') : ((packageExpr sv)))
    packageProcedure' (ServoReadMicros sv) ib' = addCommand SRVO_CMD_READ_MICROS ((fromIntegral ib') : ((packageExpr $ lit sv)))
    packageProcedure' (ServoReadMicrosE sv) ib' = addCommand SRVO_CMD_READ_MICROS ((fromIntegral ib') : ((packageExpr sv)))
    packageProcedure' QueryAllTasks ib'    = addCommand SCHED_CMD_QUERY_ALL [fromIntegral ib']
    packageProcedure' QueryAllTasksE ib'   = addCommand SCHED_CMD_QUERY_ALL [fromIntegral ib']
    packageProcedure' (QueryTask tid) ib'  = addCommand SCHED_CMD_QUERY ((fromIntegral ib') : (packageExpr $ lit tid))
    packageProcedure' (QueryTaskE tide) ib' = addCommand SCHED_CMD_QUERY ((fromIntegral ib') : (packageExpr tide))
    packageProcedure' (DelayMillis ms) ib'  = addCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib') : (packageExpr $ lit ms))
    packageProcedure' (DelayMillisE ms) ib' = addCommand BC_CMD_DELAY_MILLIS ((fromIntegral ib') : (packageExpr ms))
    packageProcedure' (DelayMicros ms) ib'  = addCommand BC_CMD_DELAY_MICROS ((fromIntegral ib') : (packageExpr $ lit ms))
    packageProcedure' (DelayMicrosE ms) ib' = addCommand BC_CMD_DELAY_MICROS ((fromIntegral ib') : (packageExpr ms))
    packageProcedure' (BootTaskE tids) ib' = addCommand SCHED_CMD_BOOT_TASK ((fromIntegral ib') : (packageExpr tids))
    packageProcedure' (ReadRemoteRefBE (RemoteRefB i)) ib' = packageReadRefProcedure EXPR_BOOL ib' i
    packageProcedure' (ReadRemoteRefW8E (RemoteRefW8 i)) ib' = packageReadRefProcedure EXPR_WORD8 ib' i
    packageProcedure' (ReadRemoteRefW16E (RemoteRefW16 i)) ib' = packageReadRefProcedure EXPR_WORD16 ib' i
    packageProcedure' (ReadRemoteRefW32E (RemoteRefW32 i)) ib' = packageReadRefProcedure EXPR_WORD32 ib' i
    packageProcedure' (ReadRemoteRefI8E (RemoteRefI8 i)) ib' = packageReadRefProcedure EXPR_INT8 ib' i
    packageProcedure' (ReadRemoteRefI16E (RemoteRefI16 i)) ib' = packageReadRefProcedure EXPR_INT16 ib' i
    packageProcedure' (ReadRemoteRefI32E (RemoteRefI32 i)) ib' = packageReadRefProcedure EXPR_INT32 ib' i
    packageProcedure' (ReadRemoteRefIE (RemoteRefI i)) ib' = packageReadRefProcedure EXPR_INT32 ib' i
    packageProcedure' (ReadRemoteRefL8E (RemoteRefL8 i)) ib' = packageReadRefProcedure EXPR_LIST8 ib' i
    packageProcedure' (ReadRemoteRefFloatE (RemoteRefFloat i)) ib' = packageReadRefProcedure EXPR_FLOAT ib' i
    packageProcedure' (DebugE s) ib' = addCommand BS_CMD_DEBUG ((fromIntegral ib') : (packageExpr s))
    packageProcedure' (IfThenElseUnitE e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolE e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseWord8E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseWord16E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseWord32E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseInt8E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseInt16E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseInt32E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseIntE e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8E e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatE e cb1 cb2) ib' = packageIfThenElseProcedure EXPR_FLOAT ib' e cb1 cb2
    -- The following IfThenElse* functions generated by toold/GenEitherTypes.hs
    packageProcedure' (IfThenElseUnitUnit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitBool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitW8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitW16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitW32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitI8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitI16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitI32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitI e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitL8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseUnitFloat e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_UNIT EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolUnit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolBool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolW8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolW16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolW32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolI8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolI16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolI32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolI e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolL8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseBoolFloat e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_BOOL EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseW8Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseW8Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseW8W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW8Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD8 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseW16Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseW16Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseW16W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW16Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD16 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseW32Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseW32Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseW32W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseW32Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_WORD32 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseI8Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseI8Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseI8W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI8Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT8 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseI16Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseI16Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseI16W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI16Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT16 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseI32Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseI32Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseI32W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseI32Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseIUnit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseIBool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseIW8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseIW16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseIW32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseII8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseII16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseII32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseII e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseIL8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseIFloat e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_INT32 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseL8Unit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseL8Bool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseL8W8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8W16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8W32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8I8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8I16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8I32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8I e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8L8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseL8Float e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_LIST8 EXPR_FLOAT ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatUnit e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_UNIT ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatBool e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_BOOL ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatW8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_WORD8 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatW16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_WORD16 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatW32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_WORD32 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatI8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_INT8 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatI16 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_INT16 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatI32 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatI e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_INT32 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatL8 e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_LIST8 ib' e cb1 cb2
    packageProcedure' (IfThenElseFloatFloat e cb1 cb2) ib' = packageIfThenElseEitherProcedure EXPR_FLOAT EXPR_FLOAT ib' e cb1 cb2
      -- The following Iterate*E functions generated by toold/GenEitherTypes.hs
    packageProcedure' (IterateUnitUnitE iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_UNIT ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitBoolE iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_BOOL ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitW8E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_WORD8 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitW16E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_WORD16 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitW32E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_WORD32 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitI8E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_INT8 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitI16E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_INT16 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitI32E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_INT32 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitIE iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_INT32 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitL8E iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_LIST8 ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateUnitFloatE iv bf) ib' = packageIterateProcedure EXPR_UNIT EXPR_FLOAT ib' (RemBindUnit ib') iv bf
    packageProcedure' (IterateBoolUnitE iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_UNIT ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolBoolE iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_BOOL ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolW8E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_WORD8 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolW16E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_WORD16 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolW32E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_WORD32 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolI8E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_INT8 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolI16E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_INT16 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolI32E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_INT32 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolIE iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_INT32 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolL8E iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_LIST8 ib' (RemBindB ib') iv bf
    packageProcedure' (IterateBoolFloatE iv bf) ib' = packageIterateProcedure EXPR_BOOL EXPR_FLOAT ib' (RemBindB ib') iv bf
    packageProcedure' (IterateW8UnitE iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_UNIT ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8BoolE iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_BOOL ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8W8E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_WORD8 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8W16E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_WORD16 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8W32E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_WORD32 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8I8E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_INT8 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8I16E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_INT16 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8I32E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_INT32 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8IE iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_INT32 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8L8E iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_LIST8 ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW8FloatE iv bf) ib' = packageIterateProcedure EXPR_WORD8 EXPR_FLOAT ib' (RemBindW8 ib') iv bf
    packageProcedure' (IterateW16UnitE iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_UNIT ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16BoolE iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_BOOL ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16W8E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_WORD8 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16W16E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_WORD16 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16W32E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_WORD32 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16I8E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_INT8 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16I16E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_INT16 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16I32E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_INT32 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16IE iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_INT32 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16L8E iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_LIST8 ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW16FloatE iv bf) ib' = packageIterateProcedure EXPR_WORD16 EXPR_FLOAT ib' (RemBindW16 ib') iv bf
    packageProcedure' (IterateW32UnitE iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_UNIT ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32BoolE iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_BOOL ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32W8E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_WORD8 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32W16E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_WORD16 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32W32E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_WORD32 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32I8E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_INT8 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32I16E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_INT16 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32I32E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_INT32 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32IE iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_INT32 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32L8E iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_LIST8 ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateW32FloatE iv bf) ib' = packageIterateProcedure EXPR_WORD32 EXPR_FLOAT ib' (RemBindW32 ib') iv bf
    packageProcedure' (IterateI8UnitE iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_UNIT ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8BoolE iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_BOOL ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8W8E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_WORD8 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8W16E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_WORD16 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8W32E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_WORD32 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8I8E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_INT8 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8I16E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_INT16 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8I32E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_INT32 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8IE iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_INT32 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8L8E iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_LIST8 ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI8FloatE iv bf) ib' = packageIterateProcedure EXPR_INT8 EXPR_FLOAT ib' (RemBindI8 ib') iv bf
    packageProcedure' (IterateI16UnitE iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_UNIT ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16BoolE iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_BOOL ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16W8E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_WORD8 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16W16E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_WORD16 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16W32E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_WORD32 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16I8E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_INT8 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16I16E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_INT16 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16I32E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_INT32 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16IE iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_INT32 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16L8E iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_LIST8 ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI16FloatE iv bf) ib' = packageIterateProcedure EXPR_INT16 EXPR_FLOAT ib' (RemBindI16 ib') iv bf
    packageProcedure' (IterateI32UnitE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_UNIT ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32BoolE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_BOOL ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32W8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD8 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32W16E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD16 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32W32E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD32 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32I8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT8 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32I16E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT16 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32I32E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT32 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32IE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT32 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32L8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_LIST8 ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateI32FloatE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_FLOAT ib' (RemBindI32 ib') iv bf
    packageProcedure' (IterateIUnitE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_UNIT ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIBoolE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_BOOL ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIW8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD8 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIW16E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD16 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIW32E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_WORD32 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateII8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT8 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateII16E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT16 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateII32E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT32 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIIE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_INT32 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIL8E iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_LIST8 ib' (RemBindI ib') iv bf
    packageProcedure' (IterateIFloatE iv bf) ib' = packageIterateProcedure EXPR_INT32 EXPR_FLOAT ib' (RemBindI ib') iv bf
    packageProcedure' (IterateL8UnitE iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_UNIT ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8BoolE iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_BOOL ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8W8E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_WORD8 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8W16E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_WORD16 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8W32E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_WORD32 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8I8E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_INT8 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8I16E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_INT16 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8I32E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_INT32 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8IE iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_INT32 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8L8E iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_LIST8 ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateL8FloatE iv bf) ib' = packageIterateProcedure EXPR_LIST8 EXPR_FLOAT ib' (RemBindList8 ib') iv bf
    packageProcedure' (IterateFloatUnitE iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_UNIT ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatBoolE iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_BOOL ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatW8E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_WORD8 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatW16E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_WORD16 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatW32E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_WORD32 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatI8E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_INT8 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatI16E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_INT16 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatI32E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_INT32 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatIE iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_INT32 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatL8E iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_LIST8 ib' (RemBindFloat ib') iv bf
    packageProcedure' (IterateFloatFloatE iv bf) ib' = packageIterateProcedure EXPR_FLOAT EXPR_FLOAT ib' (RemBindFloat ib') iv bf
    packageProcedure' DebugListen _ = return B.empty
    packageProcedure' _ _ = error "packageProcedure': unsupported Procedure (it may have been a command)"

packageReadRefProcedure :: ExprType -> Int -> Int -> State CommandState B.ByteString
packageReadRefProcedure t ib' i =
  addCommand REF_CMD_READ [toW8 t, fromIntegral ib', toW8 EXPR_WORD8, toW8 EXPR_LIT, fromIntegral i]

packageIfThenElseProcedure :: ExprType -> Int -> Expr Bool -> Arduino (Expr a) -> Arduino (Expr a) -> State CommandState B.ByteString
packageIfThenElseProcedure rt b e cb1 cb2 = do
    (r1, pc1, _) <- packageCodeBlock cb1
    let rc1 = buildCommand EXPR_CMD_RET $ (fromIntegral b) : packageExpr r1
    let pc1'  = B.append pc1 $ lenPackage rc1
    (r2, pc2, _) <- packageCodeBlock cb2
    let rc2 = buildCommand EXPR_CMD_RET $ (fromIntegral b) : packageExpr r2
    let pc2'  = B.append pc2 $ lenPackage rc2
    let thenSize = word16ToBytes $ fromIntegral (B.length pc1')
    i <- addCommand BC_CMD_IF_THEN_ELSE ([fromIntegral $ fromEnum rt, fromIntegral $ fromEnum rt, fromIntegral b] ++ thenSize ++ (packageExpr e))
    return $ B.append i (B.append pc1' pc2')

packageIfThenElseEitherProcedure :: (ExprB a, ExprB b) => ExprType -> ExprType -> Int -> Expr Bool -> Arduino (ExprEither a b) -> Arduino (ExprEither a b) -> State CommandState B.ByteString
packageIfThenElseEitherProcedure rt1 rt2 _ e cb1 cb2 = do
    s <- get
    let ibs = head $ iterBinds s
    (r1, pc1, _) <- packageCodeBlock cb1
    let rc1 = buildCommand EXPR_CMD_RET $ (fromIntegral ibs) : packageExprEither rt1 rt2 r1
    let pc1'  = B.append pc1 $ lenPackage rc1
    (r2, pc2, _) <- packageCodeBlock cb2
    let rc2 = buildCommand EXPR_CMD_RET $ (fromIntegral ibs) : packageExprEither rt1 rt2 r2
    let pc2'  = B.append pc2 $ lenPackage rc2
    let thenSize = word16ToBytes $ fromIntegral (B.length pc1')
    i <- addCommand BC_CMD_IF_THEN_ELSE ([fromIntegral $ fromEnum rt1, fromIntegral $ fromEnum rt2, fromIntegral ibs] ++ thenSize ++ (packageExpr e))
    return $ B.append i (B.append pc1' pc2')

packageIterateProcedure :: (ExprB a, ExprB b) => ExprType -> ExprType -> Int -> Expr a ->
                           Expr a -> (Expr a -> Arduino(ExprEither a b)) ->
                           State CommandState B.ByteString
packageIterateProcedure ta tb ib' be iv bf = do
    s <- get
    put s {iterBinds = ib' : iterBinds s}
    (r, pc, pureWasLast) <- packageCodeBlock $ bf be
    let rc = buildCommand EXPR_CMD_RET $ (fromIntegral ib') : packageExprEither ta tb r
    let pc'  = if pureWasLast then B.append pc $ lenPackage rc else pc
    w <- addCommand BC_CMD_ITERATE ([fromIntegral $ fromEnum ta, fromIntegral $ fromEnum tb, fromIntegral ib', fromIntegral $ length ive] ++ ive)
    s' <- get
    put s' {iterBinds = tail $ iterBinds s'}
    return $ B.append w pc'
  where
    ive = packageExpr iv

packageRemoteBinding' :: ExprType -> Expr a -> State CommandState B.ByteString
packageRemoteBinding' rt e = do
    s <- get
    addCommand REF_CMD_NEW ([fromIntegral $ fromEnum rt, fromIntegral (ib s), fromIntegral (ix s)] ++ (packageExpr e))

packageRemoteBinding :: ArduinoPrimitive a -> State CommandState B.ByteString
packageRemoteBinding (NewRemoteRefBE e) =  packageRemoteBinding' EXPR_BOOL e
packageRemoteBinding (NewRemoteRefW8E e) =  packageRemoteBinding' EXPR_WORD8 e
packageRemoteBinding (NewRemoteRefW16E e) =  packageRemoteBinding' EXPR_WORD16 e
packageRemoteBinding (NewRemoteRefW32E e) =  packageRemoteBinding' EXPR_WORD32 e
packageRemoteBinding (NewRemoteRefI8E e) =  packageRemoteBinding' EXPR_INT8 e
packageRemoteBinding (NewRemoteRefI16E e) =  packageRemoteBinding' EXPR_INT16 e
packageRemoteBinding (NewRemoteRefI32E e) =  packageRemoteBinding' EXPR_INT32 e
packageRemoteBinding (NewRemoteRefIE e) =  packageRemoteBinding' EXPR_INT32 e
packageRemoteBinding (NewRemoteRefL8E e) =  packageRemoteBinding' EXPR_LIST8 e
packageRemoteBinding (NewRemoteRefFloatE e) =  packageRemoteBinding' EXPR_FLOAT e
packageRemoteBinding _ = error "packageRemoteBinding: Unsupported primitive"

packageSubExpr :: [Word8] -> Expr a -> [Word8]
packageSubExpr ec e = ec ++ packageExpr e

packageTwoSubExpr :: [Word8] -> Expr a -> Expr b -> [Word8]
packageTwoSubExpr ec e1 e2 = ec ++ (packageExpr e1) ++ (packageExpr e2)

packageThreeSubExpr :: [Word8] -> Expr a -> Expr b -> Expr c -> [Word8]
packageThreeSubExpr ec e1 e2 e3 = ec ++ (packageExpr e1) ++ (packageExpr e2)  ++ (packageExpr e3)

packageIfBSubExpr :: [Word8] -> Expr a -> Expr b -> Expr b -> [Word8]
packageIfBSubExpr ec e1 e2 e3 = ec ++ thenSize ++ elseSize ++ pcond ++ pthen ++ pelse
  where
    pcond = packageExpr e1
    pthen = packageExpr e2
    pelse = packageExpr e3
    thenSize = word16ToBytes $ fromIntegral $ length pthen
    elseSize = word16ToBytes $ fromIntegral $ length pelse

packageMathExpr :: ExprFloatOp -> Expr a -> [Word8]
packageMathExpr o e = (exprFCmdVal o) ++ (packageExpr e)

packageTwoMathExpr :: ExprFloatOp -> Expr a -> Expr b -> [Word8]
packageTwoMathExpr o e1 e2 = (exprFCmdVal o) ++ (packageExpr e1) ++ (packageExpr e2)

packageRef :: Int -> [Word8] -> [Word8]
packageRef n ec = ec ++ [fromIntegral n]

packageExprEither :: (ExprB a, ExprB b) => ExprType -> ExprType -> ExprEither a b -> [Word8]
packageExprEither t1  _t2 (ExprLeft  el) = [toW8 t1, toW8 EXPR_LEFT] ++ packageExpr el
packageExprEither _t1 _t2 (ExprRight er) = packageExpr er

packageExpr :: Expr a -> [Word8]
packageExpr (LitUnit) = [toW8 EXPR_UNIT, toW8 EXPR_LIT]
packageExpr (ShowUnit e) = packageSubExpr (exprCmdVal EXPR_UNIT EXPR_SHOW) e
packageExpr (RemBindUnit b) = (exprCmdVal EXPR_UNIT EXPR_BIND) ++ [fromIntegral b]
packageExpr (LitB b) = [toW8 EXPR_BOOL, toW8 EXPR_LIT, if b then 1 else 0]
packageExpr (ShowB e) = packageSubExpr (exprCmdVal EXPR_BOOL EXPR_SHOW) e
packageExpr (RefB n) = packageRef n (exprCmdVal EXPR_BOOL EXPR_REF)
packageExpr (RemBindB b) = (exprCmdVal EXPR_BOOL EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (EqI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_EQ) e1 e2
packageExpr (LessI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_LESS) e1 e2
packageExpr (EqL8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_EQ) e1 e2
packageExpr (LessL8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_LESS) e1 e2
packageExpr (EqFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_EQ) e1 e2
packageExpr (LessFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_LESS) e1 e2
packageExpr (LitPinMode m) = (exprCmdVal EXPR_WORD8 EXPR_LIT) ++ [case m of
                                                                    INPUT         -> 0
                                                                    OUTPUT        -> 1
                                                                    INPUT_PULLUP  -> 2 ]
packageExpr (EqPinMode e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_WORD8 EXPR_EQ) e1 e2
packageExpr (ShowPinMode e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHOW) e
packageExpr (IfPinMode e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_WORD8 EXPR_IF) e1 e2 e3
packageExpr (RemBindPinMode b) = (exprCmdVal EXPR_WORD8 EXPR_BIND) ++ [fromIntegral b]
packageExpr (LitW8 w) = (exprCmdVal EXPR_WORD8 EXPR_LIT) ++ [w]
packageExpr (ShowW8 e) = packageSubExpr (exprCmdVal EXPR_WORD8 EXPR_SHOW) e
packageExpr (RefW8 n) = packageRef n (exprCmdVal EXPR_WORD8 EXPR_REF)
packageExpr (RemBindW8 b) = (exprCmdVal EXPR_WORD8 EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (LitW16 w) = (exprCmdVal EXPR_WORD16 EXPR_LIT) ++ word16ToBytes w
packageExpr (ShowW16 e) = packageSubExpr (exprCmdVal EXPR_WORD16 EXPR_SHOW) e
packageExpr (RefW16 n) = packageRef n (exprCmdVal EXPR_WORD16 EXPR_REF)
packageExpr (RemBindW16 b) = (exprCmdVal EXPR_WORD16 EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (LitW32 w) = (exprCmdVal EXPR_WORD32 EXPR_LIT) ++ word32ToBytes w
packageExpr (ShowW32 e) = packageSubExpr (exprCmdVal EXPR_WORD32 EXPR_SHOW) e
packageExpr (RefW32 n) = packageRef n (exprCmdVal EXPR_WORD32 EXPR_REF)
packageExpr (RemBindW32 b) = (exprCmdVal EXPR_WORD32 EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (LitI8 w) = (exprCmdVal EXPR_INT8 EXPR_LIT) ++ [fromIntegral w]
packageExpr (ShowI8 e) = packageSubExpr (exprCmdVal EXPR_INT8 EXPR_SHOW) e
packageExpr (RefI8 n) = packageRef n (exprCmdVal EXPR_INT8 EXPR_REF)
packageExpr (RemBindI8 b) = (exprCmdVal EXPR_INT8 EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (LitI16 w) = (exprCmdVal EXPR_INT16 EXPR_LIT) ++ word16ToBytes (fromIntegral w)
packageExpr (ShowI16 e) = packageSubExpr (exprCmdVal EXPR_INT16 EXPR_SHOW) e
packageExpr (RefI16 n) = packageRef n (exprCmdVal EXPR_INT16 EXPR_REF)
packageExpr (RemBindI16 b) = (exprCmdVal EXPR_INT16 EXPR_BIND) ++ [fromIntegral b]
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
packageExpr (LitI32 w) = (exprCmdVal EXPR_INT32 EXPR_LIT) ++ word32ToBytes (fromIntegral w)
packageExpr (ShowI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_SHOW) e
packageExpr (RefI32 n) = packageRef n (exprCmdVal EXPR_INT32 EXPR_REF)
packageExpr (RemBindI32 b) = (exprCmdVal EXPR_INT32 EXPR_BIND) ++ [fromIntegral b]
packageExpr (FromIntI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_FINT) e
packageExpr (ToIntI32 e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_TINT) e
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
packageExpr (LitI w) = (exprCmdVal EXPR_INT32 EXPR_LIT) ++ word32ToBytes (fromIntegral w)
packageExpr (ShowI e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_SHOW) e
packageExpr (RefI n) = packageRef n (exprCmdVal EXPR_INT32 EXPR_REF)
packageExpr (RemBindI b) = (exprCmdVal EXPR_INT32 EXPR_BIND) ++ [fromIntegral b]
packageExpr (NegI e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_NEG) e
packageExpr (SignI e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_SIGN) e
packageExpr (AddI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_ADD) e1 e2
packageExpr (SubI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SUB) e1 e2
packageExpr (MultI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_MULT) e1 e2
packageExpr (DivI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_DIV) e1 e2
packageExpr (RemI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_REM) e1 e2
packageExpr (QuotI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_QUOT) e1 e2
packageExpr (ModI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_MOD) e1 e2
packageExpr (AndI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_AND) e1 e2
packageExpr (OrI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_OR) e1 e2
packageExpr (XorI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_XOR) e1 e2
packageExpr (CompI e) = packageSubExpr (exprCmdVal EXPR_INT32 EXPR_COMP) e
packageExpr (ShfLI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SHFL) e1 e2
packageExpr (ShfRI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SHFR) e1 e2
packageExpr (IfI e1 e2 e3) = packageIfBSubExpr (exprCmdVal EXPR_INT32 EXPR_IF) e1 e2 e3
packageExpr (TestBI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_TSTB) e1 e2
packageExpr (SetBI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_SETB) e1 e2
packageExpr (ClrBI e1 e2) = packageTwoSubExpr (exprCmdVal EXPR_INT32 EXPR_CLRB) e1 e2
packageExpr (LitList8 ws) = (exprLCmdVal EXPRL_LIT) ++ [fromIntegral $ length ws] ++ ws
packageExpr (RefList8 n) = packageRef n (exprLCmdVal EXPRL_REF)
packageExpr (RemBindList8 b) = (exprLCmdVal EXPRL_BIND) ++ [fromIntegral b]
packageExpr (IfL8 e1 e2 e3) = packageIfBSubExpr (exprLCmdVal EXPRL_IF) e1 e2 e3
packageExpr (ElemList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_ELEM) e1 e2
packageExpr (LenList8 e) = packageSubExpr (exprLCmdVal EXPRL_LEN) e
packageExpr (ConsList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_CONS) e1 e2
packageExpr (ApndList8 e1 e2) = packageTwoSubExpr (exprLCmdVal EXPRL_APND) e1 e2
packageExpr (PackList8 es) = (exprLCmdVal EXPRL_PACK) ++ [fromIntegral $ length es] ++ (foldl (++) [] (map packageExpr es))
packageExpr (SliceList8 e1 e2 e3) = packageThreeSubExpr (exprLCmdVal EXPRL_SLIC) e1 e2 e3
packageExpr (LitFloat f) = (exprFCmdVal EXPRF_LIT) ++ floatToBytes f
packageExpr (ShowFloat e1 e2) = packageTwoSubExpr (exprFCmdVal EXPRF_SHOW) e1 e2
packageExpr (RefFloat n) = packageRef n (exprFCmdVal EXPRF_REF)
packageExpr (RemBindFloat b) = (exprFCmdVal EXPRF_BIND) ++ [fromIntegral b]
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
packageExpr PiFloat = exprFCmdVal EXPRF_PI
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
      (BC_RESP_IF_THEN_ELSE , [t,l])   | t == toW8 EXPR_UNIT && l == toW8 EXPR_LIT
                                      -> IfThenElseUnitReply ()
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == toW8 EXPR_BOOL && l == toW8 EXPR_LIT
                                      -> IfThenElseBoolReply (if b == 0 then False else True)
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == toW8 EXPR_WORD8 && l == toW8 EXPR_LIT
                                      -> IfThenElseW8Reply b
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2]) | t == toW8 EXPR_WORD16 && l == toW8 EXPR_LIT
                                      -> IfThenElseW16Reply (bytesToWord16 (b1, b2))
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_WORD32 && l == toW8 EXPR_LIT
                                      -> IfThenElseW32Reply (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == toW8 EXPR_INT8 && l == toW8 EXPR_LIT
                                      -> IfThenElseI8Reply $ fromIntegral b
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2]) | t == toW8 EXPR_INT16 && l == toW8 EXPR_LIT
                                      -> IfThenElseI16Reply $ fromIntegral (bytesToWord16 (b1, b2))
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_INT32 && l == toW8 EXPR_LIT
                                      -> IfThenElseI32Reply $ fromIntegral (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_IF_THEN_ELSE , t:l:_:bs) | t == toW8 EXPR_LIST8 && l == toW8 EXPR_LIT
                                      -> IfThenElseL8Reply bs
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_FLOAT && l == toW8 EXPR_LIT
                                      -> IfThenElseFloatReply $ bytesToFloat (b1, b2, b3, b4)
      {-- The IfThenElse Left replies are only used inside of Iterates
       -- so they will never be received by the host, but are here for
       -- test purposes
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == (toW8 EXPR_BOOL  + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseBoolLeftReply (if b == 0 then False else True)
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == (toW8 EXPR_WORD8 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseW8LeftReply b
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2]) | t == (toW8 EXPR_WORD16 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseW16LeftReply (bytesToWord16 (b1, b2))
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == (toW8 EXPR_WORD32 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseW32LeftReply (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_IF_THEN_ELSE , [t,l,b]) | t == (toW8 EXPR_INT8 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseI8LeftReply $ fromIntegral b
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2]) | t == (toW8 EXPR_INT16 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseI16LeftReply $ fromIntegral (bytesToWord16 (b1, b2))
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == (toW8 EXPR_INT32 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseI32LeftReply $ fromIntegral (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_IF_THEN_ELSE , t:l:_:bs) | t == (toW8 EXPR_LIST8 + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseL8LeftReply bs
      (BC_RESP_IF_THEN_ELSE , [t,l,b1,b2,b3,b4]) | t == (toW8 EXPR_FLOAT + 0x80) && l == toW8 EXPR_LIT
                                      -> IfThenElseFloatLeftReply $ bytesToFloat (b1, b2, b3, b4)
      -}
      (BC_RESP_ITERATE , [t,l]) | t == toW8 EXPR_UNIT && l == toW8 EXPR_LIT
                                      -> IterateUnitReply
      (BC_RESP_ITERATE , [t,l,b]) | t == toW8 EXPR_BOOL && l == toW8 EXPR_LIT
                                      -> IterateBoolReply (if b == 0 then False else True)
      (BC_RESP_ITERATE , [t,l,b]) | t == toW8 EXPR_WORD8 && l == toW8 EXPR_LIT
                                      -> IterateW8Reply b
      (BC_RESP_ITERATE , [t,l,b1,b2]) | t == toW8 EXPR_WORD16 && l == toW8 EXPR_LIT
                                      -> IterateW16Reply (bytesToWord16 (b1, b2))
      (BC_RESP_ITERATE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_WORD32 && l == toW8 EXPR_LIT
                                      -> IterateW32Reply (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_ITERATE , [t,l,b]) | t == toW8 EXPR_INT8 && l == toW8 EXPR_LIT
                                      -> IterateI8Reply $ fromIntegral b
      (BC_RESP_ITERATE , [t,l,b1,b2]) | t == toW8 EXPR_INT16 && l == toW8 EXPR_LIT
                                      -> IterateI16Reply $ fromIntegral (bytesToWord16 (b1, b2))
      (BC_RESP_ITERATE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_INT32 && l == toW8 EXPR_LIT
                                      -> IterateI32Reply $ fromIntegral (bytesToWord32 (b1, b2, b3, b4))
      (BC_RESP_ITERATE , t:l:_:bs) | t == toW8 EXPR_LIST8 && l == toW8 EXPR_LIT
                                      -> IterateL8Reply bs
      (BC_RESP_ITERATE , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_FLOAT && l == toW8 EXPR_LIT
                                      -> IterateFloatReply $ bytesToFloat (b1, b2, b3, b4)
      (BS_RESP_DEBUG, [])                    -> DebugResp
      (BS_RESP_VERSION, [majV, minV])        -> Firmware (bytesToWord16 (majV,minV))
      (BS_RESP_TYPE, [p])                    -> ProcessorType p
      (BS_RESP_MICROS, [m0,m1,m2,m3])        -> MicrosReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_MILLIS, [m0,m1,m2,m3])        -> MillisReply (bytesToWord32 (m0,m1,m2,m3))
      (BS_RESP_STRING, rest)                 -> StringMessage (getString rest)
      (DIG_RESP_READ_PIN, [_t,_l,b])         -> DigitalReply b
      (DIG_RESP_READ_PORT, [_t,_l,b])        -> DigitalPortReply b
      (ALG_RESP_READ_PIN, [_t,_l,bl,bh])     -> AnalogReply (bytesToWord16 (bl,bh))
      (I2C_RESP_READ, _:_:_:xs)              -> I2CReply xs
      (SER_RESP_AVAIL, [w0])                 -> SerialAvailableReply w0
      (SER_RESP_READ, [i0,i1,i2,i3])         -> SerialReadReply (bytesToInt32 (i0,i1,i2,i3))
      (SER_RESP_READ_LIST, _:_:_:xs)         -> SerialReadListReply xs
      (STEP_RESP_2PIN, [_t,_l,st])           -> Stepper2PinReply st
      (STEP_RESP_4PIN, [_t,_l,st])           -> Stepper4PinReply st
      (STEP_RESP_STEP, [])                   -> StepperStepReply
      (SRVO_RESP_ATTACH, [_t,_l,sv])         -> ServoAttachReply sv
      (SRVO_RESP_READ, [_t,_l,il,ih])        -> ServoReadReply (fromIntegral (bytesToWord16 (il,ih)))
      (SRVO_RESP_READ_MICROS, [_t,_l,il,ih]) -> ServoReadMicrosReply (fromIntegral (bytesToWord16 (il,ih)))
      (SCHED_RESP_BOOT, [_t,_l,b])           -> BootTaskResp b
      (SCHED_RESP_QUERY_ALL, _:_:_:ts)       -> QueryAllTasksReply ts
      (SCHED_RESP_QUERY, ts) | length ts == 0 ->
          QueryTaskReply Nothing
      (SCHED_RESP_QUERY, ts) | length ts >= 9 ->
          let ts0:ts1:tl0:tl1:tp0:tp1:tt0:tt1:tt2:tt3:_ = ts
          in QueryTaskReply (Just (bytesToWord16 (ts0,ts1),
                                   bytesToWord16 (tl0,tl1),
                                   bytesToWord16 (tp0,tp1),
                                   bytesToWord32 (tt0,tt1,tt2,tt3)))
      (REF_RESP_READ , [t,l,b]) | t == toW8 EXPR_BOOL && l == toW8 EXPR_LIT
                                      -> ReadRefBReply (if b == 0 then False else True)
      (REF_RESP_READ , [t,l,b]) | t == toW8 EXPR_WORD8 && l == toW8 EXPR_LIT
                                      -> ReadRefW8Reply b
      (REF_RESP_READ , [t,l,b1,b2]) | t == toW8 EXPR_WORD16 && l == toW8 EXPR_LIT
                                      -> ReadRefW16Reply (bytesToWord16 (b1, b2))
      (REF_RESP_READ , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_WORD32 && l == toW8 EXPR_LIT
                                      -> ReadRefW32Reply (bytesToWord32 (b1, b2, b3, b4))
      (REF_RESP_READ , [t,l,b]) | t == toW8 EXPR_INT8 && l == toW8 EXPR_LIT
                                      -> ReadRefI8Reply $ fromIntegral b
      (REF_RESP_READ , [t,l,b1,b2]) | t == toW8 EXPR_INT16 && l == toW8 EXPR_LIT
                                      -> ReadRefI16Reply $ fromIntegral (bytesToWord16 (b1, b2))
      (REF_RESP_READ , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_INT32 && l == toW8 EXPR_LIT
                                      -> ReadRefI32Reply $ fromIntegral (bytesToWord32 (b1, b2, b3, b4))
      (REF_RESP_READ , t:l:_:bs) | t == toW8 EXPR_LIST8 && l == toW8 EXPR_LIT
                                      -> ReadRefL8Reply bs
      (REF_RESP_READ , [t,l,b1,b2,b3,b4]) | t == toW8 EXPR_FLOAT && l == toW8 EXPR_LIT
                                      -> ReadRefFloatReply $ bytesToFloat (b1, b2, b3, b4)
      (REF_RESP_NEW , [_t,_l,w])      -> NewReply w
      (REF_RESP_NEW , [])             -> FailedNewRef
      _                               -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
parseQueryResult :: ArduinoPrimitive a -> Response -> Maybe a
parseQueryResult QueryFirmware (Firmware v) = Just v
parseQueryResult QueryFirmwareE (Firmware v) = Just (lit v)
parseQueryResult QueryProcessor (ProcessorType pt) = Just $ toEnum $ fromIntegral pt
parseQueryResult QueryProcessorE (ProcessorType pt) = Just $ (lit pt)
parseQueryResult Micros (MicrosReply m) = Just m
parseQueryResult MicrosE (MicrosReply m) = Just (lit m)
parseQueryResult Millis (MillisReply m) = Just m
parseQueryResult MillisE (MillisReply m) = Just (lit m)
parseQueryResult (DelayMicros _) DelayResp = Just ()
parseQueryResult (DelayMicrosE _) DelayResp = Just LitUnit
parseQueryResult (DelayMillis _) DelayResp = Just ()
parseQueryResult (DelayMillisE _) DelayResp = Just LitUnit
parseQueryResult (DebugE _) DebugResp = Just ()
parseQueryResult (DigitalRead _) (DigitalReply d) = Just (if d == 0 then False else True)
parseQueryResult (DigitalReadE _) (DigitalReply d) = Just (if d == 0 then lit False else lit True)
parseQueryResult (DigitalPortRead _ _) (DigitalPortReply d) = Just d
parseQueryResult (DigitalPortReadE _ _) (DigitalPortReply d) = Just (lit d)
parseQueryResult (AnalogRead _) (AnalogReply a) = Just a
parseQueryResult (AnalogReadE _) (AnalogReply a) = Just (lit a)
parseQueryResult (I2CRead _ _) (I2CReply ds) = Just ds
parseQueryResult (I2CReadE _ _) (I2CReply ds) = Just (lit ds)
parseQueryResult (SerialAvailable _) (SerialAvailableReply c) = Just c
parseQueryResult (SerialAvailableE _) (SerialAvailableReply c) = Just (lit c)
parseQueryResult (SerialRead _) (SerialReadReply w) = Just w
parseQueryResult (SerialReadE _) (SerialReadReply w) = Just (lit w)
parseQueryResult (SerialReadList _) (SerialReadListReply ws) = Just ws
parseQueryResult (SerialReadListE _) (SerialReadListReply ws) = Just (lit ws)
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
parseQueryResult (NewRemoteRefBE _) (NewReply r) = Just $ RemoteRefB $ fromIntegral r
parseQueryResult (NewRemoteRefW8E _) (NewReply r) = Just $ RemoteRefW8 $ fromIntegral r
parseQueryResult (NewRemoteRefW16E _) (NewReply r) = Just $ RemoteRefW16 $ fromIntegral r
parseQueryResult (NewRemoteRefW32E _) (NewReply r) = Just $ RemoteRefW32 $ fromIntegral r
parseQueryResult (NewRemoteRefI8E _) (NewReply r) = Just $ RemoteRefI8 $ fromIntegral r
parseQueryResult (NewRemoteRefI16E _) (NewReply r) = Just $ RemoteRefI16 $ fromIntegral r
parseQueryResult (NewRemoteRefI32E _) (NewReply r) = Just $ RemoteRefI32 $ fromIntegral r
parseQueryResult (NewRemoteRefIE _) (NewReply r) = Just $ RemoteRefI $ fromIntegral r
parseQueryResult (NewRemoteRefL8E _) (NewReply r) = Just $ RemoteRefL8 $ fromIntegral r
parseQueryResult (NewRemoteRefFloatE _) (NewReply r) = Just $ RemoteRefFloat$ fromIntegral r
parseQueryResult (ReadRemoteRefBE _) (ReadRefBReply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW8E _) (ReadRefW8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW16E _) (ReadRefW16Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefW32E _) (ReadRefW32Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI8E _) (ReadRefI8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI16E _) (ReadRefI16Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefI32E _) (ReadRefI32Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefIE _) (ReadRefI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (ReadRemoteRefL8E _) (ReadRefL8Reply r) = Just $ lit r
parseQueryResult (ReadRemoteRefFloatE _) (ReadRefFloatReply r) = Just $ lit r
parseQueryResult (IfThenElseUnitE _ _ _) (IfThenElseUnitReply r) = Just $ lit r
parseQueryResult (IfThenElseBoolE _ _ _) (IfThenElseBoolReply r) = Just $ lit r
parseQueryResult (IfThenElseWord8E _ _ _) (IfThenElseW8Reply r) = Just $ lit r
parseQueryResult (IfThenElseWord16E _ _ _) (IfThenElseW16Reply r) = Just $ lit r
parseQueryResult (IfThenElseWord32E _ _ _) (IfThenElseW32Reply r) = Just $ lit r
parseQueryResult (IfThenElseInt8E _ _ _) (IfThenElseI8Reply r) = Just $ lit r
parseQueryResult (IfThenElseInt16E _ _ _) (IfThenElseI16Reply r) = Just $ lit r
parseQueryResult (IfThenElseInt32E _ _ _) (IfThenElseI32Reply r) = Just $ lit r
parseQueryResult (IfThenElseIntE _ _ _) (IfThenElseI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IfThenElseL8E _ _ _) (IfThenElseL8Reply r) = Just $ lit r
parseQueryResult (IfThenElseFloatE _ _ _) (IfThenElseFloatReply r) = Just $ lit r
parseQueryResult (IfThenElseUnitUnit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitUnit _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitBool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitBool _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitW8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitW8 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitW16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitW16 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitW32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitW32 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitI8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitI8 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitI16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitI16 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitI32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitI32 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitI _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseUnitI _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitL8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitL8 _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseUnitFloat _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseUnitFloat _ _ _) (IfThenElseUnitLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolUnit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolUnit _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolBool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolBool _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolW8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolW8 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolW16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolW16 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolW32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolW32 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolI8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolI8 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolI16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolI16 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolI32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolI32 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolI _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseBoolI _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolL8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolL8 _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseBoolFloat _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseBoolFloat _ _ _) (IfThenElseBoolLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8Unit _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8Bool _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8W8 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8W16 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8W32 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8I8 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8I16 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8I32 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseW8I _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8L8 _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW8Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW8Float _ _ _) (IfThenElseW8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16Unit _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16Bool _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16W8 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16W16 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16W32 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16I8 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16I16 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16I32 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseW16I _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16L8 _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW16Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW16Float _ _ _) (IfThenElseW16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32Unit _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32Bool _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32W8 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32W16 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32W32 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32I8 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32I16 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32I32 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseW32I _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32L8 _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseW32Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseW32Float _ _ _) (IfThenElseW32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8Unit _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8Bool _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8W8 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8W16 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8W32 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8I8 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8I16 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8I32 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseI8I _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8L8 _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI8Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI8Float _ _ _) (IfThenElseI8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16Unit _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16Bool _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16W8 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16W16 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16W32 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16I8 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16I16 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16I32 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseI16I _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16L8 _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI16Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI16Float _ _ _) (IfThenElseI16LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32Unit _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32Bool _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32W8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32W16 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32W32 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32I8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32I16 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32I32 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseI32I _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32L8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseI32Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseI32Float _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseIUnit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIUnit _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIBool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIBool _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIW8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIW8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIW16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIW16 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIW32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIW32 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseII8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseII8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseII16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseII16 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseII32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseII32 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseII _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseII _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIL8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIL8 _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseIFloat _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseIFloat _ _ _) (IfThenElseI32LeftReply r) = Just $ ExprLeft $ lit (fromIntegral r)
parseQueryResult (IfThenElseL8Unit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8Unit _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8Bool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8Bool _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8W8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8W8 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8W16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8W16 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8W32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8W32 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8I8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8I8 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8I16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8I16 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8I32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8I32 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8I _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseL8I _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8L8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8L8 _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseL8Float _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseL8Float _ _ _) (IfThenElseL8LeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatUnit _ _ _) (IfThenElseUnitReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatUnit _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatBool _ _ _) (IfThenElseBoolReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatBool _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatW8 _ _ _) (IfThenElseW8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatW8 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatW16 _ _ _) (IfThenElseW16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatW16 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatW32 _ _ _) (IfThenElseW32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatW32 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatI8 _ _ _) (IfThenElseI8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatI8 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatI16 _ _ _) (IfThenElseI16Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatI16 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatI32 _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatI32 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatI _ _ _) (IfThenElseI32Reply r) = Just $ ExprRight $ lit (fromIntegral r)
parseQueryResult (IfThenElseFloatI _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatL8 _ _ _) (IfThenElseL8Reply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatL8 _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IfThenElseFloatFloat _ _ _) (IfThenElseFloatReply r) = Just $ ExprRight $ lit r
parseQueryResult (IfThenElseFloatFloat _ _ _) (IfThenElseFloatLeftReply r) = Just $ ExprLeft $ lit r
parseQueryResult (IterateUnitUnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateUnitBoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateUnitW8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateUnitW16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateUnitW32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateUnitI8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateUnitI16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateUnitI32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateUnitIE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateUnitL8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateUnitFloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateBoolUnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateBoolBoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateBoolW8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateBoolW16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateBoolW32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateBoolI8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateBoolI16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateBoolI32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateBoolIE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateBoolL8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateBoolFloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateW8UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateW8BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateW8W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateW8W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateW8W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateW8I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateW8I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateW8I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateW8IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateW8L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateW8FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateW16UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateW16BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateW16W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateW16W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateW16W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateW16I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateW16I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateW16I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateW16IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateW16L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateW16FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateW32UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateW32BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateW32W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateW32W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateW32W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateW32I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateW32I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateW32I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateW32IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateW32L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateW32FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateI8UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateI8BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateI8W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateI8W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateI8W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateI8I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateI8I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateI8I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateI8IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateI8L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateI8FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateI16UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateI16BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateI16W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateI16W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateI16W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateI16I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateI16I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateI16I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateI16IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateI16L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateI16FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateI32UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateI32BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateI32W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateI32W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateI32W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateI32I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateI32I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateI32I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateI32IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateI32L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateI32FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateIUnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateIBoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateIW8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateIW16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateIW32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateII8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateII16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateII32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateIIE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateIL8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateIFloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateL8UnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateL8BoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateL8W8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateL8W16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateL8W32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateL8I8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateL8I16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateL8I32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateL8IE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateL8L8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateL8FloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult (IterateFloatUnitE _ _) (IterateUnitReply) = Just $ LitUnit
parseQueryResult (IterateFloatBoolE _ _) (IterateBoolReply r) = Just $ lit r
parseQueryResult (IterateFloatW8E _ _) (IterateW8Reply r) = Just $ lit r
parseQueryResult (IterateFloatW16E _ _) (IterateW16Reply r) = Just $ lit r
parseQueryResult (IterateFloatW32E _ _) (IterateW32Reply r) = Just $ lit r
parseQueryResult (IterateFloatI8E _ _) (IterateI8Reply r) = Just $ lit r
parseQueryResult (IterateFloatI16E _ _) (IterateI16Reply r) = Just $ lit r
parseQueryResult (IterateFloatI32E _ _) (IterateI32Reply r) = Just $ lit r
parseQueryResult (IterateFloatIE _ _) (IterateI32Reply r) = Just $ lit (fromIntegral r)
parseQueryResult (IterateFloatL8E _ _) (IterateL8Reply r) = Just $ lit r
parseQueryResult (IterateFloatFloatE _ _) (IterateFloatReply r) = Just $ lit r
parseQueryResult _q _r = Nothing
