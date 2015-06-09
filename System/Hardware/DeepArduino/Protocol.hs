-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino.comm (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs      #-}

module System.Hardware.DeepArduino.Protocol(packageProcedure, packageQuery, unpackageSysEx, unpackageNonSysEx, parseQueryResult) where

import Data.Bits ((.|.), (.&.))
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.Map        as M

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Utils

-- | Wrap a sys-ex message to be sent to the board
sysEx :: SysExCmd -> [Word8] -> B.ByteString
sysEx cmd bs = B.pack $  firmataCmdVal START_SYSEX
                      :  sysExCmdVal cmd
                      :  bs
                      ++ [firmataCmdVal END_SYSEX]

-- | Wrap the start of a sys-ex message to be sent to the board (all except
-- |  the END_SYSEX byte)
startSysEx :: SysExCmd -> [Word8] -> B.ByteString
startSysEx cmd bs = B.pack $  firmataCmdVal START_SYSEX
                      :  sysExCmdVal cmd
                      :  bs

-- | Construct a non sys-ex message
nonSysEx :: FirmataCmd -> [Word8] -> B.ByteString
nonSysEx cmd bs = B.pack $ firmataCmdVal cmd : bs

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
packageProcedure :: ArduinoConnection -> Procedure -> B.ByteString
packageProcedure c SystemReset              = nonSysEx SYSTEM_RESET            []
packageProcedure c (AnalogReport  p b)      = nonSysEx (REPORT_ANALOG_PIN (getInternalPin c p))   [if b then 1 else 0]
packageProcedure c (DigitalReport p b)      = nonSysEx (REPORT_DIGITAL_PORT p) [if b then 1 else 0]
packageProcedure c (SetPinMode p m)         = nonSysEx SET_PIN_MODE            [fromIntegral (pinNo (getInternalPin c p)), fromIntegral (fromEnum m)]
packageProcedure c (DigitalPortWrite p l m) = nonSysEx (DIGITAL_MESSAGE p)     [l, m]
packageProcedure c (DigitalPinWrite p b)    = nonSysEx SET_DIGITAL_PIN_VALUE   [fromIntegral (pinNo (getInternalPin c p)), if b then 1 else 0]
packageProcedure c (AnalogPinWrite p l m)   = nonSysEx (ANALOG_MESSAGE (getInternalPin c p))      [l, m]
packageProcedure c (AnalogPinExtendedWrite p w8s) = sysEx EXTENDED_ANALOG      ([fromIntegral (pinNo (getInternalPin c p))] ++ w8s)
packageProcedure c (SamplingInterval l m)   = sysEx    SAMPLING_INTERVAL       [l, m]
packageProcedure c (I2CWrite m sa w16s)     = sysEx    I2C_REQUEST     ((packageI2c m False sa Nothing) ++
                                                                      (words16ToArduinoBytes w16s)) 
packageProcedure c (DeleteTask tid)         = sysEx SCHEDULER_DATA [schedulerCmdVal DELETE_TASK, tid]
packageProcedure c (Delay tt)               = sysEx SCHEDULER_DATA ([schedulerCmdVal DELAY_TASK] ++ (word32ToArduinoBytes tt))
packageProcedure c (ScheduleTask tid tt)    = sysEx SCHEDULER_DATA ([schedulerCmdVal SCHEDULE_TASK, tid] ++ (word32ToArduinoBytes tt))
packageProcedure c (CreateTask tid m)       = (sysEx SCHEDULER_DATA ([schedulerCmdVal CREATE_TASK, tid] ++ (word16ToArduinoBytes taskSize)))
                                              `B.append` (startSysEx SCHEDULER_DATA [schedulerCmdVal ADD_TO_TASK, tid])
                                              `B.append` (arduinoEncoded taskData)
                                              `B.append` (B.singleton (firmataCmdVal END_SYSEX))
  where
    taskData = packageTaskData c m
    taskSize = fromIntegral (B.length taskData)

packageTaskData :: ArduinoConnection -> Arduino a -> B.ByteString
packageTaskData conn commands =
      packageTaskData' conn commands B.empty
  where
      packBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packBind c (Return a)      k cmds = packageTaskData' c (k a) cmds
      packBind c (Bind m k1)    k2 cmds = packBind c m (\ r -> Bind (k1 r) k2) cmds
      packBind c (Procedure cmd) k cmds = packageTaskData' c (k ()) (B.append cmds (packageProcedure c cmd))
      -- For sending as part of a Scheduler task, locals make no sense.  
      -- Queries will work, but receiving them is problematic at the moment.
      -- Instead of signalling an error, at this point they are just ignored.
      packBind c (Local local)   k cmds = packLocal c local k cmds
      packBind c (Query query)   k cmds = packQuery c query k cmds

      packLocal :: ArduinoConnection -> Local a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packLocal c (AnalogPinRead _) k cmds = packageTaskData' c (k 0) cmds
      packLocal c (DigitalPortRead _) k cmds = packageTaskData' c (k 0) cmds
      packLocal c (DigitalPinRead _) k cmds = packageTaskData' c (k False) cmds

      packQuery :: ArduinoConnection -> Query a -> (a -> Arduino b) -> B.ByteString -> B.ByteString
      packQuery c QueryFirmware k cmds = packageTaskData' c (k (0,0,[])) cmds
      packQuery c CapabilityQuery k cmds = packageTaskData' c (k (BoardCapabilities M.empty)) cmds
      packQuery c AnalogMappingQuery k cmds = packageTaskData' c (k ([])) cmds
      packQuery c (Pulse _ _ _ _) k cmds = packageTaskData' c (k 0) cmds
      packQuery c QueryAllTasks k cmds = packageTaskData' c (k ([])) cmds
      packQuery c (QueryTask _) k cmds = packageTaskData' c (k (0,0,0,0,[])) cmds

      packageTaskData' :: ArduinoConnection -> Arduino a -> B.ByteString -> B.ByteString
      packageTaskData' c (Bind m k) cmds = packBind c m k cmds
      packageTaskData' c (Return a) cmds = cmds
      packageTaskData' c cmd        cmds = packBind c cmd Return cmds

packageQuery :: Query a -> B.ByteString
packageQuery QueryFirmware            = sysEx    REPORT_FIRMWARE         []
packageQuery CapabilityQuery          = sysEx    CAPABILITY_QUERY        []
packageQuery AnalogMappingQuery       = sysEx    ANALOG_MAPPING_QUERY    []
-- TBD - Does Pulse still exist in Firmata?
packageQuery (Pulse p b dur to)       = sysEx    PULSE                   ([fromIntegral (pinNo p), if b then 1 else 0] ++ concatMap toArduinoBytes (word32ToBytes dur ++ word32ToBytes to))
packageQuery (I2CRead m sa sr)        = sysEx    I2C_REQUEST  (packageI2c m False sa sr)
packageQuery QueryAllTasks            = sysEx    SCHEDULER_DATA [schedulerCmdVal QUERY_ALL_TASKS]
packageQuery (QueryTask tid)          = sysEx    SCHEDULER_DATA [schedulerCmdVal QUERY_TASK, tid]

packageI2c :: I2CAddrMode -> Bool -> SlaveAddress -> Maybe SlaveRegister -> [Word8]
packageI2c m w sa sr = [addrBytes !! 0, commandByte] ++ slaveBytes
  where
    addrBytes = word16ToArduinoBytes sa
    commandByte = (firmataI2CModeVal m) .&. writeByte .&. (addrBytes !! 1)
    writeByte = if w then 0x00 else 0x08
    slaveBytes = case sr of 
                    Nothing -> []
                    Just r  -> word16ToArduinoBytes r

-- | Unpackage a SysEx response
unpackageSysEx :: [Word8] -> Response
unpackageSysEx []              = Unimplemented (Just "<EMPTY-SYSEX-CMD>") []
unpackageSysEx (cmdWord:args)
  | Right cmd <- getSysExCommand cmdWord
  = case (cmd, args) of
      (REPORT_FIRMWARE, majV : minV : rest)  -> Firmware majV minV (getString rest)
      (CAPABILITY_RESPONSE, bs)              -> Capabilities (getCapabilities bs)
      (ANALOG_MAPPING_RESPONSE, bs)          -> AnalogMapping bs
      (PULSE, xs) | length xs == 10          -> let [p, a, b, c, d] = fromArduinoBytes xs in PulseResponse (InternalPin p) (bytesToWord32 (a, b, c, d))
      (STRING_DATA, rest)                    -> StringMessage (getString rest)
      (I2C_REPLY, xs)                        -> let (sa:sr:idata) = arduinoBytesToWords16 xs in I2CReply sa sr idata
      (SCHEDULER_DATA, srWord : ts) | Right sr <- getSchedulerReply srWord
        -> case sr of 
          QUERY_ALL_TASKS_REPLY              -> QueryAllTasksReply ts
          QUERY_TASK_REPLY | length ts == 1  -> QueryTaskReply (ts !! 0) 0 0 0 []
          -- TBD Fix reply decode
          QUERY_TASK_REPLY | length ts >= 11 -> let tt0:tt1:tt2:tt3:tl0:tl1:tp0:tp1:td = arduinoDecoded (tail ts)
                                                in QueryTaskReply (head ts) (bytesToWord32 (tt3,tt2,tt1,tt0)) (bytesToWord16 (tl1,tl0)) (bytesToWord16 (tp1,tp0)) td
    -- TBD add other scheduler responses
      _                                      -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries TBD need to handle mismatches
parseQueryResult :: Query a -> Response -> a
parseQueryResult QueryFirmware (Firmware wa wb s) = (wa,wb,s)
parseQueryResult CapabilityQuery (Capabilities bc) = bc
parseQueryResult AnalogMappingQuery (AnalogMapping ms) = ms
parseQueryResult (Pulse p b dur to) (PulseResponse p2 w) = w
parseQueryResult (I2CRead am saq srq) (I2CReply sar srr ds) = ds
parseQueryResult QueryAllTasks (QueryAllTasksReply ts) = ts
parseQueryResult (QueryTask tid) (QueryTaskReply tid' tt tl tp ws) = (tid',tt,tl,tp,ws)

getCapabilities :: [Word8] -> BoardCapabilities
getCapabilities bs = BoardCapabilities $ M.fromList $ zipWith (\p c -> (p, PinCapabilities{analogPinNumber = Nothing, allowedModes = c}))
                                                              (map InternalPin [(0::Word8)..]) (map pinCaps (chunk bs))
  where chunk xs = case break (== 0x7f) xs of
                     ([], [])         -> []
                     (cur, 0x7f:rest) -> cur : chunk rest
                     _                -> [xs]
        pinCaps (x:y:rest) = (toEnum (fromIntegral x), y) : pinCaps rest
        pinCaps _          = []

-- | Unpackage a Non-SysEx response
unpackageNonSysEx :: (Int -> IO [Word8]) -> FirmataCmd -> IO Response
unpackageNonSysEx getBytes c = grab c
 where unimplemented n = Unimplemented (Just (show c)) `fmap` getBytes n
       grab (ANALOG_MESSAGE       p)    = getBytes 2 >>= \[l, h] -> return (AnalogMessage  p l h)
       grab (DIGITAL_MESSAGE      p)    = getBytes 2 >>= \[l, h] -> return (DigitalMessage p l h)
       -- we should never see any of the following since they are "request" codes
       grab (REPORT_ANALOG_PIN   _pin)  = unimplemented 1
       grab (REPORT_DIGITAL_PORT _port) = unimplemented 1
       grab START_SYSEX                 = unimplemented 0
       grab SET_PIN_MODE                = unimplemented 2
       grab END_SYSEX                   = unimplemented 0
       grab PROTOCOL_VERSION            = unimplemented 2
       grab SYSTEM_RESET                = unimplemented 0
