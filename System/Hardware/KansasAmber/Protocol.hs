-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs      #-}

module System.Hardware.KansasAmber.Protocol(framePackage, packageCommand, packageProcedure, unpackageResponse, parseQueryResult) where

import Data.Bits            (xor)
import Data.Word (Word8)

import qualified Data.ByteString as B

import System.Hardware.KansasAmber.Data
import System.Hardware.KansasAmber.Utils

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
packageCommand c (DigitalWrite p b)  = do
    return $ buildCommand DIG_CMD_WRITE_PIN [p, if b then 1 else 0]
packageCommand c (AnalogWrite p w) = do
    return $ buildCommand ALG_CMD_WRITE_PIN (p : (word16ToBytes w))
packageCommand c (Tone p f (Just d)) = do
    return $ buildCommand ALG_CMD_TONE_PIN (p : (word16ToBytes f) ++ (word32ToBytes d))
packageCommand c (Tone p f Nothing) = do
    packageCommand c (Tone p f (Just 0))
packageCommand c (NoTone p) = do
    return $ buildCommand ALG_CMD_NOTONE_PIN [p]
packageCommand c (I2CWrite sa w8s) = 
    return $ buildCommand I2C_CMD_WRITE (sa : w8s)
packageCommand c I2CConfig = 
    return $ buildCommand I2C_CMD_CONFIG []
packageCommand c (DeleteTask tid) = 
    return $ buildCommand SCHED_CMD_DELETE_TASK [tid]
packageCommand c (DelayMillis ms) = 
    return $ buildCommand BC_CMD_DELAY_MILLIS (word32ToBytes ms)
packageCommand c (DelayMicros ms) = 
    return $ buildCommand BC_CMD_DELAY_MICROS (word32ToBytes ms)
packageCommand c (ScheduleTask tid tt)    = 
    return $ buildCommand SCHED_CMD_SCHED_TASK (tid : word32ToBytes tt)
packageCommand c (CreateTask tid m)       = do
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
packageProcedure QueryFirmware       = 
    buildCommand BS_CMD_REQUEST_VERSION []
packageProcedure QueryProcessor      = 
    buildCommand BS_CMD_REQUEST_TYPE []
packageProcedure (DigitalRead p)     = 
    buildCommand DIG_CMD_READ_PIN [p]
packageProcedure (AnalogRead p)      = 
    buildCommand ALG_CMD_READ_PIN [p]
packageProcedure (I2CRead sa cnt)    = 
    buildCommand I2C_CMD_READ [sa,cnt]
packageProcedure QueryAllTasks       = 
    buildCommand SCHED_CMD_QUERY_ALL []
packageProcedure (QueryTask tid)     = 
    buildCommand SCHED_CMD_QUERY [tid]

-- | Unpackage a SysEx response
unpackageResponse :: [Word8] -> Response
unpackageResponse [] = Unimplemented (Just "<EMPTY-REPLY>") []
unpackageResponse (cmdWord:args)
  | Right cmd <- getFirmwareReply cmdWord
  = case (cmd, args) of
      (BS_RESP_VERSION, [majV, minV]) -> Firmware majV minV
      (BS_RESP_TYPE, [p])             -> ProcessorType p
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
      _                               -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries
parseQueryResult :: Procedure a -> Response -> Maybe a
parseQueryResult QueryFirmware (Firmware wa wb) = Just (wa,wb)
parseQueryResult QueryProcessor (ProcessorType pt) = Just $ getProcessor pt
parseQueryResult (DigitalRead p) (DigitalReply d) = Just (if d == 0 then False else True)
parseQueryResult (AnalogRead p) (AnalogReply a) = Just a
parseQueryResult (I2CRead saq cnt) (I2CReply ds) = Just ds
parseQueryResult QueryAllTasks (QueryAllTasksReply ts) = Just ts
parseQueryResult (QueryTask tid) (QueryTaskReply tr) = Just tr
parseQueryResult q r = Nothing
