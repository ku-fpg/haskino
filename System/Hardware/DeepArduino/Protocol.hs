-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Arduino.Protocol
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Internal representation of the firmata protocol.
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs      #-}

module System.Hardware.DeepArduino.Protocol(packageProcedure, packageQuery, unpackageSysEx, unpackageNonSysEx, parseQueryResult) where

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

-- | Construct a non sys-ex message
nonSysEx :: FirmataCmd -> [Word8] -> B.ByteString
nonSysEx cmd bs = B.pack $ firmataCmdVal cmd : bs

-- | Package a request as a sequence of bytes to be sent to the board
-- using the Firmata protocol.
packageProcedure :: Procedure -> B.ByteString
packageProcedure SystemReset              = nonSysEx SYSTEM_RESET            []
packageProcedure (AnalogReport  p b)      = nonSysEx (REPORT_ANALOG_PIN p)   [if b then 1 else 0]
packageProcedure (DigitalReport p b)      = nonSysEx (REPORT_DIGITAL_PORT p) [if b then 1 else 0]
packageProcedure (SetPinMode p m)         = nonSysEx SET_PIN_MODE            [fromIntegral (pinNo p), fromIntegral (fromEnum m)]
packageProcedure (DigitalPortWrite p l m) = nonSysEx (DIGITAL_MESSAGE p)     [l, m]
packageProcedure (AnalogPinWrite p l m)   = nonSysEx (ANALOG_MESSAGE p)      [l, m]
-- packageProcedure (AnalogPinExtendedWrite p w8s) = sysEx EXTENDED_ANALOG      [fromIntegral (pinNo p)] ++ w8s
packageProcedure (SamplingInterval l m)   = sysEx    SAMPLING_INTERVAL       [l, m]
-- package (I2CWrite m sa w16s)     = sysEx    I2C_REQUEST

-- TBD need to add new queries
packageQuery :: Query a -> B.ByteString
packageQuery QueryFirmware            = sysEx    REPORT_FIRMWARE         []
packageQuery CapabilityQuery          = sysEx    CAPABILITY_QUERY        []
packageQuery AnalogMappingQuery       = sysEx    ANALOG_MAPPING_QUERY    []
packageQuery (Pulse p b dur to)       = sysEx    PULSE                   ([fromIntegral (pinNo p), if b then 1 else 0] ++ concatMap toArduinoBytes (word2Bytes dur ++ word2Bytes to))

-- | Unpackage a SysEx response
unpackageSysEx :: [Word8] -> Response
unpackageSysEx []              = Unimplemented (Just "<EMPTY-SYSEX-CMD>") []
unpackageSysEx (cmdWord:args)
  | Right cmd <- getSysExCommand cmdWord
  = case (cmd, args) of
      (REPORT_FIRMWARE, majV : minV : rest) -> Firmware majV minV (getString rest)
      (CAPABILITY_RESPONSE, bs)             -> Capabilities (getCapabilities bs)
      (ANALOG_MAPPING_RESPONSE, bs)         -> AnalogMapping bs
      (PULSE, xs) | length xs == 10         -> let [p, a, b, c, d] = fromArduinoBytes xs in PulseResponse (InternalPin p) (bytes2Words (a, b, c, d))
      _                                     -> Unimplemented (Just (show cmd)) args
  | True
  = Unimplemented Nothing (cmdWord : args)

-- This is how we match responses with queries TBD need to handle mismatches
-- TBD need to add new queries
parseQueryResult :: Query a -> Response -> a
parseQueryResult QueryFirmware (Firmware wa wb s) = (wa,wb,s)
parseQueryResult CapabilityQuery (Capabilities bc) = bc
parseQueryResult AnalogMappingQuery (AnalogMapping ms) = ms
parseQueryResult (Pulse p b dur to) (PulseResponse p2 w) = w

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
       -- TBD: Maybe we should put them in a different data-type
       grab (REPORT_ANALOG_PIN   _pin)  = unimplemented 1
       grab (REPORT_DIGITAL_PORT _port) = unimplemented 1
       grab START_SYSEX                 = unimplemented 0
       grab SET_PIN_MODE                = unimplemented 2
       grab END_SYSEX                   = unimplemented 0
       grab PROTOCOL_VERSION            = unimplemented 2
       grab SYSTEM_RESET                = unimplemented 0
