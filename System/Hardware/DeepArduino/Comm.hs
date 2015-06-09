-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.DeepArduino.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino.comm (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Basic serial communication routines
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module System.Hardware.DeepArduino.Comm where

import Control.Monad        (when, forever)
import Control.Concurrent   (Chan, MVar, ThreadId, newChan, newMVar, newEmptyMVar, putMVar, takeMVar, writeChan, readChan, forkIO, modifyMVar_, tryTakeMVar, killThread, threadDelay)
import Control.Monad.State  (liftIO)
import Data.Bits            (testBit, (.&.))
import Data.List            (intercalate)
import Data.Maybe           (listToMaybe)
import System.Hardware.Serialport (SerialPort)

import qualified Data.ByteString            as B 
import Data.ByteString.Base16 (encode)
import qualified Data.Map                   as M (empty, mapWithKey, insert, assocs, lookup)
import qualified Data.Set                   as S (empty)
import qualified System.Hardware.Serialport as S (openSerial, closeSerial, defaultSerialSettings, CommSpeed(CS57600), commSpeed, recv, send)

import System.Hardware.DeepArduino.Data
import System.Hardware.DeepArduino.Utils
import System.Hardware.DeepArduino.Protocol


-- | Open the connection to control the board:
--
--    * The file path argument should point to the device file that is
--      associated with the board. ('COM1' on Windows,
--      '/dev/cu.usbmodemfd131' on Mac, etc.)
--
--    * The boolean argument controls verbosity. It should remain
--      'False' unless you have communication issues. The print-out
--      is typically less-than-useful, but it might point to the root
--      cause of the problem.
--
-- See "System.Hardware.DeepArduino.Examples.Blink" for a simple example.
openArduino :: Bool                 -- ^ If 'True', debugging info will be printed
            -> FilePath             -- ^ Path to the Serial port
            -> IO ArduinoConnection
openArduino verbose fp = do
        debugger <- mkDebugPrinter verbose
        debugger $ "Accessing arduino located at: " ++ show fp
        listenerTid <- newEmptyMVar
        port <- S.openSerial fp S.defaultSerialSettings{S.commSpeed = S.CS57600}
        let initBoardState = BoardState {
                                 boardCapabilities    = BoardCapabilities M.empty
                               , analogReportingPins  = S.empty
                               , digitalReportingPins = S.empty
                               , pinStates            = M.empty
                               , digitalWakeUpQueue   = []
                              }
        bs <- newMVar initBoardState
        dc <- newChan
        tid <- setupListener port debugger dc bs
        liftIO $ putMVar listenerTid tid
        debugger $ "Started listener thread: " ++ show tid
        let initState = ArduinoConnection {
                           message       = debugger
                         , bailOut       = bailOut listenerTid
                         , port          = port
                         , firmataID     = "Unknown"
                         , capabilities  = BoardCapabilities M.empty
                         , boardState    = bs
                         , deviceChannel = dc
                         , listenerTid   = listenerTid
                      }
        -- Step 1: Send a reset to get things going
        send initState systemReset
        -- Step 2: Send query-firmware, and wait until we get a response
        -- TBD Need to handle no response instance with Queries returning maybies
        (v1, v2, s) <- send initState queryFirmware
        let versionState = initState {firmataID = "Firmware v" ++ show v1 ++ "." ++ show v2 ++ "(" ++ s ++ ")"}
        -- Step 3: Send a capabilities request
        bc <- send versionState capabilityQuery
        -- Step 4: Send an analog mapping query
        am <- send versionState analogMappingQuery
        -- Use the board capabilities and the analog mapping to create new
        -- board capabilities
        let BoardCapabilities m = bc
        let newBc = BoardCapabilities (M.mapWithKey (mapAnalog am) m)
        -- Update the capabilities in the connection state
        let openState = versionState {capabilities = newBc}
        -- Update the capabilities in the board state, and put new 
        -- board state in the connetion state
        modifyMVar_ (boardState openState) $ \bst -> return initBoardState {boardCapabilities = newBc}
        return openState
    where
        bailOut tid m ms = do cleanUpArduino tid
                              error $ "\n*** DeepArduino:ERROR: " ++ intercalate "\n*** " (m:ms)
        mapAnalog as p c
            | i < rl && m /= 0x7f
            = c{analogPinNumber = Just m}
            | True             -- out-of-bounds, or not analog; ignore
            = c
          where rl = length as
                i  = fromIntegral (pinNo p)
                m  = as !! i

closeArduino :: ArduinoConnection -> IO ()
closeArduino conn = do
        cleanUpArduino $ listenerTid conn
        S.closeSerial $ port conn
        return ()

cleanUpArduino :: MVar ThreadId -> IO ()
cleanUpArduino tid = do mbltid <- tryTakeMVar tid
                        case mbltid of
                            Just t -> killThread t
                            _      -> return ()

withArduino :: Bool       -- ^ If 'True', debugging info will be printed
            -> FilePath   -- ^ Path to the USB port
            -> Arduino () -- ^ The Haskell controller program to run
            -> IO ()
withArduino verbose fp program = do 
        conn <- openArduino verbose fp
        send conn program
        closeArduino conn  

send :: ArduinoConnection -> Arduino a -> IO a
send conn commands =
      send' conn commands B.empty
  where
      sendBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendBind c (Return a)      k cmds = send' c (k a) cmds
      sendBind c (Bind m k1)    k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
      sendBind c (Procedure (Delay d)) k cmds = do
          sendToArduino c cmds
          message c $ "Delaying: " ++ show d
          threadDelay ((fromIntegral d)*1000)
          send' c (k ()) B.empty
      sendBind c (Procedure cmd) k cmds = send' c (k ()) (B.append cmds (packageProcedure c cmd))
      sendBind c (Local local)   k cmds = sendLocal c local k cmds
      sendBind c (Query query)   k cmds = sendQuery c query k cmds

      sendLocal :: ArduinoConnection -> Local a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendLocal c (AnalogPinRead p) k cmds = do
          sendToArduino c cmds
          a <- analogRead c p
          send' c (k (fromIntegral a)) B.empty
-- TBD need to finish Locals, complete digtial port read and add waits
      sendLocal c (DigitalPortRead p) k cmds = do
          sendToArduino c cmds
          send' c (k (digPortRead p)) B.empty
      sendLocal c (DigitalPinRead p) k cmds = do
          sendToArduino c cmds
          b <- digitalRead c p
          send' c (k b) B.empty

      sendQuery :: ArduinoConnection -> Query a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendQuery c query k cmds = do
          sendToArduino c (B.append cmds (packageQuery query))
          resp <- liftIO $ readChan (deviceChannel c)
          send' c (k (parseQueryResult query resp)) B.empty

      send' :: ArduinoConnection -> Arduino a -> B.ByteString -> IO a
      send' c (Bind m k)            cmds = sendBind c m k cmds
      send' _ (Return a)            cmds = do
              sendToArduino conn cmds
              return a
      send' c cmd                   cmds = sendBind c cmd Return cmds

      sendToArduino :: ArduinoConnection -> B.ByteString -> IO ()
      sendToArduino conn cmds = do
          message conn $ "Sending: " ++ show (encode cmds)
          sent <- liftIO $ S.send (port conn) cmds
          when (sent /= lp)
               (message conn $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
        where
          lp = B.length cmds

-- | Start a thread to listen to the board and populate the channel with incoming queries.
setupListener :: SerialPort -> (String -> IO ()) -> Chan Response -> MVar BoardState -> IO ThreadId
setupListener serial dbg chan bs = do
        let getBytes n = do let go need sofar
                                 | need <= 0  = return $ reverse sofar
                                 | True       = do b <- S.recv serial need
                                                   case B.length b of
                                                     0 -> go need sofar
                                                     l -> go (need - l) (b : sofar)
                            chunks <- go n []
                            return $ concatMap B.unpack chunks
            collectSysEx sofar = do [b] <- getBytes 1
                                    if b == firmataCmdVal END_SYSEX
                                       then return $ reverse sofar
                                       else collectSysEx (b : sofar)
            listener bs = do
                [cmd] <- getBytes 1
                resp  <- case getFirmataCmd cmd of
                           Left  unknown     -> return $ Unimplemented (Just (show unknown)) []
                           Right START_SYSEX -> unpackageSysEx `fmap` collectSysEx []
                           Right nonSysEx    -> unpackageNonSysEx getBytes nonSysEx
                case resp of
                  Unimplemented{}      -> dbg $ "Ignoring the received response: " ++ show resp
                  StringMessage{}      -> dbg $ "Received " ++ show resp
                  -- NB. When Firmata sends back AnalogMessage, it uses the number in A0-A1-A2, etc., i.e., 0-1-2; which we
                  -- need to properly interpret in our own pin mapping schema, where analogs come after digitals.
                  AnalogMessage mp l h -> modifyMVar_ bs $ \bst ->
                                           do let BoardCapabilities caps = boardCapabilities bst
                                                  mbP = listToMaybe [mappedPin | (mappedPin, PinCapabilities{analogPinNumber = Just mp'}) <- M.assocs caps, pinNo mp == mp']
                                              case mbP of
                                                Nothing -> return bst -- Mapping hasn't happened yet
                                                Just p  -> do
                                                   let v = (128 * fromIntegral (h .&. 0x07) + fromIntegral (l .&. 0x7f)) :: Int
                                                   case pinValue `fmap` (p `M.lookup` pinStates bst) of
                                                     Just (Just (Right v'))
                                                       | abs (v - v') < 10  -> return () -- be quiet, otherwise prints too much
                                                     _                      -> dbg $ "Updating analog pin " ++ show p ++ " values with " ++ showByteList [l,h] ++ " (" ++ show v ++ ")"
                                                   return bst{ pinStates = M.insert p PinData{pinMode = ANALOG, pinValue = Just (Right v)} (pinStates bst) }
                  DigitalMessage p l h -> do dbg $ "Updating digital port " ++ show p ++ " values with " ++ showByteList [l,h]
                                             modifyMVar_ bs $ \bst -> do
                                                  let upd o od | p /= pinPort o               = od   -- different port, no change
                                                               | pinMode od `notElem` [INPUT] = od   -- not an input pin, ignore
                                                               | True                         = od{pinValue = Just (Left newVal)}
                                                        where idx = pinPortIndex o
                                                              newVal | idx <= 6 = l `testBit` fromIntegral idx
                                                                     | True     = h `testBit` fromIntegral (idx - 7)
                                                  let wakeUpQ = digitalWakeUpQueue bst
                                                      bst' = bst{ pinStates          = M.mapWithKey upd (pinStates bst)
                                                                , digitalWakeUpQueue = []
                                                                }
                                                  mapM_ (`putMVar` ()) wakeUpQ
                                                  return bst'
                  _                    -> do dbg $ "Received " ++ show resp
                                             writeChan chan resp
        tid <- liftIO $ forkIO $ forever (listener bs)
        return tid
