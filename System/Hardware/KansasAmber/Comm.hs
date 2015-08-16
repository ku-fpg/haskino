-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.KansasAmber.Comm
--                Based on System.Hardware.Arduino.comm
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Basic serial communication routines
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module System.Hardware.KansasAmber.Comm where

import Control.Monad        (when, forever)
import Control.Concurrent   (Chan, MVar, ThreadId, newChan, newMVar, 
                             newEmptyMVar, putMVar, takeMVar, writeChan, 
                             readChan, forkIO, modifyMVar_, tryTakeMVar, 
                             killThread, threadDelay)
import Control.Exception    (tryJust, AsyncException(UserInterrupt))
import Control.Monad.State  (liftIO)
import Data.Bits            (testBit, (.&.), xor)
import Data.List            (intercalate)
import Data.Maybe           (listToMaybe)
import Data.Word            (Word8)
import System.Hardware.Serialport (SerialPort)
import System.IO.Error      (tryIOError)
import System.Timeout       (timeout)


import qualified Data.ByteString            as B 
import Data.ByteString.Base16 (encode)
import qualified Data.Map                   as M (empty, mapWithKey, insert, 
                                                  assocs, lookup)
import qualified Data.Set                   as S (empty)
import qualified System.Hardware.Serialport as S (openSerial, closeSerial, 
                                                  defaultSerialSettings, 
                                                  CommSpeed(CS115200), commSpeed,
                                                  recv, send)

import System.Hardware.KansasAmber.Data
import System.Hardware.KansasAmber.Utils
import System.Hardware.KansasAmber.Protocol

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
-- See "System.Hardware.KansasAmber.Examples.Blink" for a simple example.
openArduino :: Bool                 -- ^ If 'True', debugging info will be printed
            -> FilePath             -- ^ Path to the Serial port
            -> IO ArduinoConnection
openArduino verbose fp = do
      debugger <- mkDebugPrinter verbose
      debugger $ "Accessing arduino located at: " ++ show fp
      listenerTid <- newEmptyMVar
      portTry <- tryIOError (S.openSerial fp S.defaultSerialSettings{S.commSpeed = S.CS115200})
      case portTry of 
        Left e -> 
          error $ "\n*** KansasAmber:ERROR: Missing Port\n*** Make sure your Arduino is connected to " ++ fp
        Right port -> do
          dc <- newChan
          tid <- setupListener port debugger dc
          liftIO $ putMVar listenerTid tid
          debugger $ "Started listener thread: " ++ show tid
          let initState = ArduinoConnection {
                             message       = debugger
                           , bailOut       = bailOut listenerTid
                           , port          = port
                           , firmwareID    = "Unknown"
                           , processor     = UNKNOWN_PROCESSOR 255
                           , deviceChannel = dc
                           , listenerTid   = listenerTid
                        }
          -- Step 1: Send a reset to get things going
          -- send initState systemReset
          -- Step 2: Send query-firmware, and wait until we get a response
          S.send port (B.pack [0x73,0x10,0x73])
          (v1, v2) <- send initState queryFirmware
          let versionState = initState {firmwareID = "Firmware v" ++ show v1 ++ "." ++ show v2 }
          -- Step 3: Send a capabilities request
          p <- send versionState queryProcessor
          -- Update the connection state with the processor
          let openState = versionState {processor = p}
          return openState
  where
      bailOut tid m ms = do cleanUpArduino tid
                            error $ "\n*** KansasAmber:ERROR: " ++ intercalate "\n*** " (m:ms)

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
      res <- tryJust catchCtrlC $ send conn program
      case res of
        Left () -> putStrLn "KansasAmber: Caught Ctrl-C, quitting.."
        _       -> return ()
      closeArduino conn  
  where catchCtrlC UserInterrupt = Just ()
        catchCtrlC _             = Nothing

send :: ArduinoConnection -> Arduino a -> IO a
send conn commands =
      send' conn commands B.empty
  where
      sendBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendBind c (Return a)      k cmds = send' c (k a) cmds
      sendBind c (Bind m k1)    k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
      sendBind c (Command (DelayMillis d)) k cmds = do
          sendToArduino c cmds
          message c $ "Delaying: " ++ show d
          threadDelay ((fromIntegral d)*1000)
          send' c (k ()) B.empty
      sendBind c (Command (DelayMicros d)) k cmds = do
          sendToArduino c cmds
          message c $ "Delaying: " ++ show d
          threadDelay (fromIntegral d)
          send' c (k ()) B.empty
      sendBind c (Command (CreateTask tid as)) k cmds = do 
          packCmd <- packageCommand c (CreateTask tid as)
          send' c (k ()) (B.append cmds packCmd)
      sendBind c (Command cmd) k cmds = do 
          packCmd <- packageCommand c cmd
          send' c (k ()) (B.append cmds (framePackage packCmd))
      sendBind c (Control ctrl)  k cmds = sendControl c ctrl k cmds
      sendBind c (Local local)   k cmds = sendLocal c local k cmds
      sendBind c (Procedure procedure) k cmds = sendProcedure c procedure k cmds
      sendBind c (LiftIO m) k cmds = do 
          res <- m
          send' c (k res) cmds

      sendControl :: ArduinoConnection -> Control -> (a -> Arduino b) -> B.ByteString -> IO b
      sendControl c (Loop ps) k cmds = do
          sendToArduino c cmds
          send c ps
          sendControl c (Loop ps) k B.empty

      sendLocal :: ArduinoConnection -> Local a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendLocal c (Debug msg) k cmds = do
          message c msg
          send' c (k ()) cmds
      sendLocal c (Die msg msgs) k cmds = do
          runDie c msg msgs
          send' c (k ()) cmds

      sendProcedure :: ArduinoConnection -> Procedure a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendProcedure c procedure k cmds = do
          sendToArduino c (B.append cmds (framePackage $ packageProcedure procedure))
          wait c procedure k
        where
          wait :: ArduinoConnection -> Procedure a -> (a -> Arduino b) -> IO b
          wait c procedure k = do
            message c $ "Waiting for response"
            resp <- liftIO $ timeout 5000000 $ readChan $ deviceChannel c
            case resp of 
                Nothing -> runDie c "Response Timeout" 
                                 [ "Make sure your Arduino is running Amber Firmware"]
                Just r -> do 
                    let qres = parseQueryResult procedure r
                    case qres of
                        -- Ignore responses that do not match expected response
                        -- and wait for the next response.
                        Nothing -> do message c $ "Unmatched response" ++ show r
                                      wait c procedure k
                        Just qr -> send' c (k qr) B.empty

      send' :: ArduinoConnection -> Arduino a -> B.ByteString -> IO a
      send' c (Bind m k)            cmds = sendBind c m k cmds
      send' _ (Return a)            cmds = do
              sendToArduino conn cmds
              return a
      send' c cmd                   cmds = sendBind c cmd Return cmds

      sendToArduino :: ArduinoConnection -> B.ByteString -> IO ()
      sendToArduino conn cmds = do
          when (lp /= 0) 
               (message conn $ "Sending: " ++ show (encode cmds))
          sent <- liftIO $ S.send (port conn) cmds
          when (sent /= lp)
               (message conn $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
        where
          lp = B.length cmds

-- | Start a thread to listen to the board and populate the channel with incoming queries.
setupListener :: SerialPort -> (String -> IO ()) -> Chan Response -> IO ThreadId
setupListener serial dbg chan = do
        let getByte = do bs <- S.recv serial 1
                         case B.length bs of
                            0 -> getByte
                            1 -> return $ B.head bs 
            collectFrame sofar = do b <- getByte
                                    case b of
                                      0x7E -> return $ reverse sofar
                                      0x7D -> do e <- getByte
                                                 collectFrame ((xor e 0x20) : sofar)
                                      _    -> collectFrame (b : sofar)
            listener = do
                frame  <- collectFrame []
                resp <- case frame of 
                           [] -> return $ EmptyFrame
                           fs -> case getFirmwareReply $ head fs of
                                    Left  unknown  -> return $ Unimplemented (Just (show unknown)) []
                                    Right c        -> return $ unpackageResponse fs
                case resp of
                  EmptyFrame           -> dbg $ "Ignoring empty received frame"
                  Unimplemented{}      -> dbg $ "Ignoring the received response: " ++ show resp
                  StringMessage{}      -> dbg $ "Received " ++ show resp
                  _                    -> do dbg $ "Received " ++ show resp
                                             writeChan chan resp
        tid <- liftIO $ forkIO $ forever listener
        return tid
