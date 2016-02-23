-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Comm
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

module System.Hardware.Haskino.Comm where

import Control.Monad        (when, forever)
import Control.Concurrent   (Chan, MVar, ThreadId, newChan, newMVar, 
                             newEmptyMVar, putMVar, takeMVar, writeChan, 
                             readChan, forkIO, modifyMVar_, tryTakeMVar, 
                             killThread, threadDelay, withMVar)
import Control.Exception    (tryJust, AsyncException(UserInterrupt))
import Control.Monad.State  (liftIO)
import Control.Natural (nat, run, (#))
import Control.Remote.Monad
import Control.Remote.Monad.Packet.Weak as WP
import Control.Remote.Monad.Packet.Strong as SP
import Data.Bits            (testBit, (.&.), xor, shiftR)
import Data.List            (intercalate)
import Data.Maybe           (listToMaybe)
import Data.Word            (Word8)
import System.Hardware.Serialport (SerialPort)
import System.IO.Error      (tryIOError)
import System.Timeout       (timeout)


import qualified Data.ByteString            as B 
import Data.ByteString.Base16 (encode)
import qualified Data.Map                   as M (empty, mapWithKey, insert, 
                                                  assocs, lookup, member)
import qualified Data.Set                   as S (empty)
import qualified System.Hardware.Serialport as S (openSerial, closeSerial, 
                                                  defaultSerialSettings, 
                                                  CommSpeed(CS115200), commSpeed,
                                                  recv, send)

import System.Hardware.Haskino.Data
import System.Hardware.Haskino.Expr
import System.Hardware.Haskino.Utils
import System.Hardware.Haskino.Protocol
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
-- See "System.Hardware.Haskino.Examples.Blink" for a simple example.
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
          error $ "\n*** Haskino:ERROR: Missing Port\n*** Make sure your Arduino is connected to " ++ fp
        Right port -> do
          dc <- newChan
          tid <- setupListener port debugger dc
          liftIO $ putMVar listenerTid tid
          refIndex <- newEmptyMVar
          liftIO $ putMVar refIndex 0
          debugger $ "Started listener thread: " ++ show tid
          let initState = ArduinoConnection {
                             message       = debugger
                           , bailOut       = bailOut listenerTid
                           , port          = port
                           , firmwareID    = "Unknown"
                           , processor     = UNKNOWN_PROCESSOR
                           , deviceChannel = dc
                           , listenerTid   = listenerTid
                           , refIndex      = refIndex
                        }
          -- Step 0: Delay for 1 second after opeing serial port to allow Mega
          --    to funciton correctly, as opening the serial port while 
          --    connected to a Mac or Linux machine with these boards causes
          --    them to reset. (Bootloader needs 1/2 sec itself).
          liftIO $ threadDelay (1000 * 1000);
          -- Step 1: Send a reset to get things going (Especially for non Mega)
          send initState systemReset
          -- Delay after the reset for 1 second
          liftIO $ threadDelay (1000 * 1000);
          -- Step 2: Send query-firmware, and wait until we get a response
          ver <- send initState queryFirmware
          let maj = ver `shiftR` 8
              min = ver .&. 0xFF
              id = "Firmware v" ++ show maj ++ "." ++ show min
              versionState = initState {firmwareID = id}
          putStrLn id
          -- Step 3: Send a processor type request
          p <- send versionState queryProcessor
          -- Update the connection state with the processor
          let openState = versionState {processor = p}
              proc = "Processor: " ++ show p
          putStrLn proc
          return openState
  where
      bailOut tid m ms = do cleanUpArduino tid
                            error $ "\n*** Haskino:ERROR: " ++ intercalate "\n*** " (m:ms)

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

-- | Bailing out: print the given string on stdout and die
runDie :: ArduinoConnection -> String -> [String] -> IO a
runDie c m ms = do 
    let f = bailOut c
    f m ms

withArduino :: Bool       -- ^ If 'True', debugging info will be printed
            -> FilePath   -- ^ Path to the USB port
            -> Arduino () -- ^ The Haskell controller program to run
            -> IO ()
withArduino verbose fp program = do 
      conn <- openArduino verbose fp
      res <- tryJust catchCtrlC $ send conn program
      case res of
        Left () -> putStrLn "Haskino: Caught Ctrl-C, quitting.."
        _       -> return ()
      closeArduino conn  
  where catchCtrlC UserInterrupt = Just ()
        catchCtrlC _             = Nothing

{-
send :: ArduinoConnection -> Arduino a -> IO a
send conn commands =
      send' conn commands B.empty
  where
      sendBind :: ArduinoConnection -> Arduino a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendBind c (Return a)      k cmds = send' c (k a) cmds
      sendBind c (Bind m k1)    k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
      sendBind c (Command (CreateTaskE tid as)) k cmds = do
          pc <- packageCommandIndex c (CreateTaskE tid as) 
          send' c (k ()) (B.append cmds pc)
      sendBind c (Command cmd) k cmds = do
          pc <- packageCommandIndex c cmd 
          checkPackageLength c pc
          send' c (k ()) (B.append cmds (framePackage pc))
      sendBind c (Procedure procedure) k cmds = sendProcedure c procedure k cmds

      packageCommandIndex :: ArduinoConnection -> Command -> IO B.ByteString
      packageCommandIndex c cmd = do
          ix <- takeMVar (refIndex c)
          let (pc, ix') = packageCommand cmd ix 0
          putMVar (refIndex c) ix'
          return pc

      sendProcedure :: ArduinoConnection -> Procedure a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendProcedure c (Debug msg) k cmds = do
          message c msg
          send' c (k ()) cmds
      sendProcedure c (Die msg msgs) k cmds = do
          runDie c msg msgs
          send' c (k ()) cmds
      sendProcedure c (NewRemoteRefB r) k cmds = sendRemoteBinding c (NewRemoteRefB r) k cmds 
      sendProcedure c (NewRemoteRefW8 r) k cmds = sendRemoteBinding c (NewRemoteRefW8 r) k cmds 
      sendProcedure c (NewRemoteRefW16 r) k cmds = sendRemoteBinding c (NewRemoteRefW16 r) k cmds 
      sendProcedure c (NewRemoteRefW32 r) k cmds = sendRemoteBinding c (NewRemoteRefW32 r) k cmds 
      sendProcedure c (NewRemoteRefI8 r) k cmds = sendRemoteBinding c (NewRemoteRefI8 r) k cmds 
      sendProcedure c (NewRemoteRefI16 r) k cmds = sendRemoteBinding c (NewRemoteRefI16 r) k cmds 
      sendProcedure c (NewRemoteRefI32 r) k cmds = sendRemoteBinding c (NewRemoteRefI32 r) k cmds 
      sendProcedure c (NewRemoteRefL8 r) k cmds = sendRemoteBinding c (NewRemoteRefL8 r) k cmds 
      sendProcedure c (NewRemoteRefFloat r) k cmds = sendRemoteBinding c (NewRemoteRefFloat r) k cmds 
      sendProcedure c (LiftIO m) k cmds = do
          sendToArduino c cmds
          res <- m
          send' c (k res) B.empty
      sendProcedure c procedure k cmds = do
          let pc = packageProcedure procedure 0
          checkPackageLength c pc
          sendToArduino c (B.append cmds (framePackage pc))
          qr <- waitResponse c (procDelay procedure) (Procedure procedure)
          send' c (k qr) B.empty
        where
          procDelay :: Procedure a -> Int
          procDelay proc = 
            case proc of
              DelayMillis d           -> millisToMicros (fromIntegral d) + secsToMicros 2
              DelayMillisE (LitW32 d) -> millisToMicros (fromIntegral d) + secsToMicros 2
              BootTaskE _             -> secsToMicros 30
              _                       -> secsToMicros 5

      sendRemoteBinding :: ArduinoConnection -> Procedure a -> (a -> Arduino b) -> B.ByteString -> IO b
      sendRemoteBinding c b k cmds = do
          ix <- takeMVar (refIndex c)
          let prb = packageRemoteBinding b ix 0
          checkPackageLength c prb
          putMVar (refIndex c) (ix+1)
          sendToArduino c (B.append cmds (framePackage prb))
          qr <- waitResponse c (secsToMicros 5) (Procedure b)
          send' c (k qr) B.empty

      checkPackageLength :: ArduinoConnection -> B.ByteString -> IO ()
      checkPackageLength c p = if B.length p > (maxFirmwareSize - 2)
                               then runDie c ("Protocol Frame Too Large (" ++ (show $ B.length p) ++ " bytes)")
                                            ["Frame in Error starts with " ++ (show $ firmwareValCmd $ B.head p),
                                             "Common error is a control structure (while, ifThenElse, loopE)",
                                             "which exceeds frame limits is used outside of a task"]
                               else return ()

      waitResponse :: ArduinoConnection -> Int -> Arduino a -> IO a
      waitResponse c t procedure = do
        message c $ "Waiting for response"
        resp <- liftIO $ timeout t $ readChan $ deviceChannel c
        case resp of 
            Nothing -> runDie c "Haskino:ERROR: Response Timeout" 
                             [ "Make sure your Arduino is running Haskino Firmware"]
            Just FailedNewRef -> runDie c "Haskino:ERROR: Failed to Allocate Reference" 
                                  [ "Make sure MAX_REFS is set high enough in Firmware",
                                    "and that Arudino has sufficient RAM"]
            Just r -> do 
                case parseQueryResult procedure r of
                    -- Ignore responses that do not match expected response
                    -- and wait for the next response.
                    Nothing -> do message c $ "Unmatched response" ++ show r
                                  waitResponse c t procedure
                    Just qr -> return qr

      millisToMicros :: Int -> Int
      millisToMicros m = m * 1000

      secsToMicros :: Int -> Int
      secsToMicros s = s * 1000000

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
-}

sendWeak :: ArduinoConnection -> Arduino a -> IO a
sendWeak c (Arduino m) = (run $ runMonad $ nat (runWP c)) m

send :: ArduinoConnection -> Arduino a -> IO a
send c (Arduino m) = (run $ runMonad $ nat (runSP c)) m

runWP :: ArduinoConnection -> WeakPacket ArduinoCommand ArduinoProcedure a -> IO a 
runWP c pkt = 
  case pkt of
    WP.Command cmd -> sendCommand c cmd
    WP.Procedure p -> sendProcedure c p
  where
      sendCommand :: ArduinoConnection -> ArduinoCommand -> IO ()
      sendCommand c (Loop m) = forever $ send c m
      sendCommand c cmd = do
          pc <- packageCommandIndex c cmd 
          checkPackageLength c pc
          sendToArduino c (framePackage pc)

      sendProcedure :: ArduinoConnection -> ArduinoProcedure a -> IO a
      sendProcedure c (Debug msg) = message c msg
      sendProcedure c (Die msg msgs) = runDie c msg msgs
      sendProcedure c (NewRemoteRefB r) = sendRemoteBinding c (NewRemoteRefB r)
      sendProcedure c (NewRemoteRefW8 r) = sendRemoteBinding c (NewRemoteRefW8 r) 
      sendProcedure c (NewRemoteRefW16 r) = sendRemoteBinding c (NewRemoteRefW16 r) 
      sendProcedure c (NewRemoteRefW32 r) = sendRemoteBinding c (NewRemoteRefW32 r)
      sendProcedure c (NewRemoteRefI8 r) = sendRemoteBinding c (NewRemoteRefI8 r) 
      sendProcedure c (NewRemoteRefI16 r) = sendRemoteBinding c (NewRemoteRefI16 r) 
      sendProcedure c (NewRemoteRefI32 r) = sendRemoteBinding c (NewRemoteRefI32 r) 
      sendProcedure c (NewRemoteRefL8 r) = sendRemoteBinding c (NewRemoteRefL8 r) 
      sendProcedure c (NewRemoteRefFloat r) = sendRemoteBinding c (NewRemoteRefFloat r) 
      sendProcedure c (LiftIO m) = m
      sendProcedure c procedure = do
          let pc = packageProcedure procedure 0
          checkPackageLength c pc
          sendToArduino c (framePackage pc)
          qr <- waitResponse c (procDelay procedure) procedure
          return qr

      sendRemoteBinding :: ArduinoConnection -> ArduinoProcedure a -> IO a
      sendRemoteBinding c b = do
          ix <- takeMVar (refIndex c)
          let prb = packageRemoteBinding b ix 0
          checkPackageLength c prb
          putMVar (refIndex c) (ix+1)
          sendToArduino c (framePackage prb)
          qr <- waitResponse c (secsToMicros 5) b
          return qr

runSP :: ArduinoConnection -> StrongPacket ArduinoCommand ArduinoProcedure a -> IO a 
runSP c pkt = go c pkt B.empty
  where
      go :: ArduinoConnection -> StrongPacket ArduinoCommand ArduinoProcedure a -> B.ByteString -> IO a
      go c pkt cmds = 
        case pkt of
          SP.Command cmd k -> do
            cmds' <- sendCommand c cmd cmds
            go c k cmds'
          SP.Procedure p   -> sendProcedure c p cmds
          SP.Done          -> sendToArduino c cmds

      sendCommand :: ArduinoConnection -> ArduinoCommand -> B.ByteString -> IO B.ByteString
      sendCommand c (Loop m) cmds = do
          sendToArduino c cmds
          forever $ send c m
      sendCommand c cmd cmds= do
          pc <- packageCommandIndex c cmd 
          checkPackageLength c pc
          return $ B.append cmds (framePackage pc) 

      sendProcedure :: ArduinoConnection -> ArduinoProcedure a -> B.ByteString -> IO a
      sendProcedure c (Debug msg) cmds = do
          message c msg
          sendToArduino c cmds
      sendProcedure c (Die msg msgs) cmds = runDie c msg msgs
      sendProcedure c (NewRemoteRefB r) cmds = sendRemoteBinding c (NewRemoteRefB r) cmds
      sendProcedure c (NewRemoteRefW8 r) cmds  = sendRemoteBinding c (NewRemoteRefW8 r) cmds 
      sendProcedure c (NewRemoteRefW16 r) cmds = sendRemoteBinding c (NewRemoteRefW16 r) cmds
      sendProcedure c (NewRemoteRefW32 r) cmds = sendRemoteBinding c (NewRemoteRefW32 r) cmds
      sendProcedure c (NewRemoteRefI8 r) cmds = sendRemoteBinding c (NewRemoteRefI8 r) cmds
      sendProcedure c (NewRemoteRefI16 r) cmds = sendRemoteBinding c (NewRemoteRefI16 r) cmds
      sendProcedure c (NewRemoteRefI32 r) cmds = sendRemoteBinding c (NewRemoteRefI32 r) cmds
      sendProcedure c (NewRemoteRefL8 r) cmds = sendRemoteBinding c (NewRemoteRefL8 r) cmds 
      sendProcedure c (NewRemoteRefFloat r) cmds = sendRemoteBinding c (NewRemoteRefFloat r) cmds
      sendProcedure c (LiftIO m) cmds = do
          sendToArduino c cmds
          m
      sendProcedure c procedure cmds = do
          let pc = packageProcedure procedure 0
          checkPackageLength c pc
          sendToArduino c (B.append cmds (framePackage pc))
          qr <- waitResponse c (procDelay procedure) procedure
          return qr

      sendRemoteBinding :: ArduinoConnection -> ArduinoProcedure a -> B.ByteString -> IO a
      sendRemoteBinding c b cmds = do
          ix <- takeMVar (refIndex c)
          let prb = packageRemoteBinding b ix 0
          checkPackageLength c prb
          putMVar (refIndex c) (ix+1)
          sendToArduino c (B.append cmds (framePackage prb))
          qr <- waitResponse c (secsToMicros 5) b
          return qr

packageCommandIndex :: ArduinoConnection -> ArduinoCommand -> IO B.ByteString
packageCommandIndex c cmd = do
    ix <- takeMVar (refIndex c)
    let (pc, ix') = packageCommand cmd ix 0
    putMVar (refIndex c) ix'
    return pc

checkPackageLength :: ArduinoConnection -> B.ByteString -> IO ()
checkPackageLength c p = if B.length p > (maxFirmwareSize - 2)
                         then runDie c ("Protocol Frame Too Large (" ++ (show $ B.length p) ++ " bytes)")
                                      ["Frame in Error starts with " ++ (show $ firmwareValCmd $ B.head p),
                                       "Common error is a control structure (while, ifThenElse, loopE)",
                                       "which exceeds frame limits is used outside of a task"]
                         else return ()

sendToArduino :: ArduinoConnection -> B.ByteString -> IO ()
sendToArduino conn cmds = do
    when (lp /= 0) 
         (message conn $ "Sending: " ++ show (encode cmds))
    sent <- liftIO $ S.send (port conn) cmds
    when (sent /= lp)
         (message conn $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
  where
    lp = B.length cmds

waitResponse :: ArduinoConnection -> Int -> ArduinoProcedure a -> IO a
waitResponse c t procedure = do
  message c $ "Waiting for response"
  resp <- liftIO $ timeout t $ readChan $ deviceChannel c
  case resp of 
      Nothing -> runDie c "Haskino:ERROR: Response Timeout" 
                       [ "Make sure your Arduino is running Haskino Firmware"]
      Just FailedNewRef -> runDie c "Haskino:ERROR: Failed to Allocate Reference" 
                            [ "Make sure MAX_REFS is set high enough in Firmware",
                              "and that Arudino has sufficient RAM"]
      Just r -> do 
          case parseQueryResult procedure r of
              -- Ignore responses that do not match expected response
              -- and wait for the next response.
              Nothing -> do message c $ "Unmatched response" ++ show r
                            waitResponse c t procedure
              Just qr -> return qr

procDelay :: ArduinoProcedure a -> Int
procDelay proc = 
  case proc of
    DelayMillis d           -> millisToMicros (fromIntegral d) + secsToMicros 2
    DelayMillisE (LitW32 d) -> millisToMicros (fromIntegral d) + secsToMicros 2
    BootTaskE _             -> secsToMicros 30
    _                       -> secsToMicros 5

millisToMicros :: Int -> Int
millisToMicros m = m * 1000

secsToMicros :: Int -> Int
secsToMicros s = s * 1000000

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
            checkFrame fs = (last fs) == (foldl (+) 0 $ init fs)
            listener = do
                frame  <- collectFrame []
                resp <- case frame of 
                           [] -> return $ EmptyFrame
                           fs | not (checkFrame fs) -> return $ InvalidChecksumFrame fs
                           fs -> case getFirmwareReply $ head fs of
                                    Left  unknown  -> return $ Unimplemented (Just (show unknown)) []
                                    Right c        -> return $ unpackageResponse $ init fs
                case resp of
                  EmptyFrame             -> dbg $ "Ignoring empty received frame"
                  InvalidChecksumFrame{} -> dbg $ "Ignoring received frame with invalid checksum" ++ show resp
                  Unimplemented{}        -> dbg $ "Ignoring the received response: " ++ show resp
                  StringMessage{}        -> dbg $ "Received " ++ show resp
                  _                      -> do dbg $ "Received " ++ show resp
                                               writeChan chan resp
        S.recv serial maxFirmwareSize -- Clear serial port of any unneeded characters
        tid <- liftIO $ forkIO $ forever listener
        return tid
