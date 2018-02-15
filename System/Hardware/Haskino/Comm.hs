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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hardware.Haskino.Comm where

import           Control.Concurrent                (Chan, MVar, ThreadId,
                                                    newChan, newEmptyMVar,
                                                    putMVar, takeMVar,
                                                    writeChan, readChan,
                                                    forkIO, tryTakeMVar,
                                                    killThread, threadDelay)
import           Control.Exception                 (tryJust, AsyncException(UserInterrupt))
import           Control.Monad                     (when, forever)
import           Control.Monad.State               (runState)
import           Control.Monad.State               (liftIO)
import           Control.Natural                   (wrapNT,unwrapNT)
import           Control.Remote.Monad
import           Control.Remote.Packet.Weak        as WP
import           Control.Remote.Packet.Applicative as AP
import           Data.Bits                         ((.&.), xor, shiftR)
import qualified Data.ByteString                   as B
import           Data.List                         (intercalate)
import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Decode
import           System.Hardware.Haskino.Expr
import           System.Hardware.Haskino.Protocol
import           System.Hardware.Haskino.Utils
import           System.Hardware.Serialport        (SerialPort)
import qualified System.Hardware.Serialport        as S (openSerial, closeSerial,
                                                         defaultSerialSettings,
                                                         CommSpeed(CS115200),
                                                         commSpeed,
                                                         recv, send)
import           System.IO.Error                   (tryIOError)
import           System.Timeout                    (timeout)


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
        Left _ ->
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
                           , processor     = fromIntegral $ fromEnum UNKNOWN_PROCESSOR
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
              min' = ver .&. 0xFF
              id' = "Firmware v" ++ show maj ++ "." ++ show min'
              versionState = initState {firmwareID = id'}
          if maj == 0 && min' >= 6 then return () else error $ "\n*** Haskino:ERROR: Firmware version 0.6 or greater required"
          putStrLn id'
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
            -> Arduino a  -- ^ The Haskell controller program to run
            -> IO ()
withArduino = withArduinoApp

withArduinoWeak :: Bool       -- ^ If 'True', debugging info will be printed
                -> FilePath   -- ^ Path to the USB port
                -> Arduino a  -- ^ The Haskell controller program to run
                -> IO ()
withArduinoWeak = withArduinoMode sendWeak

withArduinoApp :: Bool       -- ^ If 'True', debugging info will be printed
               -> FilePath   -- ^ Path to the USB port
               -> Arduino a  -- ^ The Haskell controller program to run
               -> IO ()
withArduinoApp = withArduinoMode sendApp

withArduinoMode :: (ArduinoConnection -> Arduino a -> IO a) -- ^ Send function
                -> Bool       -- ^ If 'True', debugging info will be printed
                -> FilePath   -- ^ Path to the USB port
                -> Arduino a  -- ^ The Haskell controller program to run
                -> IO ()
withArduinoMode useSend verbose fp program = do
      conn <- openArduino verbose fp
      res <- tryJust catchCtrlC $ useSend conn program
      case res of
        Left () -> putStrLn "Haskino: Caught Ctrl-C, quitting.."
        _       -> return ()
      closeArduino conn
  where catchCtrlC UserInterrupt = Just ()
        catchCtrlC _             = Nothing

send :: ArduinoConnection -> Arduino a -> IO a
send = sendApp

sendWeak :: ArduinoConnection -> Arduino a -> IO a
sendWeak c (Arduino m) = (unwrapNT $ runMonad $ wrapNT (runWP c)) m

sendApp :: ArduinoConnection -> Arduino a -> IO a
sendApp c (Arduino m) = (unwrapNT $ runMonad $ wrapNT (runAP c)) m

runWP :: forall a . ArduinoConnection -> WeakPacket ArduinoPrimitive a -> IO a
runWP c pkt =
  case pkt of
    WP.Primitive p -> case knownResult p of
                        Just a -> do
                          frame <- frameCommand c p B.empty
                          sendToArduino c frame
                          return a
                        Nothing -> sendProcedureCmds c p B.empty

runAP :: forall a . ArduinoConnection -> ApplicativePacket ArduinoPrimitive a -> IO a
runAP c pkt =
  case knownResult pkt of
    Just a -> do
        cmds <- batchCommands c pkt B.empty
        sendToArduino c cmds
        return a
    Nothing -> case pkt of
                  AP.Primitive p -> case knownResult p of
                                      Just a -> do
                                        frame <- frameCommand c p B.empty
                                        sendToArduino c frame
                                        return a
                                      Nothing -> sendProcedureCmds c p B.empty
                  AP.Pure a      -> pure a
                  AP.Zip f g h   -> f <$> runAP c g <*> runAP c h
  where
    batchCommands :: forall a' . ArduinoConnection -> ApplicativePacket ArduinoPrimitive a' -> B.ByteString -> IO B.ByteString
    batchCommands c' pkt' cmds =
          case pkt' of
              AP.Primitive p -> case knownResult p of
                                  Just _ -> frameCommand c' p cmds
                                  Nothing -> return cmds
              AP.Pure _      -> return cmds
              AP.Zip _ g h   -> do
                  gcmds <- batchCommands c' g cmds
                  hcmds <- batchCommands c' h gcmds
                  return hcmds

frameCommand :: ArduinoConnection -> ArduinoPrimitive a -> B.ByteString -> IO B.ByteString
frameCommand c (Loop m) cmds = do
    sendToArduino c cmds
    forever $ send c m
frameCommand c (CreateTaskE tid as) cmds= do
    pc <- packageCommandIndex c (CreateTaskE tid as)
    return $ B.append cmds pc
frameCommand c cmd cmds= do
    pc <- packageCommandIndex c cmd
    checkPackageLength c pc
    return $ B.append cmds (framePackage pc)

sendProcedureCmds :: ArduinoConnection -> ArduinoPrimitive a -> B.ByteString -> IO a
sendProcedureCmds c (Debug msg) cmds = do
    message c $ bytesToString msg
    sendToArduino c cmds
sendProcedureCmds c (Die msg msgs) _ = runDie c msg msgs
sendProcedureCmds c (NewRemoteRefBE r) cmds = sendRemoteBindingCmds c (NewRemoteRefBE r) cmds
sendProcedureCmds c (NewRemoteRefW8E r) cmds  = sendRemoteBindingCmds c (NewRemoteRefW8E r) cmds
sendProcedureCmds c (NewRemoteRefW16E r) cmds = sendRemoteBindingCmds c (NewRemoteRefW16E r) cmds
sendProcedureCmds c (NewRemoteRefW32E r) cmds = sendRemoteBindingCmds c (NewRemoteRefW32E r) cmds
sendProcedureCmds c (NewRemoteRefI8E r) cmds = sendRemoteBindingCmds c (NewRemoteRefI8E r) cmds
sendProcedureCmds c (NewRemoteRefI16E r) cmds = sendRemoteBindingCmds c (NewRemoteRefI16E r) cmds
sendProcedureCmds c (NewRemoteRefI32E r) cmds = sendRemoteBindingCmds c (NewRemoteRefI32E r) cmds
sendProcedureCmds c (NewRemoteRefIE r) cmds = sendRemoteBindingCmds c (NewRemoteRefIE r) cmds
sendProcedureCmds c (NewRemoteRefL8E r) cmds = sendRemoteBindingCmds c (NewRemoteRefL8E r) cmds
sendProcedureCmds c (NewRemoteRefFloatE r) cmds = sendRemoteBindingCmds c (NewRemoteRefFloatE r) cmds
sendProcedureCmds c (LiftIO m) cmds = do
    sendToArduino c cmds
    m
sendProcedureCmds c procedure cmds = do
    let (pc, _) = runState (packageProcedure procedure) (CommandState 0 0 B.empty [] False [] [])
    checkPackageLength c pc
    sendToArduino c (B.append cmds (framePackage pc))
    qr <- waitResponse c (procDelay procedure) procedure
    return qr

sendRemoteBindingCmds :: ArduinoConnection -> ArduinoPrimitive a -> B.ByteString -> IO a
sendRemoteBindingCmds c b cmds = do
    ix <- takeMVar (refIndex c)
    let (prb, _) = runState (packageRemoteBinding b) (CommandState ix 0 B.empty [] False [] [])
    checkPackageLength c prb
    putMVar (refIndex c) (ix+1)
    sendToArduino c (B.append cmds (framePackage prb))
    qr <- waitResponse c (secsToMicros 5) b
    return qr

packageCommandIndex :: ArduinoConnection -> ArduinoPrimitive a -> IO B.ByteString
packageCommandIndex c cmd = do
    index <- takeMVar (refIndex c)
    let (pc, st) = runState (packageCommand cmd) (CommandState index 0 B.empty [] False [] [])
    putMVar (refIndex c) (ix st)
    return pc

checkPackageLength :: ArduinoConnection -> B.ByteString -> IO ()
checkPackageLength c p = if B.length p > (maxFirmwareSize - 2)
                         then runDie c ("Protocol Frame Too Large (" ++ (show $ B.length p) ++ " bytes)")
                                      ["Frame in Error starts with " ++ (show $ firmwareValCmd $ B.head p),
                                       "Common error is a control structure (iterateE or ifThenElseE)",
                                       "which exceeds frame limits is used outside of a task"]
                         else return ()

sendToArduino :: ArduinoConnection -> B.ByteString -> IO ()
sendToArduino conn cmds = do
    when (lp /= 0)
         -- (message conn $ "Sending: " ++ show (encode cmds))
         (message conn $ "Sending: \n" ++ (decodeFrame cmds))
    sent <- liftIO $ S.send (port conn) cmds
    when (sent /= lp)
         (message conn $ "Send failed. Tried: " ++ show lp ++ "bytes, reported: " ++ show sent)
  where
    lp = B.length cmds

waitResponse :: ArduinoConnection -> Int -> ArduinoPrimitive a -> IO a
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

procDelay :: ArduinoPrimitive a -> Int
procDelay proc =
  case proc of
    DelayMillis d           -> millisToMicros (fromIntegral d) + secsToMicros 2
    DelayMillisE (LitW32 d) -> millisToMicros (fromIntegral d) + secsToMicros 2
    DebugListen             -> 1000 * 1000 * 60 * 60 *24 -- Listen for a day
    BootTaskE _             -> secsToMicros 30
    IterateUnitUnitE _ _ _  -> secsToMicros 60
    IterateW8BoolE _ _ _    -> secsToMicros 60
    IterateW8W8E _ _ _      -> secsToMicros 60
    IterateW8W16E _ _ _     -> secsToMicros 60
    -- TBD add rest of Iterates
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
                            _ -> error "getByte - Invalid number of bytes"
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
                                    Right _        -> return $ unpackageResponse $ init fs
                case resp of
                  EmptyFrame             -> dbg $ "Ignoring empty received frame"
                  InvalidChecksumFrame{} -> dbg $ "Ignoring received frame with invalid checksum" ++ show resp
                  Unimplemented{}        -> dbg $ "Ignoring the received response: " ++ show resp
                  StringMessage{}        -> dbg $ "Received " ++ show resp
                  _                      -> do dbg $ "Received " ++ show resp
                                               writeChan chan resp
        _ <- S.recv serial maxFirmwareSize -- Clear serial port of any unneeded characters
        tid <- liftIO $ forkIO $ forever listener
        return tid
