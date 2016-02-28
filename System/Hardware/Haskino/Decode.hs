-------------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.Haskino.Utils
--                Based on System.Hardware.Arduino.Utils
-- Copyright   :  (c) University of Kansas
--                System.Hardware.Arduino (c) Levent Erkok
-- License     :  BSD3
-- Stability   :  experimental
--
-- Internal utilities
-------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module System.Hardware.Haskino.Decode where

import           Data.Bits
import qualified Data.ByteString  as B 
import           Data.ByteString.Base16 (encode)
import           Data.Foldable (foldMap)
import           Data.Word
import           System.Hardware.Haskino.Data
import           System.Hardware.Haskino.Expr

infixr 5 :<

pattern b :< bs <- (B.uncons -> Just (b, bs))
pattern Empty   <- (B.uncons -> Nothing)

deframe :: B.ByteString -> [B.ByteString]
deframe bs = map unescape (deframe' bs [])
  where
    notFrameChar :: Word8 -> Bool
    notFrameChar b = b /= 0x7e

    deframe' :: B.ByteString -> [B.ByteString] -> [B.ByteString]
    deframe' Empty        xs = xs
    deframe' bs           xs = deframe' (tailFrame bs) (xs ++ [headFrame bs])

    headFrame :: B.ByteString -> B.ByteString  
    headFrame bs = B.init $ B.takeWhile notFrameChar bs

    tailFrame :: B.ByteString -> B.ByteString  
    tailFrame bs = B.tail $ B.dropWhile notFrameChar bs

    unescape :: B.ByteString -> B.ByteString
    unescape Empty        = B.empty
    unescape (x :< Empty) = B.singleton x
    unescape (x :< y :< xs)
      | x == 0x7d && y == 0x5d =  B.cons 0x7d (unescape xs)
      | x == 0x7d && y == 0x5e =  B.cons 0x7e (unescape xs)
      | otherwise              =  B.cons x (unescape (B.cons y xs))

decodeFrame :: B.ByteString -> String
decodeFrame bs = decodeCmds $ deframe bs

decodeCmds :: [B.ByteString] -> String
decodeCmds cs = concat $ map decodeCmd cs

decodeCmd :: B.ByteString -> String
decodeCmd Empty        = "EmptyCommand"
decodeCmd (x :< Empty) = show (firmwareValCmd x)
decodeCmd (x :< xs)    = show (firmwareValCmd x) ++ "[" ++ show (encode xs) ++ "]\n"

decodeCmdArgs :: FirmwareCmd -> B.ByteString -> (String, B.ByteString)
decodeCmdArgs BC_CMD_SYSTEM_RESET xs = ("", xs)
decodeCmdArgs BC_CMD_SET_PIN_MODE xs = decodeExprCmd 1 xs
decodeCmdArgs BC_CMD_DELAY_MILLIS xs = decodeExprCmd 1 xs
decodeCmdArgs BC_CMD_DELAY_MICROS xs = decodeExprCmd 1 xs
decodeCmdArgs BC_CMD_LOOP xs = ("", xs) -- TBD
decodeCmdArgs BC_CMD_WHILE xs = ("", xs) -- TBD
decodeCmdArgs BC_CMD_IF_THEN_ELSE xs = ("", xs) -- TBD
decodeCmdArgs BC_CMD_FORIN xs = ("", xs) -- TBD
decodeCmdArgs BS_CMD_REQUEST_VERSION xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_TYPE xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_MICROS xs = decodeExprProc 0 xs
decodeCmdArgs BS_CMD_REQUEST_MILLIS xs = decodeExprProc 0 xs
decodeCmdArgs DIG_CMD_READ_PIN xs = decodeExprProc 1 xs
decodeCmdArgs DIG_CMD_WRITE_PIN xs = decodeExprCmd 2 xs
decodeCmdArgs DIG_CMD_READ_PORT xs = decodeExprProc 2 xs
decodeCmdArgs DIG_CMD_WRITE_PORT xs = decodeExprCmd 3 xs
decodeCmdArgs ALG_CMD_READ_PIN xs = decodeExprProc 1 xs
decodeCmdArgs ALG_CMD_WRITE_PIN xs = decodeExprCmd 2 xs
decodeCmdArgs ALG_CMD_TONE_PIN xs = decodeExprCmd 3 xs
decodeCmdArgs ALG_CMD_NOTONE_PIN xs = decodeExprCmd 1 xs
decodeCmdArgs I2C_CMD_CONFIG xs = decodeExprCmd 0 xs
decodeCmdArgs I2C_CMD_READ xs = decodeExprProc 2 xs
decodeCmdArgs I2C_CMD_WRITE xs = decodeExprCmd 2 xs
decodeCmdArgs STEP_CMD_2PIN xs = decodeExprCmd 3 xs
decodeCmdArgs STEP_CMD_4PIN xs = decodeExprCmd 5 xs
decodeCmdArgs STEP_CMD_SET_SPEED xs = decodeExprCmd 2 xs
decodeCmdArgs STEP_CMD_STEP xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_ATTACH xs = decodeExprCmd 3 xs
decodeCmdArgs SRVO_CMD_DETACH xs = decodeExprCmd 1 xs
decodeCmdArgs SRVO_CMD_WRITE xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_WRITE_MICROS xs = decodeExprCmd 2 xs
decodeCmdArgs SRVO_CMD_READ xs = decodeExprProc 1 xs
decodeCmdArgs SRVO_CMD_READ_MICROS xs = decodeExprProc 1 xs
decodeCmdArgs SCHED_CMD_CREATE_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs SCHED_CMD_DELETE_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs SCHED_CMD_ADD_TO_TASK xs = ("", xs)
decodeCmdArgs SCHED_CMD_SCHED_TASK xs = decodeExprCmd 2 xs
decodeCmdArgs SCHED_CMD_QUERY_ALL xs = decodeExprProc 0 xs
decodeCmdArgs SCHED_CMD_QUERY xs = decodeExprProc 1 xs
decodeCmdArgs SCHED_CMD_RESET xs =  decodeExprCmd 0 xs
decodeCmdArgs SCHED_CMD_BOOT_TASK xs = decodeExprCmd 1 xs
decodeCmdArgs REF_CMD_NEW xs = ("", xs) -- TBD
decodeCmdArgs REF_CMD_READ xs =  ("", xs) -- TBD
decodeCmdArgs REF_CMD_WRITE xs = ("", xs) -- TBD
decodeCmdArgs UNKNOWN_COMMAND xs = ("Error-Unknown Command", xs)

decodeExprCmd :: Int -> B.ByteString -> (String, B.ByteString)
decodeExprCmd cnt bs = ("", bs)

decodeExprProc :: Int -> B.ByteString -> (String, B.ByteString)
decodeExprProc cnt bs = ("Bind " ++ show b ++ "<-", bs')
  where
    b = B.head bs
    (c, bs') = decodeExprCmd cnt (B.tail bs)

byteToTypeOp :: Word8 -> (Either ExprType ExprExtType, Int)
byteToTypeOp b = if (byteTypeNum b) < 7
                 then (Left $ toEnum $ fromIntegral $ byteTypeNum b, 
                       fromIntegral $ byteOpNum b)
                 else (Right $ exprExtValType $ byteExtTypeNum b, 
                       fromIntegral $ byteExtOpNum b)
  where 
    byteTypeNum :: Word8 -> Word8
    byteTypeNum b = (b .&. 0xE0) `shiftL` 5

    byteExtTypeNum :: Word8 -> Word8
    byteExtTypeNum b = (b .&. 0xF0) `shiftL` 4

    byteOpNum :: Word8 -> Word8
    byteOpNum b = b .&. 0x1F

    byteExtOpNum :: Word8 -> Word8
    byteExtOpNum b = b .&. 0x1F


